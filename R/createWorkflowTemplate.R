testEnv <- function() {
  env <- new.env(parent = emptyenv())
  env$pwd<- function() {
    return(pwd())
  }
  return(env)
}




#' Create a workflow template environment from an existing workflow
#'
#' Extracts all steps, relationships, parameters, and files from the workflow
#' and builds a reusable workflow template environment.
#'
#' @param workflow The workflow environment to template. If NULL (default),
#'   creates an empty workflow template.
#' @param addParental If TRUE, the created stepTemplates have the steps from
#'   the workflow as parents (default: FALSE).
#' @returns An environment representing the workflow template with the following methods:
#'   \describe{
#'     \item{addStepTemplate(stepTemplate, name=NULL)}{Adds a step template to the workflow template.}
#'     \item{createExecutionPlan()}{Creates a data.frame describing the execution plan for the workflow.}
#'     \item{df()}{Returns a data.frame with metadata for all steps in the template.}
#'     \item{executePlan(executionPlan)}{Executes a given execution plan data.frame.}
#'     \item{listParameters()}{Returns a data frame of all defined parameters.}
#'     \item{parameterizeStep(paramName, stepPattern, property, target=NULL, required=TRUE, defaultValue=NULL)}{
#'       Registers a parameter to modify steps before realisation.
#'       \describe{
#'         \item{paramName}{Name of the parameter.}
#'         \item{stepPattern}{Pattern to match steps. Three matching modes are supported:
#'           \itemize{
#'             \item \code{"*"} matches all steps in the workflow template.
#'             \item Prefix pattern ending with \code{*} (e.g., \code{"analysisTree1/*"}) matches
#'               all steps whose fullName starts with the prefix.
#'             \item Exact match (e.g., \code{"import data"}) first tries to match the step's
#'               fullName, then falls back to matching the step's description.
#'           }
#'         }
#'         \item{property}{The property to parameterize: "remoteFile", "localFile", "treeIdent", "gridArgument.<name>", "description", "rationale", or "stepName".}
#'         \item{target}{(Optional) Target within the property, e.g., the path of a remote file.}
#'         \item{required}{(Optional) Whether this parameter must be set before realization (default: TRUE).}
#'         \item{defaultValue}{(Optional) Default value if not set.}
#'       }
#'     }
#'     \item{realise()}{Executes the plan, applying parameters and creating the workflow steps.}
#'     \item{setParameter(paramName, value)}{Sets a value for a defined parameter.}
#'     \item{setWorkflowTreeIdent(treeIdent, from=pwd())}{Sets the identifier for the workflow tree.}
#'     \item{setWorkflowTreeName(treeName)}{Sets the name for the workflow tree.}
#'     \item{setWorkflowTreeRootFolder(rootFolder)}{Sets the root folder path for the workflow tree.}
#'     \item{toJSON(filepath, pretty=TRUE)}{Saves the template definition to a JSON file.}
#'     \item{validateParameters()}{Checks if all required parameters are set, throwing an error if not.}
#'   }
#' @export
createWorkflowTemplateEnv <- function(workflow = NULL, addParental=F) {
  env <- new.env(parent = emptyenv())
  env$this <- env
  env$workflow <- workflow
  env$addParental <- addParental

  # Initialize parameter registry
  env$parameters <- new.env(parent = emptyenv())

  internalLinks <- NULL
  if (!is.null(workflow)) {
    internalLinks <- workflow$internalLinks
    if (!is.null(internalLinks) && nrow(internalLinks) > 0) {
      # Only select columns that exist in the data frame
      requiredCols <- c("entityId","fileHash","revisionId","path","targetStep","sourceStep","sourceInventoryPath","name")
      existingCols <- intersect(names(internalLinks), requiredCols)
      if (length(existingCols) > 0) {
        internalLinks <- dplyr::select(internalLinks, dplyr::all_of(existingCols))
      }
    }
  }


  # Extract steps and their templates
  stepTemplates <- list()
  if (!is.null(workflow)) {
    for (stepName in names(workflow$steps)) {
      stepEnv <- workflow$steps[[stepName]]
    # Convert each step to a template environment
    stepDf <- stepEnv$stepDf
    if (addParental) {
      stepDf$parentIdent<-stepDf$sourceEntityId
    }
    workflowLinks <- internalLinks[internalLinks$targetStep==stepName,]
    if (!is.null(workflowLinks) && nrow(workflowLinks)>0) {
      remoteFiles<- stepDf$remoteFiles[[1]]
      # Only join if remoteFiles exists (step might only have local files)
      if (!is.null(remoteFiles) && nrow(remoteFiles) > 0) {
        jointRemoteFiles <- dplyr::left_join(remoteFiles,workflowLinks,by=c("name"="name"))
        stepDf$remoteFiles<-list(jointRemoteFiles)
      }
      #case import:
      if (F) {


      if (!("sourceStep"%in%names(remoteFiles))) {
        # During import, entityId might not exist, need a different join strategy
        if ("entityId" %in% names(workflowLinks) && "ident" %in% names(remoteFiles)) {
          # Normal case: join by entityId
          jointRemoteFiles <- dplyr::left_join(remoteFiles,workflowLinks,by=c("ident"="entityId"))
        } else {
          # Import case: manually match based on targetStep + name combination
          # Since we already filtered workflowLinks by targetStep (line 49), we're only
          # matching within a single step's context. File names within a single step
          # should be unique (can't have two files with same path in one step).
          # This makes the name-based matching safe within this filtered context.
          jointRemoteFiles <- remoteFiles
          if ("name" %in% names(workflowLinks) && "name" %in% names(remoteFiles)) {
            for (i in seq_len(nrow(jointRemoteFiles))) {
              # Find matching link for this specific remote file
              # Clean the name to match (remove ./ prefix if present)
              cleanName <- jointRemoteFiles$name[i]
              if (startsWith(cleanName, "./")) {
                cleanName <- substr(cleanName, 3, nchar(cleanName))
              }
              matchingLinks <- workflowLinks[workflowLinks$name == cleanName | workflowLinks$name == jointRemoteFiles$name[i],]
              if (nrow(matchingLinks) > 0) {
                # Take the first match (they should all be the same for this targetStep+name combo)
                for (col in names(matchingLinks)) {
                  if (!(col %in% names(jointRemoteFiles))) {
                    jointRemoteFiles[[col]] <- NA
                  }
                  jointRemoteFiles[i, col] <- matchingLinks[1, col]
                }
              }
            }
          }
        }

        jointRemoteFiles <-byNotEmptyAsDf(jointRemoteFiles,function(remoteFile) {

          if ("sourceStep" %in% names(remoteFile) && !is.na(remoteFile$sourceStep)) {
            # If sourceInventoryPath is already present from the workflow links, use it
            if ("sourceInventoryPath" %in% names(remoteFile) && !is.na(remoteFile$sourceInventoryPath)) {
              # Already has sourceInventoryPath, nothing to do
            } else {
              # Try to construct sourceInventoryPath from the source file
              # First, we need to find the actual source file in the source step
              sourceStepEnv <- workflow$steps[[remoteFile$sourceStep]]
              if (!is.null(sourceStepEnv)) {
                # Get all files from the source step
                sourceStepFiles <- sourceStepEnv$stepDf$localFiles[[1]]
                sourceStepRemoteFiles <- sourceStepEnv$stepDf$remoteFiles[[1]]

                # Find the file that matches this link's entity ID
                matchingFile <- NULL
                if (!is.null(sourceStepRemoteFiles) && "ident" %in% names(sourceStepRemoteFiles)) {
                  matchingFile <- sourceStepRemoteFiles[sourceStepRemoteFiles$ident == remoteFile$ident,]
                }

                if (!is.null(matchingFile) && nrow(matchingFile) > 0) {
                  # Use the name from the matching file in the source step
                  sourceFileName <- matchingFile$name[1]
                  if (startsWith(sourceFileName, "./")) {
                    remoteFile$sourceInventoryPath <- sourceFileName
                  } else {
                    remoteFile$sourceInventoryPath <- paste0("./", sourceFileName)
                  }
                } else if (!is.null(sourceStepFiles) && "name" %in% names(sourceStepFiles)) {
                  # Try to match by name in local files
                  cleanName <- remoteFile$name
                  if (startsWith(cleanName, "./")) {
                    cleanName <- substr(cleanName, 3, nchar(cleanName))
                  }
                  matchingFile <- sourceStepFiles[sourceStepFiles$name == cleanName | sourceStepFiles$name == remoteFile$name,]
                  if (nrow(matchingFile) > 0) {
                    sourceFileName <- matchingFile$name[1]
                    if (startsWith(sourceFileName, "./")) {
                      remoteFile$sourceInventoryPath <- sourceFileName
                    } else {
                      remoteFile$sourceInventoryPath <- paste0("./", sourceFileName)
                    }
                  }
                }
              }
            }
          }
          if (!("name" %in% names(remoteFile)) || is.na(remoteFile$name)) {
            # Try to get name from resource, but handle case where it doesn't exist
            if (!is.null(remoteFile$ident) && !is.na(remoteFile$ident)) {
              resource <- loadResource(remoteFile$ident)
              if (!is.null(resource) && !is.null(resource$name)) {
                remoteFile$name<-paste0("./",resource$name)
              }
            }
          }
          return(remoteFile)
        })
        stepDf$remoteFiles<-list(jointRemoteFiles)
      }
}
    }
    stepTemplates[[stepName]] <- createStepTemplateEnv(
      stepDf = stepDf,
      workflow = env
    )
    stepTemplates[[stepName]]$dependencies <- rlang::env_clone(stepEnv$dependencies)
    rm(list = c("load"), pos = stepTemplates[[stepName]]$dependencies)
    stepTemplates[[stepName]]$usage <- rlang::env_clone(stepEnv$usage)
    rm(list=c("load"),pos=stepTemplates[[stepName]]$usage)
    stepTemplates[[stepName]]$parent <- rlang::env_clone(stepEnv$parent)
    rm(list=c("load"),pos=stepTemplates[[stepName]]$parent)
    stepTemplates[[stepName]]$children <- rlang::env_clone(stepEnv$children)
    rm(list=c("load"),pos=stepTemplates[[stepName]]$children)
    }
  }
  env$stepTemplates <- stepTemplates


  # List all steps in the workflow template as a data frame
  # @return A data.frame with step metadata
  env$df <- function() {
    stepNames <- data.frame(fullName = ls(env$stepTemplates))
    stepDf <- byNotEmptyAsDf(stepNames, function(stepName) {
      fullName <- stepName$fullName
      df <- env$stepTemplates[[fullName]]$stepDf
      df$fullName <- fullName
      return(df)
    })
  }

  # Add a step template to the workflow template
  env$addStepTemplate <- function(stepTemplate, name = NULL) {
    if (is.null(name)) {
      name <- stepTemplate$stepDf$description
      if (is.null(name) || is.na(name)) {
        name <- paste0("Step_", length(env$stepTemplates) + 1)
      }
    }
    stepTemplate$workflow <- env
    env$stepTemplates[[name]] <- stepTemplate
    invisible(env)
  }

  env$setWorkflowTreeRootFolder <- function(rootFolder) {
    .workflow_template_private$setValue("treePath",rootFolder)
    .workflow_template_private$setValue("treeIdent",NULL)
  }

  env$setWorkflowTreeName <- function(treeName) {
    .workflow_template_private$setValue("treeName",treeName)
    .workflow_template_private$setValue("treeIdent",NULL)
  }

  env$setWorkflowTreeIdent <- function(treeIdent,from=pwd()) {

    treeResource <- loadResource(treeIdent)
    if (is.null(treeResource) || treeResource$nodeType!="Analysis Tree") {
      log_error(treeIdent,"does not exist or is not a Tree")
    } else {
      .workflow_template_private$setValue("treeIdent",treeResource$resourceId)
      .workflow_template_private$setValue("treeName",treeResource$name)
      .workflow_template_private$setValue("treePath",loadResource(treeResource$parentId)$path)
    }

  }

  # Create a re-execution plan for outdated steps
  # @return A data.frame describing the execution plan
  env$createExecutionPlan <- function() {
    stepsDf <- env$df()

    .workflow_template_private$collectInternalLinks(env)
    iL <- env$internalLinks

    allSteps <- stepsDf
    executionPlan <- byNotEmptyAsDf(allSteps, function(st) {
      internalLinks <- env$internalLinks[env$internalLinks$targetStep == st$fullName, ]
      usingSteps <- env$internalLinks[env$internalLinks$sourceStep == st$fullName, ]$targetStep
      usedSteps <- env$internalLinks[env$internalLinks$targetStep == st$fullName, ]$sourceStep
      st$dependencies <- paste(unique(usedSteps), collapse = ",", sep = "/")
      if (st$dependencies == "") {
        st$dependencies <- NA
      }
      st$usage <- paste(unique(usingSteps), collapse = ",", sep = "/")
      if (st$usage == "") st$usage <- NA
      st$toUpdate<-NA
      return(st)
    })
    if (!("description" %in% names(executionPlan))) executionPlan$description<-""
    if (!("rationale" %in% names(executionPlan))) executionPlan$rationale<-""
    if (!("sourceEntityId" %in% names(executionPlan))) executionPlan$sourceEntityId<-""
    if (!("sourceName" %in% names(executionPlan))) executionPlan$sourceName<-""
    executionPlan <- dplyr::select(
      executionPlan,
      "description",
      "rationale",
      "sourceEntityId",
      "sourceName",
      "fullName",
      "toUpdate",
      "dependencies",
      "usage"
    )
    executionPlan$inPlace <- FALSE
    return(executionPlan)
  }


  # Execute a given execution plan
  # @param executionPlan A data.frame as returned by \code{createReexecutionPlan}
  # @return Invisibly returns NULL
  env$executePlan <- function(executionPlan) {
    orderedWorkflow <-  .workflow_template_private$executionOrder(env, executionPlan)
    workflow <- NULL
    executionList <- c()
    for (i in seq_len(nrow(orderedWorkflow))) {
      nextData <- orderedWorkflow[i, ]
      nextItem <- nextData$fullName

      if ("dependencies" %in% names(nextData) && !is.na(nextData$dependencies)) {
        dependencies <- unique(strsplit(nextData$dependencies, ",")[[1]])
        for (dependencies in dependencies) {
          if (dependencies %in% executionList) {
            logging::loginfo("waiting to finish")
            finishRunResource(env$stepTemplates[[dependencies]]$stepDf$entityId)
            executionList <- executionList[executionList != dependencies]
          }
        }
      }
      if ("toUpdate" %in% names(nextData) && !is.null(nextData$toUpdate[[1]]) && !is.na(nextData$toUpdate[[1]])) {
        updateLinks(nextData$toUpdate[[1]])
      }
      if ("inPlace" %in% names(nextData) && nextData$inPlace) {
        runStepResource(nextData$sourceEntityId)
      } else {
        stepEnv <- env$stepTemplates[[nextData$fullName]]
        returnStep <- stepEnv$realise(workflow=workflow)
        if (is.null(workflow)) {
          workflow<-returnStep$workflow
        }
      }
      executionList <- c(executionList, nextItem)
    }
    if (length(executionList) > 0) {
      for (item in executionList) {
        finishRunResource(env$steps[[item]]$stepDf$sourceEntityId)
      }
    }

    # Restore parent relationships after all steps are realised
    # Only when addParental=FALSE (don't conflict with addParental feature)
    if (!env$addParental) {
      workflowDf <- env$df()

      if ("parentIdent" %in% names(workflowDf)) {
        # Build mapping from original sourceEntityId to fullName
        # We need to find the original sourceEntityId before realise updated it
        # The templateEntityId stores the original sourceEntityId

        # Get steps with parentIdent
        stepsWithParent <- workflowDf[!is.na(workflowDf$parentIdent), ]

        if (nrow(stepsWithParent) > 0) {
          for (i in seq_len(nrow(stepsWithParent))) {
            childFullName <- stepsWithParent$fullName[i]
            parentOriginalId <- stepsWithParent$parentIdent[i]

            # Find parent step by matching templateEntityId (original sourceEntityId)
            # After realise, templateEntityId holds the original, sourceEntityId holds the new
            parentRow <- which(workflowDf$templateEntityId == parentOriginalId)

            if (length(parentRow) == 1) {
              # Get new entityIds from stepTemplates (they have updated sourceEntityId after realise)
              childNewId <- workflowDf[workflowDf$fullName==childFullName,]$sourceEntityId
              parentFullName <- workflowDf[parentRow,]$fullName
              parentNewId <- workflowDf[parentRow,]$sourceEntityId

              if (!is.null(childNewId) && !is.null(parentNewId) &&
                  !is.na(childNewId) && !is.na(parentNewId)) {
                tryCatch({
                  detachStep(childNewId)
                  attachStep(childNewId, parentNewId)
                  logging::loginfo(paste("Restored parent relationship:", childNewId,childFullName, "->", parentNewId,parentFullName))
                }, error = function(e) {
                  logging::logwarn(paste("Failed to restore parent relationship for", childFullName, ":", e$message))
                })
              }
            }
          }
        }
      }
    }

    return(workflow)
  }


  # Parameterization API

  # Register a parameter for the workflow template
  # @param paramName Name of the parameter
  # @param stepPattern Pattern to match steps ("*" for all, "Step 1/*" for prefix, "Initial 1" for exact description)
  # @param property Property to parameterize ("remoteFile", "treeIdent", "gridArgument.name", "description", etc.)
  # @param target Target within the property (e.g., "./data.csv" for remoteFile, "cores" for gridArgument)
  # @param required Whether this parameter must be set before realization
  # @param defaultValue Default value if not set
  env$parameterizeStep <- function(paramName, stepPattern, property, target = NULL, required = TRUE, defaultValue = NULL) {
    # Validate stepPattern matches exactly one step (unless it's a wildcard pattern)
    workflowDf <- env$df()
    matchingSteps <- .workflow_template_private$findMatchingSteps(env, stepPattern, workflowDf)

    if (length(matchingSteps) == 0) {
      stop("Parameter '", paramName, "': stepPattern '", stepPattern, "' matches no steps. ",
           "Available descriptions: ", paste(workflowDf$description, collapse = ", "), call. = FALSE)
    }

    if (length(matchingSteps) > 1 && !grepl("\\*", stepPattern)) {
      stop("Parameter '", paramName, "': stepPattern '", stepPattern, "' matches multiple steps: ",
           paste(matchingSteps, collapse = ", "), ". Use a more specific pattern.", call. = FALSE)
    }

    # Store the resolved fullName for single matches (not wildcards)
    resolvedPattern <- if (length(matchingSteps) == 1 && !grepl("\\*", stepPattern)) {
      matchingSteps[1]
    } else {
      stepPattern
    }

    paramDef <- list(
      name = paramName,
      stepPattern = resolvedPattern,
      property = property,
      target = target,
      required = required,
      defaultValue = defaultValue,
      value = NULL,  # Start with NULL - will be set explicitly via setParameter() or use defaultValue during apply
      applied = FALSE
    )

    env$parameters[[paramName]] <- paramDef
    invisible(env)
  }

  # Set a parameter value
  # @param paramName Name of the parameter to set
  # @param value Value to set
  env$setParameter <- function(paramName, value) {
    if (!exists(paramName, envir = env$parameters)) {
      stop("Parameter '", paramName, "' has not been defined. Use parameterizeStep() first.", call. = FALSE)
    }

    paramDef <- env$parameters[[paramName]]
    paramDef$value <- value
    paramDef$applied <- FALSE
    env$parameters[[paramName]] <- paramDef

    invisible(env)
  }

  # Validate all required parameters are set
  # @return TRUE if valid, throws error otherwise
  env$validateParameters <- function() {
    params <- ls(env$parameters)
    missing <- c()

    for (paramName in params) {
      paramDef <- env$parameters[[paramName]]
      if (paramDef$required && (is.null(paramDef$value) || is.na(paramDef$value))) {
        missing <- c(missing, paramName)
      }
    }

    if (length(missing) > 0) {
      stop("Required parameters not set: ", paste(missing, collapse = ", "), call. = FALSE)
    }

    invisible(TRUE)
  }

  # List all parameters
  # @return Data frame of parameters
  env$listParameters <- function() {
    params <- ls(env$parameters)
    if (length(params) == 0) {
      return(data.frame(
        name = character(0),
        stepPattern = character(0),
        property = character(0),
        target = character(0),
        required = logical(0),
        value = character(0),
        applied = logical(0),
        stringsAsFactors = FALSE
      ))
    }

    paramList <- lapply(params, function(paramName) {
      paramDef <- env$parameters[[paramName]]
      data.frame(
        name = paramDef$name,
        stepPattern = paramDef$stepPattern,
        property = paramDef$property,
        target = ifelse(is.null(paramDef$target), "", as.character(paramDef$target)),
        required = paramDef$required,
        value = ifelse(is.null(paramDef$value), "", as.character(paramDef$value)),
        applied = paramDef$applied,
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, paramList)
  }

  # Save workflow template to JSON file
  # @param filepath Path to save the JSON file
  # @param pretty Whether to format JSON with indentation
  env$toJSON <- function(filepath, pretty = TRUE) {
    # Get workflow data
    workflowDf <- env$df()

    if (is.null(workflowDf) || nrow(workflowDf) == 0) {
      stop("Cannot export empty workflow template - contains no steps", call. = FALSE)
    }

    # Don't remove entityIds - they're needed for step identification
    # Only remove templateEntityId which is only for realized steps
    if ("templateEntityId" %in% names(workflowDf)) {
      workflowDf$templateEntityId <- NA
    }

    # Convert parameters to list for JSON
    params <- ls(env$parameters)
    parametersList <- list()
    if (length(params) > 0) {
      parametersList <- lapply(params, function(paramName) {
        paramDef <- env$parameters[[paramName]]
        list(
          name = paramDef$name,
          stepPattern = paramDef$stepPattern,
          property = paramDef$property,
          target = paramDef$target,
          required = paramDef$required,
          defaultValue = if (is.null(paramDef$defaultValue)) NA else paramDef$defaultValue,  # Convert NULL to NA for JSON
          value = if (is.null(paramDef$value)) NA else paramDef$value  # Convert NULL to NA for JSON
        )
      })
    }

    # Create export structure
    # Note: Internal links are embedded in remoteFiles and will be reconstructed during load
    exportData <- list(
      version = "1.0",
      parameters = parametersList,
      workflow = workflowDf
    )

    # Write to file
    jsonlite::write_json(exportData, filepath, pretty = pretty, auto_unbox = TRUE)

    log_info("Workflow template saved to: ", filepath)
    invisible(filepath)
  }

  env$realise <- function() {
    # Apply parameters before execution
    .workflow_template_private$applyParameters(env)

    return(env$executePlan(env$createExecutionPlan()))
  }


  # --- Private/Internal Methods (not exported to user) ---

  .workflow_template_private <- new.env(parent = emptyenv())

  .workflow_template_private$collectInternalLinks <- function(env) {
    stepsDf <- env$df()
    remoteFiles <- byNotEmptyAsDf(stepsDf, function(stepInstance) {
      rf <- stepInstance$remoteFiles[[1]]
      if (is.null(rf) || nrow(rf)==0) {return(NULL)}
      return(rf)
    })
    if (is.null(remoteFiles) || nrow(remoteFiles)==0) {
      # Initialize as empty data frame when no remote files
      env$internalLinks <- data.frame()
      return(NULL)
    }
    if ("sourceStep" %in% names(remoteFiles)) {
      allTargets <- remoteFiles[!is.na(remoteFiles$sourceStep),]
      env$internalLinks <- allTargets
    } else {
      # Initialize as empty data frame when no sourceStep column
      env$internalLinks <- data.frame()
    }

  }


  .workflow_template_private$executionOrder <- function(env, plan) {
    .workflow_template_private$executionOrderInternal(env, plan)
  }

  .workflow_template_private$executionOrderInternal <- function(env, plan, startSteps = NULL, counter = 0) {
    counter <- counter + 1
    if (!"dependencies" %in% names(plan)) {
      plan$dependencies <- NA
    }
    if (!"usage" %in% names(plan)) {
      plan$usage <- NA
    }
    if (is.null(startSteps)) {
      startSteps <- plan[is.na(plan$dependencies), ]
      plan <- plan[!is.na(plan$dependencies), ]
    }
    if (is.null(startSteps) || nrow(startSteps) == 0) {
      logging::logwarn("No step without dependencies, no executable order")
      return(NULL)
    }
    for (s in seq_len(nrow(startSteps))) {
      startStep <- startSteps[s, ]
      if (!is.na(startStep$usage)) {
        dependenciess <- strsplit(startStep$usage, ",", fixed = TRUE)[[1]]
        if (length(dependenciess) > 0) {
          for (dependentStepName in dependenciess) {
            dependenciesHandle <- plan[plan$fullName == dependentStepName, ]
            if (
              nrow(dependenciesHandle) == 1 &&
                "dependencies" %in% names(dependenciesHandle)
            ) {
              stepDependencies <- strsplit(
                dependenciesHandle$dependencies,
                ",",
                fixed = TRUE
              )[[1]]
              if (all(stepDependencies %in% startSteps$fullName)) {
                startSteps <- plyr::rbind.fill(startSteps, dependenciesHandle)
                plan <- plan[plan$fullName != dependentStepName, ]
              }
            }
          }
        }
      }

    }
    if (nrow(plan) == 0 || counter > 500) {
      if (counter > 500) {
        logging::logwarn("could not add all steps to execution order, check for cycles")
      }
      return(startSteps)
    }
    .workflow_template_private$executionOrderInternal(env, plan, startSteps, counter)
  }

  .workflow_template_private$setValue <- function(key,value) {
    stepNames <- data.frame(fullName = ls(env$stepTemplates))
    stepDf <- byNotEmpty(stepNames, function(stepName) {
      fullName <- stepName$fullName
      stepEnv <- env$stepTemplates[[fullName]]
      stepEnv$setStepValue(key,value)
      return(NULL)
    })
  }

  # Apply all parameters to workflow template
  .workflow_template_private$applyParameters <- function(env) {
    params <- ls(env$parameters)

    for (paramName in params) {
      paramDef <- env$parameters[[paramName]]

      # Skip if already applied
      if (paramDef$applied) {
        next
      }

      # Determine the value to use: explicit value or defaultValue
      valueToApply <- paramDef$value
      if (is.null(valueToApply)) {
        valueToApply <- paramDef$defaultValue
      }

      # Skip if no value to apply
      if (is.null(valueToApply)) {
        next
      }

      # Find matching steps
      workflowDf <- env$df()
      matchingSteps <- .workflow_template_private$findMatchingSteps(env, paramDef$stepPattern, workflowDf)

      if (length(matchingSteps) == 0) {
        logging::logwarn(paste0("Parameter '", paramName, "' matched no steps with pattern '", paramDef$stepPattern, "'"))
        next
      }

      # Apply parameter to each matching step
      for (stepName in matchingSteps) {
        stepTemplate <- env$stepTemplates[[stepName]]

        tryCatch({
          if (paramDef$property == "remoteFile") {
            # Change remote file
            stepTemplate$changeStepRemoteFile(
              name = paramDef$target,
              newIdent = valueToApply
            )
          } else if (paramDef$property == "localFile") {
            # Change local file
            stepTemplate$changeStepLocalFile(
              name = paramDef$target,
              newPath = valueToApply
            )
          } else if (paramDef$property == "treeIdent") {
            # Change tree
            stepTemplate$setStepTree(valueToApply)
          } else if (startsWith(paramDef$property, "gridArgument.")) {
            # Set grid argument
            argName <- substring(paramDef$property, 14)  # Remove "gridArgument." prefix
            stepTemplate$setGridArgument(argName, valueToApply)
          } else if (paramDef$property == "description") {
            stepTemplate$setStepDescription(valueToApply)
          } else if (paramDef$property == "rationale") {
            stepTemplate$setStepRationale(valueToApply)
          } else if (paramDef$property == "stepName") {
            stepTemplate$setStepName(valueToApply)
          } else {
            logging::logwarn("Unknown property type '", paramDef$property, "' for parameter '", paramName, "'")
          }
        }, error = function(e) {
          logging::logerror("Failed to apply parameter '", paramName, "' to step '", stepName, "': ", e$message)
        })
      }

      # Mark as applied
      paramDef$applied <- TRUE
      env$parameters[[paramName]] <- paramDef
    }
  }

  # Find steps matching a pattern
  .workflow_template_private$findMatchingSteps <- function(env, pattern, workflowDf) {
    if (pattern == "*") {
      # All steps
      return(workflowDf$fullName)
    } else if (grepl("\\*$", pattern)) {
      # Prefix match: "Step 1/*" or "DMG L1/*"
      prefix <- sub("\\*$", "", pattern)
      matching <- workflowDf[startsWith(workflowDf$fullName, prefix), ]
      return(matching$fullName)
    } else {
      # Try exact fullName match first, then exact description match
      matching <- workflowDf[workflowDf$fullName == pattern, ]
      if (nrow(matching) == 0) {
        matching <- workflowDf[workflowDf$description == pattern, ]
      }
      return(matching$fullName)
    }
  }

  ############## INTERNAL METHODS END



  return(env)
}


#' Load workflow template from JSON file
#'
#' Loads a workflow template that was saved with toJSON(), including
#' all step definitions, internal links, and parameter definitions.
#'
#' @param filepath Path to the JSON file to load
#' @return A workflow template environment with all parameters and steps
#'
#' @examples
#' \dontrun{
#' # Save a template
#' workflowTemplate$toJSON("my_template.json")
#'
#' # Load it back
#' loadedTemplate <- workflowTemplateFromJSON("my_template.json")
#'
#' # Set parameters and realize
#' loadedTemplate$setParameter("inputDataset", "newFileId")
#' realizedWorkflow <- loadedTemplate$realise()
#' }
#'
#' @export
workflowTemplateFromJSON <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath, call. = FALSE)
  }

  # Read JSON file
  templateData <- jsonlite::read_json(filepath, simplifyVector = TRUE)

  # Validate version
  if (is.null(templateData$version)) {
    warning("No version information in template file, assuming version 1.0")
    templateData$version <- "1.0"
  }

  # Check required fields
  if (is.null(templateData$workflow) || nrow(templateData$workflow) == 0) {
    stop("Invalid template file: no workflow data found", call. = FALSE)
  }

  # Create empty workflow to hold steps
  workflow <- createWorkflow()
  workflow$isImporting <- TRUE

  # Create step environments from workflow data
  workflowDf <- templateData$workflow

  # Clean up the workflow data - convert empty strings to NA for optional fields
  if ("parentIdent" %in% names(workflowDf)) {
    workflowDf$parentIdent[workflowDf$parentIdent == "" | is.null(workflowDf$parentIdent)] <- NA
  }
  if ("sourceEntityId" %in% names(workflowDf)) {
    workflowDf$sourceEntityId[workflowDf$sourceEntityId == "" | is.null(workflowDf$sourceEntityId)] <- NA
  }
  if ("entityId" %in% names(workflowDf)) {
    workflowDf$entityId[workflowDf$entityId == "" | is.null(workflowDf$entityId)] <- NA
  }

  # Clean up remoteFiles data - ensure ident fields are valid
  if ("remoteFiles" %in% names(workflowDf)) {
    for (i in seq_len(nrow(workflowDf))) {
      if (!is.null(workflowDf$remoteFiles[[i]]) && is.data.frame(workflowDf$remoteFiles[[i]])) {
        rf <- workflowDf$remoteFiles[[i]]
        if ("ident" %in% names(rf)) {
          # Convert empty strings to NA
          rf$ident[rf$ident == "" | is.null(rf$ident)] <- NA
        }
        workflowDf$remoteFiles[[i]] <- rf
      }
    }
  }

  for (i in seq_len(nrow(workflowDf))) {
    stepEnv <- createStepEnv(stepDf = workflowDf[i, ], workflow = workflow)
  }

  workflow$isImporting <- FALSE

  # Note: Internal links are embedded in remoteFiles with sourceStep information
  # They will be reconstructed when the workflow template is used

  # Create workflow template
  workflowTemplate <- createWorkflowTemplateEnv(workflow)

  # Restore parameters if they exist
  if (!is.null(templateData$parameters) && length(templateData$parameters) > 0) {
    params <- templateData$parameters

    # Handle both list and data.frame formats
    if (is.data.frame(params)) {
      for (i in seq_len(nrow(params))) {
        param <- params[i, ]
        workflowTemplate$parameterizeStep(
          paramName = param$name,
          stepPattern = param$stepPattern,
          property = param$property,
          target = if (is.null(param$target) || is.na(param$target)) NULL else param$target,
          required = param$required,
          defaultValue = if (is.null(param$defaultValue) || is.na(param$defaultValue)) NULL else param$defaultValue
        )

        # Set the value only if it was explicitly saved (not NA/NULL/"")
        if (!is.null(param$value) && !is.na(param$value) && param$value != "") {
          workflowTemplate$setParameter(param$name, param$value)
        }
      }
    } else if (is.list(params)) {
      for (param in params) {
        workflowTemplate$parameterizeStep(
          paramName = param$name,
          stepPattern = param$stepPattern,
          property = param$property,
          target = param$target,
          required = param$required,
          defaultValue = param$defaultValue
        )

        # Set the value only if it was explicitly saved (not NA/NULL/"")
        if (!is.null(param$value) && !is.na(param$value) && param$value != "") {
          workflowTemplate$setParameter(param$name, param$value)
        }
      }
    }
  }

  log_info("Workflow template loaded from: ", filepath)
  return(workflowTemplate)
}


  # TODO resolv parent kram






