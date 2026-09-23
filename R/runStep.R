#' Run a Step Resource
#'
#' Initiates the execution of a step on the server. This function triggers asynchronous
#' step execution, meaning the step's code and processes begin running on the improve
#' platform without blocking your R session. Control returns immediately, allowing you
#' to continue working or chain additional commands while the step runs in the background.
#'
#' @param ident A step identifier. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#' @param resetInventory Logical. If \code{TRUE}, the server resets the step's
#'   inventory before running. Defaults to \code{FALSE} (backwards compatible).
#' @param filesToKeep Character vector. Resource GUIDs of inventory items to
#'   keep when \code{resetInventory = TRUE}. Ignored when \code{resetInventory}
#'   is \code{FALSE}. Defaults to \code{NULL} (keep nothing / reset all).
#'
#' @return The step identifier, returned invisibly.
#'
#' @details
#' The function performs a quick validation to ensure the repository is in an editable
#' state, confirms the step exists, and then sends a command to the server to begin
#' execution. The actual step execution happens asynchronously on the server, managed
#' by the improve platform's execution infrastructure.
#'
#' This function is commonly used in workflow orchestration scenarios where multiple
#' steps need to be executed sequentially or in parallel. For sequential workflows,
#' combine with \code{\link{finishRunResource}} to wait for completion before proceeding.
#'
#' @seealso \code{\link{finishRunResource}} to wait for step completion
#'
#' @examples
#' \dontrun{
#' # Execute a single step
#' runStepResource(ident = "/improve-tutorial/Modeling/Step 1")
#' }
#'
#' @references ics1140
#' @export
runStepResource <- function(ident, resetInventory = FALSE, filesToKeep = NULL) {
  newStep <- refreshResource(ident)
  if (newStep$runStatus == "RUNNING") {
    log_error("Step is already running.")
    return(NULL)
  }

  improveEditable()

  # Build request body only when needed (backwards compatible — empty body
  # when neither filesToKeep nor sshPublicKeyHash is provided).
  data <- ""
  if (!is.null(filesToKeep)) {
    data <- list(filesToKeep = as.list(filesToKeep))
  }

  queryParams <- list()
  if (isTRUE(resetInventory)) {
    queryParams <- list(resetInventory = "true")
  }

  result <- authenticatedREST(
    "resources/{stepId}/run",
    urlParams = list(stepId = newStep$resourceId),
    queryParams = queryParams,
    data = data,
    restType = "POST"
  )

  invisible(ident)
}


#' Terminate a Running Step Resource
#'
#' Stops the execution of a currently running step on the server. This function sends
#' a termination request to the improve platform, which will halt the step's processes
#' and mark it as terminated. This is useful when you need to cancel a long-running
#' step that is no longer needed or when troubleshooting workflow execution issues.
#'
#' @param ident A step identifier. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#' @param verbose Logical. If \code{TRUE}, prints verbose output with HTTP status messages
#'   during termination (200: Step terminated, 304: Could not start run, 500: Runserver
#'   not reachable) and status confirmation messages. Defaults to \code{FALSE}.
#'
#' @return Logical value returned invisibly: \code{TRUE} if the termination request
#'   was successful (HTTP status 200), \code{FALSE} otherwise.
#'
#' @details
#' The function first validates that the repository is in an editable state using
#' \code{setEditable()}, then loads the specified step resource and sends a
#' termination command to the server. The server will stop all running processes
#' associated with the step.
#'
#' After a successful termination request, the function automatically polls the step's
#' status for up to 30 seconds (checking every 0.5 seconds) to confirm the server has
#' updated the runStatus. This prevents race conditions when immediately checking status
#' after termination, which is particularly important in rendered documents where code
#' executes rapidly without human delays.
#'
#' Note that terminating a step does not delete any outputs or results that may
#' have already been generated before termination.
#'
#' @seealso \code{\link{runStepResource}} to execute a step,
#'   \code{\link{finishRunResource}} to wait for step completion
#'
#' @examples
#' \dontrun{
#' # Terminate a running step
#' terminateStepResource(ident = "/improve-tutorial/Modeling/Step 1")
#'
#' # Terminate with verbose output
#' terminateStepResource(ident = "/improve-tutorial/Modeling/Step 1", verbose = TRUE)
#' }
#'
#' @references ics1140
#' @export
terminateStepResource <- function(ident, verbose = FALSE) {
  setEditable()
  stepToTerminate <- refreshResource(ident)

  if (is.null(stepToTerminate)) {
    log_warn("Step could not be loaded.")
    return(invisible(FALSE))
  }

  # isTRUE(), not ==. A resource without a runStatus column yields NULL, and
  # `NULL == "RUNNING"` is logical(0), which && cannot evaluate - the caller
  # would get "invalid length argument" instead of anything about the step.
  if (isTRUE(stepToTerminate$runStatus == "RUNNING")) {
    result <- authenticatedREST(
      "resources/{stepId}/terminate",
      urlParams = list(stepId = stepToTerminate$resourceId),
      restType = "POST"
    )
  } else {
    # sprintf, not glue. A resource without a runStatus column yields NULL, and
    # glue recycles to the shortest argument - so the whole message collapsed to
    # character(0) and log_warn() reported NOTHING. The isTRUE() guard above
    # stopped the crash and left the caller with silence instead, which is the
    # worse of the two failures: nothing in the log says the step was not
    # terminated, or why.
    status <- stepToTerminate$runStatus
    statusText <- if (length(status) == 0) "no run status" else as.character(status)[1]
    log_warn(sprintf("%s has run status %s. Cannot be terminated.",
                     paste(utils::head(as.character(stepToTerminate$path), 1),
                           collapse = ""),
                     statusText))
    return(invisible(FALSE))
  }

  # authenticatedREST answers EVERY non-2xx with NULL - its documented contract.
  # httr::status_code(NULL) has no method and dies with
  # "no applicable method for 'status_code' applied to an object of class NULL",
  # naming neither the step, nor the operation, nor the status that occurred -
  # while lastRestError() is holding all three (IMR-292).
  #
  # This is the family IMR-270 swept for and could not see: that audit searched
  # httr::content(, and this site calls httr::status_code(.
  if (is.null(result)) {
    err <- tryCatch(lastRestError(), error = function(e) NULL)
    detail <- if (is.null(err)) "improveR recorded no REST error" else
      sprintf("HTTP %s on %s %s", err$status_code, err$method, err$url)
    log_warn(sprintf("terminateStepResource: the terminate call for '%s' failed - %s",
                     paste(utils::head(as.character(ident), 3), collapse = ", "), detail))
    return(invisible(FALSE))
  }

  status <- httr::status_code(result)

  if (isTRUE(verbose)) {
    # Map specific codes to messages
    if (identical(status, 200L)) {
      log_info(glue::glue(
        "{ident} - HTTP {status}: Step is terminated"
      ))
    } else if (identical(status, 304L)) {
      log_info(glue::glue(
        "{ident} - HTTP {status}: Could start the run."
      ))
    } else if (identical(status, 500L)) {
      log_info(glue::glue(
        "{ident} - HTTP {status}: Runserver is not reachable"
      ))
    } else {
      log_info(glue::glue("{ident} - HTTP {status}: received"))
    }
  }

  # If termination request was successful, poll for status confirmation
  if (identical(status, 200L)) {
    maxWaitTime <- 30 # seconds
    pollInterval <- 0.5 # seconds
    elapsed <- 0

    while (elapsed < maxWaitTime) {
      Sys.sleep(pollInterval)
      elapsed <- elapsed + pollInterval

      # Refresh the step resource to get current status from server
      stepCheck <- refreshResource(stepToTerminate)

      # If status is no longer RUNNING, termination is confirmed
      if (!is.null(stepCheck) && stepCheck$runStatus != "RUNNING") {
        if (isTRUE(verbose)) {
          log_info(glue::glue(
            "{ident} - Status confirmed: {stepCheck$runStatus}"
          ))
        }
        break
      }
    }

    if (elapsed >= maxWaitTime && isTRUE(verbose)) {
      log_warn(glue::glue(
        "{ident} - Termination timeout: Status confirmation took longer than {maxWaitTime}s"
      ))
    }
  }

  return(invisible(identical(status, 200L)))
}


#' Wait for Step Execution to Finish
#'
#' Blocks until a running step finishes on the improve server. Optionally constrains
#' completion to a specific runserver and tool combination.
#'
#' @param ident A step identifier. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#' @param runserverName Optional runserver name. When set, completion is only
#'   acknowledged if the step finished on this runserver (must be paired with
#'   \code{runserverToolName}).
#' @param runserverToolName Optional tool name. When set, completion is only
#'   acknowledged if the step finished with this tool (must be paired with
#'   \code{runserverName}).
#'
#' @param timeout Seconds to wait before giving up. Defaults to
#'   \code{IMPROVER_FINISHRUN_TIMEOUT} when that holds a positive number, and to
#'   600 otherwise. Pass \code{Inf} to wait forever. Before IMR-268 there was no
#'   timeout at all: the wait loop left only on \code{FINISHED}, so a run ending
#'   \code{FAILED} or \code{TERMINATED} was polled forever.
#' @returns The step identifier, returned invisibly after the run completes.
#'
#' @details
#' The function polls the step status every 5 seconds until the run status is
#' \code{FINISHED}. If \code{runserverName} and \code{runserverToolName} are
#' provided, the function checks the most recent main process run and only returns
#' when the finished run matches the specified runserver tool.
#'
#' This is typically used after \code{\link{runStepResource}} when orchestrating
#' sequential workflows.
#'
#' @seealso \code{\link{runStepResource}} to start a step,
#'   \code{\link{terminateStepResource}} to stop a running step
#'
#' @examples
#' \dontrun{
#' # Run a step and wait for completion
#' runStepResource(ident = "/improve-tutorial/Modeling/Step 1")
#' finishRunResource(ident = "/improve-tutorial/Modeling/Step 1")
#' }
#'
#' @references ics1140
#' @export
finishRunResource <- function(ident,from=pwd(),runserverName=NULL, runserverToolName=NULL,
                              timeout=defaultFinishRunTimeout()) {

  step <- refreshResource(ident,from)

  if (is.null(step))
    return(NULL)
  toolId <- NULL
  if (!is.null(runserverName)&&!is.null(runserverToolName)) {
    toolId <- getToolId(runserverName, runserverToolName)
    print(toolId)
  } else if (!is.null(runserverName)||!is.null(runserverToolName)) {
    log_warn("runServerName and runServerToolName must be provided in finishRun, or none of them")
  }
  running<-TRUE
  wrongRun <- ""
  # Zeitmessung fuer die Wartegrenze (IMR-268). Die Schleife unten verliess
  # bisher nur FINISHED und pollte einen Lauf mit Endzustand FAILED oder
  # TERMINATED unbegrenzt weiter - genauso wie env$finishRun() es tat.
  startedAt <- Sys.time()
  lastState <- NA_character_

  if (!is.null(toolId)) {
    step <- refreshResource(step)
    state<-step$runStatus
    if (identical(state, "FINISHED")) {
      processes <- actualLoadProcessesForStep(step$resourceId)
      process <- dplyr::filter(processes,.data$processType=="main")
      stepTool <- processes$runserverToolId[1]
      if (toolId!=stepTool) {
        tryCatch({
          run <- actualLoadProcessRuns(process$id)
          run <- run[run$startedAt==max(run$startedAt),]
          currentRun <-run$id
          wrongRun <- currentRun
        },
        error=function(cond) {
          Sys.sleep(5)
        }
        )
      }
    }
  }

  while(running) {
    step <- refreshResource(step)
    state<-step$runStatus
    # Endzustand und Zeitgrenze pruefen, bevor irgendetwas mit state gerechnet
    # wird: bei einem Serverfehler ist step NULL und state damit NULL, und
    # NULL=="FINISHED" ist logical(0), was if() abweist.
    if (length(state)==1L && !is.na(state)) {
      lastState <- state
      if (state %in% c("FAILED","TERMINATED")) {
        stop(sprintf("finishRunResource: step ended in state '%s' after %.0f s. It will not finish.",
                     state, as.numeric(difftime(Sys.time(), startedAt, units="secs"))),
             call. = FALSE)
      }
    }
    elapsed <- as.numeric(difftime(Sys.time(), startedAt, units="secs"))
    if (elapsed >= timeout) {
      stop(sprintf(paste0("finishRunResource: gave up after %.0f s (limit %.0f s). Last run state ",
                          "was '%s'. Raise IMPROVER_FINISHRUN_TIMEOUT if the step legitimately ",
                          "needs longer."),
                   elapsed, timeout, if (is.na(lastState)) "unreadable" else lastState),
           call. = FALSE)
    }
    if (is.null(toolId)) {
      if (identical(state, "FINISHED")) {
        running<-F
      } else {
        Sys.sleep(5)
      }
    } else {
      processes <- actualLoadProcessesForStep(step$resourceId)
      process <- dplyr::filter(processes,.data$processType=="main")
      stepTool <- processes$runserverToolId[1]
      tryCatch({
        run <- actualLoadProcessRuns(process$id)
        run <- run[run$startedAt==max(run$startedAt),]
        currentRun <-run$id
        if (identical(state, "FINISHED") && toolId==stepTool && currentRun!=wrongRun) {
          running<-F
        } else {
          if (toolId!=stepTool && !identical(state, "FINISHED")) {
            wrongRun <- currentRun
          }


          Sys.sleep(5)
        }
      },
      error=function(cond) {
        Sys.sleep(5)
      }
      )
    }
  }
  invisible(ident)
}

loadRecurse <- function(ident,update=T) {
  if (update) {
    unloadFullChildResources(ident)
  }
  inventory <- loadFullChildResources(ident)
  inventoryResources <- inventory$data[[1]]
  inventoryFolders <- inventoryResources[inventoryResources$nodeType=="Folder",]
  if (nrow(inventoryFolders)>0) {
    newResources <- byNotEmptyAsDf(inventoryFolders,function(folder) {
      return(loadRecurse(folder))
    })
    inventoryResources<- plyr::rbind.fill(inventoryResources,newResources)
  }
  return(inventoryResources)
}


getStepResourceInventory <- function(step,recurse=F,update=T) {
  if (update) {
    unloadChildResources(step$entityId)
  }
  inventory <- loadFullChildResources(step$entityId)
  if (recurse) {
    inventoryResources <- loadRecurse(step$entityId,update)
    inventory$data <- list(inventoryResources)
  }
  inventoryResources <- inventory$data[[1]]
  inventoryResources <- byNotEmptyAsDf(inventoryResources,function(inventoryResource) {
    resPath <- getRelativePath(step,inventoryResource)
    resPath <- substr(resPath,3,nchar(resPath))
    inventoryResource$inventoryPath <- resPath
    return(inventoryResource)
  })
  inventory$data <- list(inventoryResources)
  inventory$type <- "inventory"
  return(inventory)
}
