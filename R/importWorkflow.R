#' Import a Workflow from a Zip File into a Repository Folder
#'
#' This function imports a workflow from a specified zip file into a given repository folder.
#'
#' @param workflowFile Character. Path to the workflow zip file to import.
#' @param importRepoFolder Character. Path to the repository folder where the workflow will be imported.
#'
#' @return None. The function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' importWorkflow("path/to/workflow.zip", "path/to/repo/folder")
#' }
#'
#' @importFrom zip unzip
#' @export
importWorkflow <- function(workflowFile,importRepoFolder) {
  workflowName <- strsplit(basename(workflowFile),split = ".",fixed = T)[[1]]
  workflowName <- paste(workflowName[1:(length(workflowName)-1)],collapse = ".")

  importFolder <- tempfile()
  dir.create(importFolder)
  zip::unzip(zipfile = workflowFile,exdir = importFolder)

  importFolder <- dir(importFolder,full.names = T)
  if (length(importFolder)!=1) {
    stop("workflow file is expected to be a zip file containing exactly one folder.")
  }
  if (!file.exists(file.path(importFolder,"workflow.json"))) {
    stop("no workflow.json file found in the first subfolder of the zip file")
  }
  importFolder <- normalizePath(importFolder,winslash = "/")
  importRepoFolderResource <- loadResource(importRepoFolder)
  if (is.null(importRepoFolderResource) || importRepoFolderResource$nodeType!="Folder") {
    stop("importRepoFolder has to be a folder in the repository and exist")
  }


  importWF <- jsonlite::read_json(
    file.path(importFolder,"workflow.json",fsep = "/"),
    simplifyVector = T
  )
  if (nrow(importWF)==0) {
    return(NULL)
  }
  workflow<-createWorkflow()
  for (i in 1:nrow(importWF)) {
    stepEnv <- createStepEnv(stepDf = importWF[i,],workflow = workflow)
  }
  workflowTemplate <- createWorkflowTemplateEnv(workflow)




  #uploadLinks
  #map outsideLinks
  outsideLinkFolder <- file.path(importFolder,"links",fsep = "/")
  providedLinks <- dir(outsideLinkFolder)
  linkMappingPath <- file.path(
    dirname(normalizePath(workflowFile)),
    paste0(workflowName,"LinkMapping.json")
    )

  #TODO check with ident / version /SHA
  linkMapping <- new.env()
  importMapping <- data.frame()
  if (file.exists(linkMappingPath)) {
    importMapping <-jsonlite::read_json(linkMappingPath,simplifyVector = T)
    x<-byNotEmpty(importMapping,function(linkMap){
      if (is.character(linkMap$ident) && linkMap$ident!="") {
        linkMapping[[linkMap$key]]<-linkMap$ident
      }
    })
  }


  if (length(providedLinks)>0) {
    for (i in 1:length(providedLinks)) {
      if (is.null(linkMapping[[providedLinks[i]]])) {
        linkName <- providedLinks[i]
        importLine <- importMapping[importMapping$key==providedLinks[i],]
        if (nrow(importLine) >0 && is.character(importLine$name) && !grepl(pattern = ",",x = importLine$name,fixed = T)) {
          linkName <- importLine$name
        }
        if (startsWith(linkName,"./")) {
          linkName<- substr(linkName,3,nchar(linkName))
        }
        linkResource <- createFile(importRepoFolderResource,
                                   fileName = linkName,
                                   localPath = file.path(outsideLinkFolder,providedLinks[i]))
        linkMapping[[providedLinks[i]]] <- linkResource$entityId
      }

    }
  }


  outsideLinks <- filterOutsideLinks(importWF)
  mappedIdents <- unlist(
    lapply(outsideLinks$version,function(v){return(linkMapping[[v]])})
  )
  outsideLinks$ident<-mappedIdents
  x<-byNotEmpty(outsideLinks,function(oL) {
    #changeStepRemoteFileDf(oL$stepHandle,oL$name,oL)
    targetStepName <- oL$targetStep
    targetStep <- workflowTemplate$stepTemplates[[targetStepName]]
    targetStep$changeStepRemoteFile(oL$name,oL$asLink,oL$name,oL$ident)
  })

  #map tools
  toolMappingPath <- file.path(
    dirname(normalizePath(workflowFile)),
    paste0(workflowName,"ToolMapping.json")
  )
  if (FALSE && file.exists(toolMappingPath)) {
    importMapping <-jsonlite::read_json(toolMappingPath,simplifyVector = T)
    processes <- byNotEmptyAsDf(importWF,function(task) {
      return(task$processes[[1]])
    })
    x<-byNotEmpty(importMapping,function(proc){
      keyParts <- strsplit(proc$key,":::",fixed=T)[[1]]
      runserverName <- keyParts[1]
      toolName <- keyParts[2]
      runserverToolName<-keyParts[3]
      affectedProcesses <- processes[processes$runserverName==runserverName & processes$toolName==toolName & processes$runserverToolName==runserverToolName,]
      y<-byNotEmpty(affectedProcesses,function(affectedProcess) {
        affectedProcess$toolName<-proc$toolName
        affectedProcess$runserverName<-proc$runserverName
        affectedProcess$runserverToolName<-proc$runserverToolName
        changeStepProcessDf(affectedProcess$handle,affectedProcess$name,affectedProcess)
      })
    })
  }


  orderedWorkflow  <- workflowTemplate$createExecutionPlan()
  executionList <- c()
  for (i in 1:nrow(orderedWorkflow)) {
    nextData <- orderedWorkflow[i,]
    nextItem <- nextData$fullName

    template <- workflowTemplate$stepTemplates[[nextItem]]


    if ("lineage" %in% names(nextData) && !is.na(nextData$lineage)) {
      dependencies <- unique(strsplit(nextData$lineage,",")[[1]])
      for (j in 1:length(dependencies)) {
        dependency <- dependencies[j]
        if (dependency %in% executionList) {
          logging::loginfo("waiting to finish")
          #finishRun(dependency)
          executionList <- executionList[executionList!=dependency]
        }
      }
    }
    #create step, add links
    #remove inputfiles
    remoteFiles <- template$stepDf$remoteFiles[[1]]
    remoteFiles <- remoteFiles[remoteFiles$asLink,]
    template$stepDf$remoteFiles[[1]]<- remoteFiles
    template$realise(run=F)

    nextStep <- improveR::loadResource(template$stepDf$entityId)
    stepInputFolderPath <- paste0("import",uuid::UUIDgenerate()  )
    dir.create(stepInputFolderPath,showWarnings = F,recursive = T)
    stepInputFolderPath <- normalizePath(stepInputFolderPath,winslash = "/")
    cloneCli(nextStep,localPath = stepInputFolderPath)

    # push input files
    inputFiles <- dir(file.path(importFolder,nextItem,"inputFiles",fsep = "/"),all.files = T)
    inputFiles <- inputFiles[!(inputFiles %in% c(".",".."))]
    if (length(inputFiles)>0) {
      for (iF in 1:length(inputFiles)) {
        inputPath <- file.path(importFolder,nextItem,"inputFiles",inputFiles[iF],fsep = "/")
        outputPath <- file.path(stepInputFolderPath,inputFiles[iF],fsep = "/")
        dir.create(dirname(outputPath),recursive = T,showWarnings = F)
        file.rename(inputPath,outputPath)
      }
    }
    pushCli(stepInputFolderPath)
    #TODO map variables

    # push output files
    #push run
    outputFiles <- dir(file.path(importFolder,nextItem,"outputFiles",fsep = "/"),all.files = T)
    outputFiles <- outputFiles[!(outputFiles %in% c(".",".."))]
    if (length(outputFiles)>0) {
      for (iF in 1:length(outputFiles)) {
        inputPath <- file.path(importFolder,nextItem,"outputFiles",outputFiles[iF],fsep = "/")
        outputPath <- file.path(stepInputFolderPath,outputFiles[iF],fsep = "/")
        dir.create(dirname(outputPath),recursive = T,showWarnings = F)
        file.rename(inputPath,outputPath)
      }
    }
    pushRunCli(stepInputFolderPath,command="import")
    unlink(file.path(importFolder,nextItem),recursive = T,force = T)
    unlink(stepInputFolderPath,recursive = T,force=T)
    executionList <- c(executionList,nextItem)
  }
  #if (length(executionList)>0) {
  #  for (i in 1:length(executionList)) {
  #    finishRun(executionList[i])
  #  }
  }




