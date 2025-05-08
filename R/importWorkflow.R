importWorkflow <- function(workflowFolder,importRepoFolder) {

  importRepoFolderResource <- createFolder(TEST_FOLDER,"import1")
  importFolder <- workflowFolder
  importFolder <- normalizePath(importFolder,winslash = "/")
  importWF <- jsonlite::read_json(
    file.path(importFolder,"workflow.json",fsep = "/"),
    simplifyVector = T
  ) %>% persistWorkflowChanges() %>%
    dplyr::pull(workflowHandle) %>%
    unique() %>%
    detachWorkflowFromResources() %>%
    detachWorkflowFromTrees() %>%
    setWorkflowTreeRootFolder(importRepoFolder) %>%
    retrieveWorkflow()

  #uploadLinks
  #here integrate mapping
  outsideLinkFolder <- file.path(importFolder,"links",fsep = "/")
  providedLinks <- dir(outsideLinkFolder)
  linkMapping <- list()
  if (length(providedLinks)>0) {
    for (i in 1:length(providedLinks)) {
      linkResource <- createFile(importRepoFolderResource,
                                 fileName = providedLinks[i],
                                 localPath = file.path(outsideLinkFolder,providedLinks[i]))
      linkMapping[[providedLinks[i]]] <- linkResource$entityId
    }
  }
  #map outsideLinks

  outsideLinks <- filterOutsideLinks(importWF)
  mappedIdents <- unlist(
    lapply(outsideLinks$version,function(v){return(linkMapping[[v]])})
  )
  outsideLinks$ident<-mappedIdents
  x<-byNotEmpty(outsideLinks,function(oL) {
    changeStepRemoteFileDf(oL$stepHandle,oL$name,oL)
  })
  importWF <- retrieveWorkflow(importWF)

  orderedWorkflow <- executionOrder(importWF)
  executionList <- c()
  for (i in 1:nrow(orderedWorkflow)) {
    nextData <- orderedWorkflow[i,]
    nextItem <- nextData$handle




    if ("dependencies" %in% names(nextData) && !is.na(nextData$dependencies)) {
      dependencies <- unique(strsplit(nextData$dependencies,",")[[1]])
      for (j in 1:length(dependencies)) {
        dependency <- dependencies[j]
        if (dependency %in% executionList) {
          logging::loginfo("waiting to finish")
          finishRun(dependency)
          executionList <- executionList[executionList!=dependency]
        }
      }
    }
    #create step, add links
    #remove inputfiles
    remoteFiles <- nextData$remoteFiles[[1]]
    remoteFiles <- remoteFiles[remoteFiles$asLink,]
    nextData$remoteFiles[[1]]<- remoteFiles
    storeStep(stepHandle = nextItem,stepList = nextData)
    realiseStep(nextItem,run = F)
    nextStep <- getStepResource(nextItem)
    stepInputFolderPath <- paste0("import",nextItem)
    dir.create(stepInputFolderPath,showWarnings = F,recursive = T)
    stepInputFolderPath <- normalizePath(paste0("import",nextItem),winslash = "/")
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




