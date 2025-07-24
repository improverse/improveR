filterOutsideLinks <- function(workflow) {
   outsideLinks <- byNotEmptyAsDf(workflow,function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      remoteFiles$targetStep <- workflowTask$fullName
      if (!("sourceStep" %in% names(remoteFiles))) {
        remoteFiles$sourceStep<-NA
      }
      oLinks <- remoteFiles[remoteFiles$asLink & is.na(remoteFiles$sourceStep),]
      return(oLinks)
    }
    return(NULL)
  })%>%
    dplyr::distinct(targetStep,version,.keep_all = T)
   return(outsideLinks)
}


exportWorkflow <- function(workflow,workflowName,targetFolder=".") {
  #list of external links
  workFlowDf<- workflow$df()
  outsideLinks <- filterOutsideLinks(workFlowDf) %>%
    dplyr::select(stepHandle,targetStep,name,version,filehash,maxVersion)

  insideLinks <- byNotEmptyAsDf(workflow$df(),function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[remoteFiles$asLink & !is.na(remoteFiles$sourceStep),]
      if (nrow(oLinks)>0) {
        return(oLinks %>% dplyr::distinct(targetStep,sourceStep,sourceInventoryPath,.keep_all = T))
      }
    }
    return(NULL)
  })

  #%>%
  #  dplyr::select(targetStep,name)

  inputs <- byNotEmptyAsDf(workFlowDf,function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[!remoteFiles$asLink,]
      if (nrow(oLinks)>0) {
        oLinks$name <- improveR::loadResource(oLinks$ident)$name
        oLinks$targetStep<-workflowTask$fullName
      }
      return(oLinks)
    }
    return(NULL)
  })%>%
    dplyr::select(stepHandle,targetStep,name)

  #pull all steps
  workflowFolder <- file.path(targetFolder,workflowName,fsep = "/")
  dir.create(workflowFolder)
  exportDir <- normalizePath(workflowFolder,winslash = "/")
  linkDir <- file.path(exportDir,"links",fsep = "/")
  dir.create(linkDir)


  taskDirs <- byNotEmptyAsDf(workFlowDf,function(exportTask) {
    taskDir <- file.path(exportDir,exportTask$handle)
    dir.create(taskDir)
    cloneCli(exportTask$sourceEntityId,taskDir)
    return(data.frame(taskDir=taskDir,handle=exportTask$handle))
  })

  #seperate in inputs, outside links, outputs

  if (nrow(taskDirs)>0) {
    for (iT in 1:nrow(taskDirs)) {
      taskDf <- taskDirs[iT,]
      unlink(file.path(taskDf$taskDir,".improve",fsep = "/"),recursive = T,force = T)
      #remove insideLinks
      taskInsideLinks <- insideLinks[insideLinks$stepHandle==taskDf$handle,]$name
      if (length(taskInsideLinks)>0) {
        for (i in 1:length(taskInsideLinks)) {
          insideLinkPath <- file.path(taskDf$taskDir,taskInsideLinks[i],fsep = "/")
          unlink(insideLinkPath,force = T)
        }
      }

      taskOutsideLinks <- outsideLinks[outsideLinks$stepHandle==taskDf$handle,]
      if (nrow(taskOutsideLinks)>0) {
        for (i in 1:nrow(taskOutsideLinks)) {
          outsideLink <- taskOutsideLinks[i,]
          outsideLinkPath <- file.path(taskDf$taskDir,taskOutsideLinks[i,]$name,fsep = "/")
          createdLinks <- dir(linkDir)

          if (outsideLink$version %in% createdLinks) {
            unlink(outsideLinkPath,force = T)
          } else {
            storedLinkPath <- file.path(linkDir,outsideLink$version,fsep = "/")
            file.rename(outsideLinkPath,storedLinkPath)
          }
        }
      }

      inputFolderPath <- file.path(exportDir,
                                   paste0(taskDf$handle,"inputFiles"))
      dir.create(inputFolderPath)
      inputFiles <- inputs[inputs$stepHandle==taskDf$handle,]
      if (nrow(inputFiles)>0) {
        for (i in 1:nrow(inputFiles)) {
          inputPath <- file.path(taskDf$taskDir,inputFiles[i,]$name,fsep = "/")
          outputPath <- file.path(inputFolderPath,inputFiles[i,]$name,fsep = "/")
          dir.create(dirname(outputPath),recursive = T,showWarnings = F)
          file.rename(inputPath,outputPath)
        }
      }

      outputFolderPath <- file.path(exportDir,
                                    paste0(taskDf$handle,"outFiles"))
      dir.create(outputFolderPath)
      outputFiles <- dir(taskDf$taskDir,all.files = T)
      x<- lapply(outputFiles, function(outputFile) {
        if (outputFile!="." && outputFile!="..") {
          file.rename(
            file.path(taskDf$taskDir,outputFile),
            file.path(outputFolderPath,outputFile)
          )
        }
      })
      file.rename(outputFolderPath,file.path(taskDf$taskDir,"outputFiles"))
      file.rename(inputFolderPath,file.path(taskDf$taskDir,"inputFiles"))
    }
  }
  jsonlite::write_json(workFlowDf,
                       file.path(exportDir,"workflow.json"),
                       pretty = T)


  #create tool mapping

  toolMapping <- byNotEmptyAsDf(workFlowDf,function(task) {
    processes <- task$processes[[1]] %>%
      dplyr::select(runserverLabel,toolLabel,toolInstance,gridTool) %>%
      dplyr::mutate(key=paste(runserverLabel,toolLabel,toolInstance,gridTool,sep=":::"))
    return(processes)
  }) %>%
    dplyr::distinct(key,.keep_all = T)
  toolMapping <- toolMapping[,c(5,1,2,3,4)]

  jsonlite::write_json(toolMapping,
                       file.path(targetFolder,
                                 paste0(workflowName,"ToolMapping.json")),
                       pretty=TRUE)

  #create link mapping
  #add stepname / treename
  outsideLinks <- filterOutsideLinks(workFlowDf) %>%
    dplyr::select(ident,version,filehash,name) %>%
    byNotEmptyAsDf(function(link) {
      sameNames <- unique(
        outsideLinks[outsideLinks$version==link$version,]$name
      )
      link$name <- paste(sameNames,collapse = ", ")
      return(link)
    }) %>%
    dplyr::distinct(ident,version,filehash,name) %>%
    dplyr::mutate(key=version)
  outsideLinks <- outsideLinks[,c(5,1,2,3,4)]
  jsonlite::write_json(outsideLinks,
                       file.path(targetFolder,
                                 paste0(workflowName,"LinkMapping.json")),
                       pretty=TRUE)

  utils::zip(zipfile = paste0(workflowFolder,".zip"),files = workflowFolder)
  #print(workflowFolder)
  unlink(workflowFolder,force = T,recursive = T)
}
