exportWorkflow <- function(workflow) {


  #list of external links
  outsideLinks <- byNotEmptyAsDf(workflow,function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[remoteFiles$asLink & !is.na(remoteFiles$ident),]
      return(oLinks)
    }
    return(NULL)
  }) %>%
    dplyr::distinct(stepHandle,version,.keep_all = T) %>%
    dplyr::select(stepHandle,name,version,filehash,maxVersion)

  insideLinks <- byNotEmptyAsDf(workflow,function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[remoteFiles$asLink & is.na(remoteFiles$ident),]
      return(oLinks)
    }
    return(NULL)
  }) %>%
    dplyr::select(stepHandle,name)

  inputs <- byNotEmptyAsDf(workflow,function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[!remoteFiles$asLink,]
      return(oLinks)
    }
    return(NULL)
  })%>%
    dplyr::select(stepHandle,name)

  #pull all steps
  dir.create("export")
  exportDir <- normalizePath("export",winslash = "/")
  linkDir <- file.path(exportDir,"links",fsep = "/")
  dir.create(linkDir)

  taskDirs <- byNotEmptyAsDf(workflow,function(exportTask) {
    taskDir <- file.path(exportDir,exportTask$handle)
    dir.create(taskDir)
    cloneCli(exportTask$entityId,taskDir)
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
      if (length(taskOutsideLinks)>0) {
        for (i in 1:length(taskOutsideLinks)) {
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
      if (length(inputFiles)>0) {
        for (i in 1:length(inputFiles)) {
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
  jsonlite::write_json(workflow,
                       file.path(exportDir,"workflow.json"),
                       pretty = T)

}
