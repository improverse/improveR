

#' gets a data table and retrieves the caption and description by the resource ID or entity ID
#' .xls and .xlsx files assumed to be excel, .rds as RDS, .sas7bdat as SAS all others as CSV
#'
#' @param ident path, resource or entity ID of the picture
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param ... pass over arguments for the read function
#' @param parser a custom parser can be handed over as argument parser=... . It needs to take a local path as first argument
#' @references ics1141
#' @export
getData <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",parser=NULL,...) {
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = T, caption = caption,description=description,folderName = "data",func=getDataByResource,parser=parser,...)
  )
}

#' gets a text file as chracter string and retrieves the caption and description by the resource ID or entity ID
#'
#' @param ident path, resource or entity ID of the picture
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param ... pass over arguments for the read function
#' @references ics1141
#' @export
getTextString <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",...) {
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = T,caption = caption,description=description,folderName = "text",func = getTextByResource,...)
  )
}




#' retrieves data table for rMarkdown, .xls and .xlsx files assumed to be excel, .rds as RDS, .sas7bdat as SAS all others as CSV
#'
#' @param resourceDesc resource descriptor of the data file
#'
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param parser a custom parsing function can be handed over. It needs to take a local path as first argument
#' @param ... pass over arguments for the read function
#'
getDataByResource <- function(resourceDesc,caption="",description="",parser=NULL,...) {
  resource <- loadResource(resourceDesc$resourceId)
  desc <- buildDescriptor(resource,resourceDesc$data[[1]],caption,description)
  resName <- tolower(resource$name)
  if (!is.null(parser)) {
    desc$data <- list(parser(desc$path,...))
    desc$dataType <- "Custom parser"
  }
  else if (endsWith(resName,".xls") | endsWith(resName,".xlsx")) {
    desc$data <- list(readxl::read_excel(path=desc$path,...))
    desc$dataType <- "Excel"
  } else if (endsWith(resName,".rds")){
    desc$data <- list(readRDS(desc$path))
    desc$dataType <- "RDS"
  } else if (endsWith(resName,".sas7bdat")){
    desc$data <- list(haven::read_sas(desc$path,...))
    desc$dataType <- "SAS"
  } else {
    desc$data <-list( utils::read.csv(file=desc$path,...))
    desc$dataType <- "CSV"
  }
  return(desc)
}

#' retrieves text string for rMarkdown
#'
#' @param resourceDesc resource descriptor of the picture
#'
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param ... pass over arguments for the read function
#'
getTextByResource <- function(resourceDesc,caption="",description="",...) {
  resource <- loadResource(resourceDesc$resourceId)
  desc <- buildDescriptor(resource,resourceDesc$data[[1]],caption,description)
  resName <- tolower(resource$name)
  desc$data <- paste(readLines(desc$path), collapse="\n")
  desc$dataType <- "Text"
  return(desc)
}


