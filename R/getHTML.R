#' gets a HTML object
#'
#' @param ident path, resource or entity ID of the HTML file
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @references ics1141
#' @export
getHTML <- function(ident,from=pwd(),addAsLink=TRUE,caption="") {
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = T,caption = caption,description="",folderName = "HTML",func=getDesc)
  )
}

#' Includes HTML in rMarkdown, creates a string that has to be outputted
#'
#' @param ident path, resource or entity ID of the picture
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param includeCaption display the caption
#' @references ics1141
#' @export

showHTML <- function(ident,from=pwd(),addAsLink=TRUE,caption="",includeCaption=T) {
  graphicsObject <- getHTML(ident=ident,from = from,addAsLink = addAsLink,caption = caption)
  if (is.null(graphicsObject$path)) {
    if (length(graphicsObject)==0) {
      logging::logerror("No HTML object found")
      logging::logerror(ident)
      return()
    }
    figString <-""
    for (i in 1:length(graphicsObject)) {
      figString <- paste(figString,
                         showHTML(graphicsObject[i][[1]]$resource,addAsLink = addAsLink,caption = caption),
                         sep="\n\n")
    }
    return(figString)
  }  else {

    if (!includeCaption) {
      caption <- ""
    } else {
      caption <- graphicsObject$caption
    }



    myTable <- htmltools::tagList(htmltools::includeHTML( normalizePath(graphicsObject$path,winslash = "/")),
                                  caption = caption)
    return(myTable)

  }
}


