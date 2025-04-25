#' gets a graphics object
#'
#' @param ident path, resource or entity ID of the picture
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @references ics1141
#' @export
getGraphics <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="") {
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = T,caption = caption,description=description,folderName = "graphics",func=getDesc)
  )
}

#' Includes a picture in rMarkdown, creates a string that has to be outputted
#'
#' @param ident path, resource or entity ID of the picture
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param includeCaption display the caption
#' @param includeDescription display the description
#' @references ics1141
#' @export

showGraphics <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",includeCaption=T,includeDescription=T) {
  graphicsObject <- getGraphics(ident=ident,from = from,addAsLink = addAsLink,caption = caption,description=description)
  if (is.null(graphicsObject$path)) {
    if (length(graphicsObject)==0) {
      logging::logerror("No picture found")
      logging::logerror(ident)
      return()
    }
    figString <-""
    for (i in 1:length(graphicsObject)) {
      figString <- paste(figString,
                         showGraphics(graphicsObject[i][[1]]$resource,addAsLink = addAsLink,caption = caption,description=description),
                         sep="\n\n")
    }
    return(figString)
  }  else {

    if (!includeCaption) {
      caption <- ""
    } else {
      caption <- graphicsObject$caption
    }
    if (!includeDescription) {
      description <- ""
    } else {
      description <- graphicsObject$description
    }

    fig <- paste0(
      "![",
      caption,
      "](",
      normalizePath(graphicsObject$path,winslash = "/"),
      " '",
      description,
      "')"
    )
    return(fig)
  }
}

#' Includes a picture in rMarkdown, with knitr::includeGraphics, used within a code block, only for a single image
#'
#' @param ident path, resource or entity ID of the picture
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastmodified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param includeCaption display the caption
#' @param includeDescription display the description
#' @param ... additional options for knitr::includeGraphics
#' @references ics1141
#' @export

includeGraphics <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",includeCaption=T,includeDescription=T,...) {
  graphicsObject <- getGraphics(ident=ident,from = from,addAsLink = addAsLink,caption = caption,description=description)
  if (is.null(graphicsObject$path)) {
    logging::logerror("No or more than one pictures found")
    logging::logerror(ident)
    return()
  }
  gg <- knitr::include_graphics(graphicsObject$path,...)
  if (includeCaption) {
    .img.cap = function(options,alt) {
      graphicsObject$caption
    }
    utils::assignInNamespace(".img.cap", .img.cap, ns="knitr")
  } else {
    .img.cap = function(options,alt) {
      ""
    }
    utils::assignInNamespace(".img.cap", .img.cap, ns="knitr")
  }
  if (includeDescription) {
    cat(graphicsObject$description)
    cat("\n\n")
  }
  return(gg)
}
