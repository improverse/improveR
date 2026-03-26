#' Get HTML File from improve Repository
#'
#' Retrieves HTML files (reports, widgets, interactive visualizations) from the
#' improve repository and returns metadata including the local file path. The HTML
#' file is downloaded and cached locally for embedding in RMarkdown reports or
#' viewing in browsers.
#'
#' @param ident Path, resource ID, or entity ID of the HTML file. Can be a
#'   relative path (from current step), absolute path, or improve identifier
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}
#'   (current step location)
#' @param addAsLink Logical. If \code{TRUE}, creates a link in the inventory for
#'   provenance tracking. Use \code{improveClean()} at workflow end to clean up
#'   links
#' @param caption Custom caption text. If empty, defaults to entityID and
#'   lastModified timestamp
#'
#' @returns A data frame with the following columns:
#'   \describe{
#'     \item{caption}{Character. Source information including entity version ID
#'       and last modified timestamp}
#'     \item{path}{Character. Local file system path to the downloaded HTML file}
#'     \item{entityId}{Character. Entity identifier on improve server}
#'     \item{name}{Character. Filename of the HTML file}
#'     \item{description}{Character. Description text (defaults to filename)}
#'     \item{resource}{List. Complete resource metadata from improve server
#'       (29 fields)}
#'   }
#'
#' @details
#' HTML files are cached locally in the \code{HTML/} subdirectory of the current
#' working directory. Common use cases include embedding interactive reports,
#' htmlwidgets output (plotly, leaflet, DT tables), or standalone HTML
#' visualizations in RMarkdown documents.
#'
#' Common workflows:
#' \itemize{
#'   \item For RMarkdown embedding: Use \code{showHTML()} to include HTML inline
#'   \item For browser viewing: Use the \code{path} column to open HTML file
#'   \item For htmltools integration: Pass \code{path} to \code{htmltools::includeHTML()}
#'   \item For metadata only: Access \code{entityId}, \code{name}, etc.
#' }
#'
#' @examples
#' \dontrun{
#' # Get HTML file metadata
#' report_info <- getHTML("results/interactive_plot.html")
#' report_path <- report_info$path
#'
#' # Embed in RMarkdown using helper function
#' showHTML("results/interactive_plot.html")
#'
#' # View in browser
#' html_info <- getHTML("reports/summary.html")
#' browseURL(html_info$path)
#'
#' # Use with htmltools
#' widget_info <- getHTML("widgets/leaflet_map.html")
#' htmltools::includeHTML(widget_info$path)
#' }
#'
#' @seealso
#' \code{\link{showHTML}} for RMarkdown embedding,
#' \code{\link{getData}} for loading data tables,
#' \code{\link{getGraphics}} for graphics files,
#' \code{\link{getR}} for R scripts
#'
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
#' @param caption by default entityID and lastModified are the caption, here alternative text can be provided
#' @param includeCaption display the caption
#' @references ics1141
#' @export

showHTML <- function(ident,from=pwd(),addAsLink=TRUE,caption="",includeCaption=T) {
  graphicsObject <- getHTML(ident=ident,from = from,addAsLink = addAsLink,caption = caption)
  if (is.null(graphicsObject$path)) {
    if (length(graphicsObject)==0) {
      log_error("No HTML object found")
      log_error(ident)
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


