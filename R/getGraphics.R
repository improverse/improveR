#' Get Graphics File from improve Repository
#'
#' Retrieves graphics files (images, plots, figures) from the improve repository
#' and returns metadata including the local file path. The graphics file is
#' downloaded and cached locally for use in reports, visualizations, or further
#' processing.
#'
#' @param ident Path, resource ID, or entity ID of the graphics file. Can be a
#'   relative path (from current step), absolute path, or improve identifier
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}
#'   (current step location)
#' @param addAsLink Logical. If \code{TRUE}, creates a link in the inventory for
#'   provenance tracking. Use \code{improveClean()} at workflow end to clean up
#'   links
#' @param caption Custom caption text. If empty, defaults to entityID and
#'   lastModified timestamp
#' @param description Custom description text. If empty, defaults to filename
#'
#' @returns A data frame with the following columns:
#'   \describe{
#'     \item{caption}{Character. Source information including entity version ID
#'       and last modified timestamp}
#'     \item{path}{Character. Local file system path to the downloaded graphics
#'       file}
#'     \item{entityId}{Character. Entity identifier on improve server}
#'     \item{name}{Character. Filename of the graphics file}
#'     \item{description}{Character. Description text (defaults to filename)}
#'     \item{resource}{List. Complete resource metadata from improve server
#'       (29 fields)}
#'   }
#'
#' @details
#' Graphics files are cached locally in the \code{graphics/} subdirectory of the
#' current working directory. The file is downloaded but not loaded into R - use
#' the returned \code{path} with appropriate display functions.
#'
#' Common workflows:
#' \itemize{
#'   \item For RMarkdown: Use \code{showGraphics()} to generate markdown syntax
#'   \item For knitr chunks: Use \code{includeGraphics()} with knitr integration
#'   \item For custom display: Use the \code{path} column with graphics functions
#'   \item For metadata only: Access \code{entityId}, \code{name}, etc.
#' }
#'
#' @examples
#' \dontrun{
#' # Get graphics file metadata
#' plot_info <- getGraphics("results/efficacy_plot.png")
#' plot_path <- plot_info$path
#'
#' # Display in RMarkdown using helper function
#' showGraphics("results/efficacy_plot.png")
#'
#' # Display in knitr chunk
#' includeGraphics("results/efficacy_plot.png")
#'
#' # Custom display with base R
#' img_info <- getGraphics("figures/diagram.png")
#' # Use img_info$path with your preferred display method
#' }
#'
#' @seealso
#' \code{\link{showGraphics}} for RMarkdown display,
#' \code{\link{includeGraphics}} for knitr integration,
#' \code{\link{getData}} for loading data tables,
#' \code{\link{getR}} for R scripts
#'
#' @references ics1141
#' @export
getGraphics <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="") {
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = T,caption = caption,description=description,folderName = "graphics",func=getDesc)
  )
}

#' Generate Markdown Syntax for Graphics
#'
#' Creates the Markdown syntax required to display a graphics file in an
#' RMarkdown document. Can optionally include caption and description text.
#'
#' @param ident Path, resource ID, or entity ID of the graphics file. Can be a
#'   relative path (from current step), absolute path, or improve identifier
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}
#'   (current step location)
#' @param addAsLink Logical. If \code{TRUE}, creates a link in the inventory for
#'   provenance tracking. Use \code{improveClean()} at workflow end to clean up
#'   links
#' @param caption Custom caption text. If empty, defaults to entityID and
#'   lastModified timestamp. To suppress caption, set \code{includeCaption = FALSE}
#' @param description Custom description text. If empty, defaults to filename.
#'   To suppress description, set \code{includeDescription = FALSE}
#' @param includeCaption Logical. If \code{TRUE} (default), includes the caption
#'   in the generated Markdown
#' @param includeDescription Logical. If \code{TRUE} (default), includes the
#'   description in the generated Markdown (as title text)
#'
#' @returns A character string containing the formatted Markdown syntax:
#'   \code{![caption](path 'description')}
#'   Returns empty string if file not found.
#'
#' @seealso
#' \code{\link{getGraphics}} for underlying retrieval function,
#' \code{\link{includeGraphics}} for Knitr integration
#'
#' @references ics1141
#' @export

showGraphics <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",includeCaption=T,includeDescription=T) {
  graphicsObject <- getGraphics(ident=ident,from = from,addAsLink = addAsLink,caption = caption,description=description)
  if (is.null(graphicsObject$path)) {
    if (length(graphicsObject)==0) {
      log_error("No picture found")
      log_error(ident)
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

#' Include Graphics in Knitr Chunk
#'
#' wrapper around \code{knitr::include_graphics} that automatically handles
#' file retrieval from improve, caching, and metadata display. Best used within
#' RMarkdown code chunks.
#'
#' @param ident Path, resource ID, or entity ID of the graphics file. Can be a
#'   relative path (from current step), absolute path, or improve identifier
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}
#'   (current step location)
#' @param addAsLink Logical. If \code{TRUE}, creates a link in the inventory for
#'   provenance tracking. Use \code{improveClean()} at workflow end to clean up
#'   links
#' @param caption Custom caption text. If empty, defaults to entityID and
#'   lastModified timestamp
#' @param description Custom description text. If empty, defaults to filename
#' @param includeCaption Logical. If \code{TRUE} (default), sets the chunk option
#'   \code{fig.cap} to the caption text
#' @param includeDescription Logical. If \code{TRUE} (default), prints the
#'   description text below the image
#' @param ... Additional arguments passed to \code{knitr::include_graphics}
#'
#' @returns The result of \code{knitr::include_graphics}, which renders the image
#'   in the RMarkdown output.
#'
#' @seealso
#' \code{\link{getGraphics}} for underlying retrieval function,
#' \code{\link{showGraphics}} for raw Markdown syntax generation
#'
#' @references ics1141
#' @export

includeGraphics <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",includeCaption=T,includeDescription=T,...) {
  graphicsObject <- getGraphics(ident=ident,from = from,addAsLink = addAsLink,caption = caption,description=description)
  if (is.null(graphicsObject$path)) {
    log_error("No or more than one pictures found")
    log_error(ident)
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
