
#' Apply Function Row-Wise With Null Protection
#'
#' Applies a function to each row of a data frame, with automatic handling of NULL
#' or empty inputs. This is a safer alternative to \code{base::by} that returns \code{NULL}
#' instead of throwing errors when given invalid or empty data frames.
#'
#' @param df Data frame to iterate over. If \code{NULL}, not a data frame, or has zero rows,
#'   the function returns \code{NULL} without attempting iteration.
#' @param func Function to apply to each row. The function receives one argument: a
#'   single-row data frame representing the current row. Should return any value.
#'
#' @return A \code{by} object containing the results from applying \code{func} to each row,
#'   or \code{NULL} if the input data frame is empty, \code{NULL}, or invalid. The \code{by}
#'   object can be converted to a list using \code{as.list()} or further processed.
#'
#' @details
#' This function wraps \code{base::by()} with null-safety checks, making it suitable for
#' use in pipelines where data frames may be empty or NULL. It's commonly used throughout
#' improveR for iterating over resource lists, child resources, and query results where
#' the number of results may be zero.
#'
#' The function is applied to each row independently, with rows identified by
#' \code{seq_len(nrow(df))}. If you need the results automatically converted back to a
#' data frame, use \code{\link{byNotEmptyAsDf}} instead.
#'
#' @seealso
#' \code{\link{byNotEmptyAsDf}} for automatic conversion of results to data frame,
#' \code{\link[base]{by}} for the underlying iteration mechanism
#'
#' @examples
#' \dontrun{
#' # Safely iterate over resources
#' resources <- loadChildResources("/Data")$data[[1]]
#' byNotEmpty(resources, function(row) {
#'   cat("Processing:", row$name, "\n")
#'   # Returns NULL for empty result sets
#' })
#'
#' # Extract specific fields from each row
#' result <- byNotEmpty(resources, function(row) {
#'   list(name = row$name, type = row$nodeType)
#' })
#' }
#'
#' @export
byNotEmpty <- function(df,func) {
  if (!is.data.frame(df) || nrow(df)==0) {
    return(NULL)
  }
  return(
    by(df,seq_len(nrow(df)),func)
  )
}

#' Apply Function Row-Wise and Merge Results to Data Frame
#'
#' Applies a function to each row of a data frame and automatically merges the results
#' back into a single data frame. This is a convenience wrapper around \code{\link{byNotEmpty}}
#' with automatic result consolidation, handling NULL or empty inputs gracefully.
#'
#' @param df Data frame to iterate over. If \code{NULL}, not a data frame, or has zero rows,
#'   the function returns \code{NULL} without attempting iteration.
#' @param func Function to apply to each row. The function receives one argument: a
#'   single-row data frame representing the current row. Should return a single-row data frame,
#'   a named list, or another structure that can be coerced to a data frame row.
#'
#' @return A data frame containing the merged results from all row applications, or \code{NULL}
#'   if the input data frame is empty, \code{NULL}, or invalid. Rows from individual function
#'   calls are combined using \code{mergeDataframeList()}.
#'
#' @details
#' This function combines \code{\link{byNotEmpty}} with automatic data frame consolidation,
#' making it ideal for transforming data frames row-by-row when each transformation produces
#' a data frame row as output.
#'
#' Common use cases include:
#' \itemize{
#'   \item Enriching each row with additional data from API calls
#'   \item Expanding rows that contain nested data structures
#'   \item Computing row-wise aggregations that return structured results
#' }
#'
#' The function internally uses \code{mergeDataframeList()} to intelligently combine
#' heterogeneous row results, handling varying column sets gracefully.
#'
#' @seealso
#' \code{\link{byNotEmpty}} for row-wise iteration without automatic merging,
#' \code{\link[base]{by}} for the underlying iteration mechanism
#'
#' @examples
#' \dontrun{
#' # Transform each resource row
#' steps <- loadChildSteps("/Workflow")$data[[1]]
#' enriched <- byNotEmptyAsDf(steps, function(row) {
#'   data.frame(
#'     stepName = row$name,
#'     status = row$runStatus,
#'     isFinished = row$runStatus == "FINISHED"
#'   )
#' })
#'
#' # Expand nested structures
#' byNotEmptyAsDf(parentData, function(row) {
#'   # Load children for this parent
#'   children <- loadChildResources(row$resourceId)$data[[1]]
#'   # Return summary
#'   data.frame(parent = row$name, childCount = nrow(children))
#' })
#' }
#'
#' @export
byNotEmptyAsDf <- function(df,func) {
  if (!is.data.frame(df) || nrow(df)==0) {
    return(NULL)
  }
  return(
    mergeDataframeList(
      by(df,seq_len(nrow(df)),func)
    )
  )
}
