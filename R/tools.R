

toolCategoriesCacheList <- list(
  toolCategoriesCache=defaultKey
)




actualToolCategories <- function(...) {
  return(restGetAsDf("configuration/toolCategories"))
}


#' Load Tool Categories
#'
#' Retrieves all registered tool categories from the improve repository.
#'
#' @returns A data frame of tool categories with columns such as id, name, identifier.
#'   Returns \code{NULL} if no categories exist.
#' @seealso \code{\link{createToolCategory}}, \code{\link{loadToolsForCategory}}
#' @references ics1229
#' @export
loadToolCategories <- function() {
  categories <- getFromCache(defaultKey,actualToolCategories,toolCategoriesCacheList,NULL)
  return(categories)
}


#' unloadToolCategories
#' @references ics1229
#' @noRd
unloadToolCategories <- function() {
  loadToolCategories()
  removeFromCache(defaultKey,"",toolCategoriesCacheList)
}

#' refreshToolCategories reloads the tool categories from the repository
#' @references ics1229
#' @noRd
refreshToolCategories <- function() {
  unloadToolCategories()
  res <- loadToolCategories()
  return(res)
}

#' @rdname refreshToolCategories
#' @noRd
updateToolCategories <- function(...) {
  .Deprecated("refreshToolCategories")
  refreshToolCategories(...)
}

##############################TOOLS

toolsCacheList <- list(
  toolsCache="categoryId"
)

#' Load Tools for a Category
#'
#' Retrieves all tools registered under a specific tool category.
#'
#' @param categoryId Character. The ID of the tool category.
#' @returns A data frame of tools with columns such as id, name, categoryId.
#'   Returns \code{NULL} if no tools exist in the category.
#' @seealso \code{\link{loadToolCategories}}, \code{\link{createTool}}
#' @references ics1230
#' @export
loadToolsForCategory <- function(categoryId) {
  catgoryTools <- getFromCache(categoryId,actualLoadToolsForCategory,toolsCacheList,NULL)
  return(catgoryTools)
}


actualLoadToolsForCategory <- function(categoryId) {
  toolsDf <- restGetAsDf("configuration/toolCategories/{id}/tools",
                          urlParams = list(id = categoryId))
  if (is.null(toolsDf) || nrow(toolsDf) == 0) {
    log_warn("no tools found for category:", categoryId)
    return(NULL)
  }
  toolsDf$categoryId <- categoryId
  return(toolsDf)
}


#' unloadToolsForCategory
#' @param categoryId categoryId of the tools
#' @references ics1230
#' @noRd
unloadToolsForCategory <- function(categoryId) {
  loadToolsForCategory(categoryId)
  removeFromCache(categoryId,"",runserverToolsCacheList)
}

#' refreshToolsForCategory reloads the category tools from the repository
#' @param categoryId categoryId of the category
#' @references ics1230
#' @noRd
refreshToolsForCategory <- function(categoryId) {
  unloadToolsForCategory(categoryId)
  res <- loadToolsForCategory(categoryId)
  return(res)
}

#' @rdname refreshToolsForCategory
#' @noRd
updateToolsForCategory <- function(...) {
  .Deprecated("refreshToolsForCategory")
  refreshToolsForCategory(...)
}

#' loads all registered tools with their categories
#' @references ics1230
#' @noRd
loadAllTools <- function() {
  categories <- loadToolCategories()
  allTools <- NULL
  if (nrow(categories)>0) {
    for (i in 1:nrow(categories)) {
      category <- categories[i,]
      tools <- loadToolsForCategory(category$id)
      if (!is.null(tools) && nrow(tools)>0) {
        tools$categoryName <- category$name
        tools$categoryIdentifier <- category$identifier
        if (is.null(allTools)) {
          allTools<- tools
        } else {
          allTools <- plyr::rbind.fill(allTools,tools)
        }
      }
    }
  }
  return(allTools)
}

