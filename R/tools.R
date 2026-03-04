

toolCategoriesCacheList <- list(
  toolCategoriesCache=defaultKey
)




actualToolCategories <- function(...) {
  return(restGetAsDf("configuration/toolCategories"))
}


#' loads all registered tool categories
#' @references ics1229
#' @noRd
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

#' updateToolCategories reloads the tool categories from the repository
#' @references ics1229
#' @noRd
updateToolCategories <- function() {
  unloadToolCategories()
  res <- loadToolCategories()
  return(res)
}

##############################TOOLS

toolsCacheList <- list(
  toolsCache="categoryId"
)

#' loadToolsForCategory
#'
#' @param categoryId categoryId of the tools
#' @references ics1230
#' @noRd
loadToolsForCategory <- function(categoryId) {
  catgoryTools <- getFromCache(categoryId,actualLoadToolsForCategory,toolsCacheList,NULL)
  return(catgoryTools)
}


actualLoadToolsForCategory <- function(categoryId) {
  toolsDf <- restGetAsDf("configuration/toolCategories/{id}/tools",
                          urlParams = list(id = categoryId))
  if (is.null(toolsDf)) {
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

#' updateToolsForCategory reloads the category tools from the repository
#' @param categoryId categoryId of the category
#' @references ics1230
#' @noRd
updateToolsForCategory <- function(categoryId) {
  unloadToolsForCategory(categoryId)
  res <- loadToolsForCategory(categoryId)
  return(res)
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

