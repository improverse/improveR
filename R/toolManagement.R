# Tool Management — CRUD for tool categories, tools, instances, and parameters
#
# These functions wrap the improve configuration REST endpoints for managing
# tools and runserver tool instances. They complement the read-only functions
# in tools.R and stepConfigData.R.

# ============================================================================
# Tool Categories
# ============================================================================

#' Create a Tool Category
#'
#' Creates a new tool category in the improve repository.
#'
#' @param name Character. Name for the new tool category.
#' @returns A list with the created category details (id, name), or \code{NULL} on failure.
#' @seealso \code{\link{loadToolCategories}}, \code{\link{deleteToolCategory}}, \code{\link{renameToolCategory}}
#' @references ccs32
#' @export
createToolCategory <- function(name) {
  improveEditable()
  result <- authenticatedREST(
    "configuration/toolCategories",
    data = list(name = name),
    restType = "POST")
  if (is.null(result)) {
    log_warn("createToolCategory: failed to create category:", name)
    return(NULL)
  }
  resetToolInstances()
  httr::content(result)
}

#' Rename a Tool Category
#'
#' Updates the name of an existing tool category.
#'
#' @param categoryId Character. The ID of the tool category to rename.
#' @param name Character. The new name.
#' @returns A list with the updated category details, or \code{NULL} on failure.
#' @seealso \code{\link{loadToolCategories}}, \code{\link{createToolCategory}}
#' @references ccs33
#' @export
renameToolCategory <- function(categoryId, name) {
  improveEditable()
  result <- authenticatedREST(
    "configuration/toolCategories/{id}",
    urlParams = list(id = categoryId),
    data = list(name = name),
    restType = "PUT")
  if (is.null(result)) {
    log_warn("renameToolCategory: failed to rename category:", categoryId)
    return(NULL)
  }
  resetToolInstances()
  httr::content(result)
}

#' Delete a Tool Category
#'
#' Deletes an existing tool category from the improve repository.
#'
#' @param categoryId Character. The ID of the tool category to delete.
#' @returns \code{TRUE} if deleted successfully, \code{FALSE} otherwise.
#' @seealso \code{\link{loadToolCategories}}, \code{\link{createToolCategory}}
#' @references ccs34
#' @export
deleteToolCategory <- function(categoryId) {
  improveEditable()
  result <- authenticatedREST(
    "configuration/toolCategories/{id}",
    urlParams = list(id = categoryId),
    restType = "DELETE")
  resetToolInstances()
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("deleteToolCategory: failed to delete category:", categoryId)
  return(FALSE)
}

# ============================================================================
# Tools (within a category)
# ============================================================================

#' Create a Tool in a Category
#'
#' Creates a new tool in the specified category. Idempotent: if a tool with
#' the given name already exists in the category, it is returned instead.
#'
#' @param categoryId Character. The ID of the tool category.
#' @param toolName Character. The name of the tool to create.
#' @returns A list with the tool details (id, name, categoryId), or \code{NULL} on failure.
#' @seealso \code{\link{loadToolsForCategory}}, \code{\link{createToolCategory}}
#' @references ccs35
#' @export
createTool <- function(categoryId, toolName) {
  improveEditable()

  # Check if tool already exists
  existing <- loadToolsForCategory(categoryId)
  if (!is.null(existing) && nrow(existing) > 0) {
    match <- existing[existing$name == toolName, ]
    if (nrow(match) > 0) {
      return(as.list(match[1, ]))
    }
  }

  result <- authenticatedREST(
    "configuration/toolCategories/{id}/tools",
    urlParams = list(id = categoryId),
    data = list(name = toolName),
    restType = "POST")
  if (is.null(result)) {
    log_warn("createTool: failed to create tool:", toolName, "in category:", categoryId)
    return(NULL)
  }
  resetToolInstances()
  httr::content(result)
}

# ============================================================================
# Tool Instances (on a runserver)
# ============================================================================

#' Create a Tool Instance on a Runserver
#'
#' Creates a new tool instance on the specified runserver. Idempotent: if an
#' instance with the same toolId and name already exists, it is returned.
#'
#' @param runserverId Character. The ID of the runserver.
#' @param toolId Character. The ID of the tool.
#' @param instanceName Character. The name for the tool instance.
#' @param command Character. The command string. Default \code{""}.
#' @param gridProvider Character. Grid provider type (e.g. "DOCKER"). Default \code{""}.
#' @param gridProviderInstance Character. Grid provider instance name. Default \code{""}.
#' @returns A list with the instance details (id, name, toolId, runserverId), or \code{NULL} on failure.
#' @seealso \code{\link{updateToolInstance}}, \code{\link{getToolInstances}}
#' @references ccs36
#' @export
createToolInstance <- function(runserverId, toolId, instanceName, command = "",
                               gridProvider = "", gridProviderInstance = "") {
  improveEditable()

  # Check if instance already exists
  res <- authenticatedREST(
    "configuration/runservers/{runserverId}/tools",
    urlParams = list(runserverId = runserverId),
    restType = "GET")
  if (!is.null(res)) {
    existing <- httr::content(res)
    for (inst in existing) {
      if (identical(inst$toolId, toolId) && identical(inst$name, instanceName)) {
        return(inst)
      }
    }
  }

  data <- list(toolId = toolId, name = instanceName, command = command)
  if (nzchar(gridProvider)) data$gridProvider <- gridProvider
  if (nzchar(gridProviderInstance)) data$gridProviderInstance <- gridProviderInstance

  result <- authenticatedREST(
    "configuration/runservers/{runserverId}/tools",
    urlParams = list(runserverId = runserverId),
    data = data,
    restType = "POST")
  if (is.null(result)) {
    log_warn("createToolInstance: failed to create instance:", instanceName,
             "on runserver:", runserverId)
    return(NULL)
  }
  resetToolInstances()
  httr::content(result)
}

#' Update a Tool Instance
#'
#' Updates the command of an existing tool instance on a runserver.
#'
#' @param runserverId Character. The ID of the runserver.
#' @param instanceId Character. The ID of the tool instance.
#' @param command Character. The new command string.
#' @returns A list with the updated instance details, or \code{NULL} on failure.
#' @seealso \code{\link{createToolInstance}}, \code{\link{getToolInstances}}
#' @references ccs37
#' @export
updateToolInstance <- function(runserverId, instanceId, command) {
  improveEditable()

  # Fetch existing instance to get full object for PUT
  existingRes <- authenticatedREST(
    "configuration/runservers/{runserverId}/tools",
    urlParams = list(runserverId = runserverId),
    restType = "GET")
  existing <- NULL
  if (!is.null(existingRes)) {
    for (inst in httr::content(existingRes)) {
      if (identical(inst$id, instanceId)) { existing <- inst; break }
    }
  }

  data <- if (!is.null(existing)) {
    existing$command <- command
    existing
  } else {
    list(id = instanceId, command = command)
  }

  result <- authenticatedREST(
    "configuration/runservers/{runserverId}/tools/{instanceId}",
    urlParams = list(runserverId = runserverId, instanceId = instanceId),
    data = data,
    restType = "PUT")
  if (is.null(result)) {
    log_warn("updateToolInstance: failed to update instance:", instanceId)
    return(NULL)
  }
  resetToolInstances()
  httr::content(result)
}

# ============================================================================
# Tool Parameters
# ============================================================================

#' Create a Tool Parameter on an Instance
#'
#' Creates a parameter on a tool instance. The parameter is identified by name
#' (e.g. "Tool Arguments") which is resolved to a parameter LOV ID. Idempotent:
#' if the parameter already exists, it is updated instead.
#'
#' @param runserverId Character. The ID of the runserver.
#' @param instanceId Character. The ID of the tool instance.
#' @param parameterName Character. The parameter name (from parameterLov, e.g. "Tool Arguments").
#' @param value Character. The parameter value.
#' @returns A list with the parameter details (id, parameterLovId, value), or \code{NULL} on failure.
#' @seealso \code{\link{updateToolParameter}}, \code{\link{createToolInstance}}
#' @references ccs38
#' @export
createToolParameter <- function(runserverId, instanceId, parameterName, value) {
  improveEditable()

  # Resolve parameter name to LOV ID
  params <- getParameterValues()
  if (is.null(params) || nrow(params) == 0) {
    log_warn("createToolParameter: could not load parameter LOV")
    return(NULL)
  }
  lovMatch <- params[params$name == parameterName, ]
  if (nrow(lovMatch) == 0) {
    log_warn("createToolParameter: parameter LOV not found:", parameterName)
    return(NULL)
  }
  lovId <- lovMatch$id[1]

  # Check existing parameters
  existingRes <- authenticatedREST(
    "configuration/runservers/{runserverId}/tools/{instanceId}/parameters",
    urlParams = list(runserverId = runserverId, instanceId = instanceId),
    restType = "GET")
  if (!is.null(existingRes)) {
    existing <- httr::content(existingRes)
    for (p in existing) {
      if (identical(p$parameterLovId, lovId)) {
        # Update existing parameter
        return(updateToolParameter(runserverId, instanceId, p$id, value))
      }
    }
  }

  result <- authenticatedREST(
    "configuration/runservers/{runserverId}/tools/{instanceId}/parameters",
    urlParams = list(runserverId = runserverId, instanceId = instanceId),
    data = list(parameterLovId = lovId, value = value),
    restType = "POST")
  if (is.null(result)) {
    log_warn("createToolParameter: failed to create parameter:", parameterName)
    return(NULL)
  }
  resetToolInstances()
  httr::content(result)
}

#' Update a Tool Parameter Value
#'
#' Updates the value of an existing parameter on a tool instance.
#'
#' @param runserverId Character. The ID of the runserver.
#' @param instanceId Character. The ID of the tool instance.
#' @param parameterId Character. The ID of the parameter to update.
#' @param value Character. The new parameter value.
#' @param parameterLovId Character or NULL. The parameter LOV ID. If NULL, the
#'   existing parameter is fetched to determine it.
#' @returns A list with the updated parameter details, or \code{NULL} on failure.
#' @seealso \code{\link{createToolParameter}}, \code{\link{createToolInstance}}
#' @references ccs39
#' @export
updateToolParameter <- function(runserverId, instanceId, parameterId, value,
                                parameterLovId = NULL) {
  improveEditable()

  # Resolve parameterLovId if not provided
  if (is.null(parameterLovId)) {
    existingRes <- authenticatedREST(
      "configuration/runservers/{runserverId}/tools/{instanceId}/parameters",
      urlParams = list(runserverId = runserverId, instanceId = instanceId),
      restType = "GET")
    if (!is.null(existingRes)) {
      for (p in httr::content(existingRes)) {
        if (identical(p$id, parameterId)) {
          parameterLovId <- p$parameterLovId
          break
        }
      }
    }
  }

  result <- authenticatedREST(
    "configuration/runservers/{runserverId}/tools/{instanceId}/parameters/{paramId}",
    urlParams = list(runserverId = runserverId, instanceId = instanceId, paramId = parameterId),
    data = list(id = parameterId, parameterLovId = parameterLovId, value = value),
    restType = "PUT")
  if (is.null(result)) {
    log_warn("updateToolParameter: failed to update parameter:", parameterId)
    return(NULL)
  }
  resetToolInstances()
  httr::content(result)
}
