#' Create Link to Resource
#'
#' Creates a symbolic link to an existing resource within a container (folder or workflow).
#' Links provide lightweight references to resources without duplicating content, enabling
#' resource reuse across workflows while maintaining a single source of truth. Changes to
#' the linked resource are automatically reflected wherever the link is used.
#'
#' @param linkContainer Identifier of the folder or workflow where the link will be created.
#'   Can be a path, resource id, or entity id.
#' @param links Identifier(s) of the resource(s) to link to. Can be a single resource or
#'   multiple resources. Can be a path, resource id, or entity id.
#' @param linkName Character. Optional name for the created link. If not provided (empty string),
#'   the link uses the name of the target resource. Only applicable when creating a single link;
#'   ignored when creating multiple links.
#'
#' @return The created link resource as a data frame (single link), or a list of link resources
#'   (multiple links). Returns \code{NULL} if the operation fails (e.g., target doesn't exist,
#'   link already exists, invalid node type for link creation).
#'
#' @details
#' Links differ from copies in that they reference the target resource rather than duplicating it.
#' The link always points to the latest version of the target resource. This is useful for:
#' \itemize{
#'   \item Sharing datasets across multiple workflow steps
#'   \item Referencing common templates or configuration files
#'   \item Organizing resources without duplication
#' }
#'
#' The function performs validation to ensure:
#' \itemize{
#'   \item Both target container and link source exist
#'   \item Target container accepts links (not all node types support links)
#'   \item No naming conflicts exist in the target container
#'   \item Repository is in editable mode (automatically checked via \code{improveEditable()})
#' }
#'
#' After creating links, related caches are automatically cleared to ensure fresh data
#' on subsequent queries.
#'
#' @seealso
#' \code{\link{copy}} for duplicating resources instead of linking,
#' \code{\link{createExternalLink}} for creating external URL references,
#' \code{\link{loadReferences}} to view all references from a resource
#'
#' @examples
#' \dontrun{
#' # Create a link to a dataset in a workflow step
#' createLink(
#'   linkContainer = "/Workflows/Analysis/Step 1",
#'   links = "/Data/master_dataset.csv"
#' )
#'
#' # Create link with custom name
#' createLink(
#'   linkContainer = "/Workflows/Analysis/Step 2",
#'   links = "/Data/master_dataset.csv",
#'   linkName = "input_data"
#' )
#'
#' # Create multiple links at once
#' createLink(
#'   linkContainer = "/Workflows/Modeling",
#'   links = c("/Data/dataset1.csv", "/Data/dataset2.csv")
#' )
#' }
#'
#' @references ics1138
#' @export
createLink <- function(linkContainer,links,linkName="") {
  improveEditable()
  if (!is.character(linkName)) {
    log_warn("name needs to be of type character")
    return(NULL)
  }
  if (linkName =="") {
    linkName<-NULL
  }
  target <- loadResource(linkContainer)
  if (is.null(target)) {
    log_warn("Target does not exist")
    return(NULL)
  }
  links <- loadResource(links)
  if (is.null(links)) {
    log_warn("LinkTarget does not exist")
    return(NULL)
  }
  # If the caller passed an existing Link as the resource to link (e.g.
  # iv_link_tree iterating children of a folder and re-linking each child
  # without distinguishing between Files and Links it encounters),
  # dereference to the underlying target. Otherwise the server creates a
  # Link -> Link -> real-resource chain, which inflates the inventory
  # graph and breaks if the intermediate Link is later moved or removed.
  # iv_link_tree already resolves on the client side; doing it here means
  # callers do not have to. The field on a loaded Link is targetEntityId
  # (see e.g. createStepTemplateEnv.R:538, getStep.R:399, exportFolder.R:291,
  # exportImportUtils.R:983 — the convention across the package).
  if (!is.null(links$nodeType) &&
      nrow(links) == 1 &&
      links$nodeType == "Link" &&
      "targetEntityId" %in% colnames(links) &&
      !is.na(links$targetEntityId) &&
      nchar(as.character(links$targetEntityId)) > 0) {
    resolved <- loadResource(links$targetEntityId)
    if (!is.null(resolved)) {
      links <- resolved
    }
  }
  if (nrow(target)>1) {
    return(Map(function(targ) {
      return(createLink(targ,links,linkName))
    },target$path))
  } else if (nrow(target)==1) {
    if (nrow(links)>1) {
      return(Map(function(lin) {
        return(createLink(target,lin,NULL))
      },links))
    } else {
      targetChildren <- loadChildResources(target)$data[[1]]

      #TODO more checks
      if (is.null(linkName)) {
        linkName<-links$name
      }
      if (!isAllowedTarget(target$nodeType,"Link",logWarning=T)) {
        return(NULL)
      }
      exists <- targetChildren[targetChildren$name==linkName,]



      if (nrow(exists)==1) {
        if (exists$nodeType=="Link") {
          log_info(linkName,"already exists in",target$path)
          return(loadResource(exists))
        }
        log_warn(linkName,"already exists in",target$path,"but is of type",exists$nodeType)
        return(NULL)
      }
      data<-list(name=linkName,targetId=target$resourceId)
      result <- authenticatedREST('/resources/{resourceId}/references',
                                                urlParams = list(resourceId=links$resourceId),
                                                data=data,
                                                restType = "POST")

      # Invalidate before the path-based loadResource: it walks via
      # loadChildResources which is cached, and a stale cache (without the
      # just-created link) makes the lookup return NULL. Once the POST
      # response shape is verified (the new link's resourceId), this should
      # switch to loadResource(parsedResult$resourceId) + warm-cache append
      # like createGeneric does, but the safe-correctness path here is to
      # nuke the cache and let the next load fetch fresh.
      unloadChildResources(target)
      unloadFullChildResources(target)
      unloadReferences(links)
      res <- loadResource(paste0("./",linkName),from = target)
      return(res)
    }
  } else {
    log_warn(paste0(linkContainer," does not exist"))
  }

}



