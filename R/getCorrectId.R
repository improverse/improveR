
#COMMENT
## CHANGED & to &&: & evaluates all conditions even if e.g. first one fails; && stops at the first failure

#' getCorrectId
#'
#' Resolves various ident types to an ID.
#' * a resource => resource ID
#' * a resource ID => resource ID
#' * a resource versionID => resource version ID
#' * a long entity ID with http: ... => short entity ID
#' * a short entity ID => short entity ID
#' * an entity ID without prefix => short entity ID
#' @param resolveToId 
#' Value which should be resolved to an ID. Accepts a resource ID, resource version ID, long and short entity ID, and an entity 
#' without prefix.
#' @examples 
#' \dontrun{
#' getCorrectId("112EE78F4CDC4400836F8C059AF2EA5F") #resourceId
#' getCorrectId("02C347E7439942FE834C7714F49EF082") #resourceVersionId
#' getCorrectId("http://host/provide?resourceId=resourceId=my_server:ST-63657") #long entity id
#' getCorrectId("my_server:ST-63657") #short entity id
#' }
#' @references ics1087
#' @noRd

getCorrectId <- function(resolveToId) {
    if (is.data.frame(resolveToId)) {
        if ("resourceId" %in% names(resolveToId)) {
            return(resolveToId$resourceId)
        } else {
            log_error("no resourceId contained in data frame")
            log_error(resolveToId)
            stop("missing resourceId")
        }
    } else if (startsWith(resolveToId, "/") | startsWith(resolveToId, "./") | startsWith(resolveToId, "\\") | startsWith(resolveToId, ".\\")) {
        return(resolveToId)
    } else if (!grepl("=", resolveToId, fixed = T) &&
        !grepl(":", resolveToId, fixed = T) &&
        !grepl("-", resolveToId, fixed = T)) {
        return(resolveToId)
    } else if (grepl("=", resolveToId, fixed = T)) {
        identifier <- gsub(".*=", "", resolveToId)
        # Convert hostname-based entity IDs to short format
        # e.g., "envhost1.example.com-4310:ST-12345" → "hc4310:ST-12345"
        idParts <- strsplit(identifier, ":")[[1]]
        if (length(idParts) == 2 && grepl("\\.", idParts[1])) {
            identifier <- paste0(repoPrefix(), idParts[2])
        }
        return(identifier)
    } else if (!grepl(":", resolveToId, fixed = T)) {
        return(paste0(repoPrefix(), resolveToId))
    } else {
        return(resolveToId)
    }
}
