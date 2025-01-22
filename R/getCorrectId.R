#' getCorrectId
#'
#' Resolves various ident types to an ID.
#' * a resource => resource ID
#' * a resource ID => resource ID
#' * a resource versionID => resource version ID
#' * a long entity ID with http: ... => short entity ID
#' * a short entity ID => short entity ID
#' * an entity ID without prefix => short entity ID
#' @param resolveToId the value resolve to short entity ID, or resource ID, version possible for both
#' @examples 
#' \dontrun{
#' getCorrectId("112EE78F4CDC4400836F8C059AF2EA5F") #resourceId
#' getCorrectId("02C347E7439942FE834C7714F49EF082") #resourceVersionId
#' getCorrectId("http://host/provide?resourceId=resourceId=my_server:ST-63657") #long entity id
#' getCorrectId("my_server:ST-63657") #short entity id
#' }
#' @references ics1087
#' @export

getCorrectId <- function(resolveToId) {
    if (is.data.frame(resolveToId)) {
        if ("resourceId" %in% names(resolveToId)) {
            return(resolveToId$resourceId)
        } else {
            logging::logerror("no resourceId contained in dataframe")
            logging::logerror(resolveToId)
            stop("missing resourceId")
        }
    } else if (startsWith(resolveToId, "/") | startsWith(resolveToId, "./") | startsWith(resolveToId, "\\") | startsWith(resolveToId, ".\\")) {
        return(resolveToId)
    } else if (!grepl("=", resolveToId, fixed = T) &
        !grepl(":", resolveToId, fixed = T) &
        !grepl("-", resolveToId, fixed = T)) {
        return(resolveToId)
    } else if (grepl("=", resolveToId, fixed = T)) {
        identifier <- gsub(".*=", "", resolveToId)
        return(identifier)
    } else if (!grepl(":", resolveToId, fixed = T)) {
        return(paste0(repoPrefix(), resolveToId))
    } else {
        return(resolveToId) # nolint: indentation_linter.
    }
}
