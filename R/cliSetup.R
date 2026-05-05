#' stores all cli related variables during the load time of the package
#' @noRd
cliEnv <- new.env()



#' Get CLI Path
#'
#' Determines the path to the CLI executable, unpacking it if necessary.
#'
#' @param unpack Logical. Whether to unpack the CLI files if not found.
#' @return The path to the CLI executable.
#' @noRd
cliPath <- function(unpack = TRUE) {
  return(improveRcontributions::cliPath(unpack))
}

#' Detect CLI mode and version
#' @noRd
detectCli <- function() {
  path <- tryCatch(cliPath(), error = function(e) NULL)
  if (is.null(path)) {
    cliEnv$cliMode <- "none"
    cliEnv$cliVersion <- NULL
    return(invisible(NULL))
  }
  if (grepl("-jar", path, fixed = TRUE)) {
    cliEnv$cliMode <- "jar"
  } else {
    cliEnv$cliMode <- "legacy_binary"
  }
  # Try to get version
  ver <- tryCatch({
    out <- system(paste(path, "--version"), intern = TRUE, ignore.stderr = TRUE)
    m <- regmatches(out, regexpr("\\d+\\.\\d+\\.\\d+", out))
    if (length(m) > 0) m[1] else NULL
  }, error = function(e) NULL)
  cliEnv$cliVersion <- ver
  invisible(NULL)
}

#' Reset CLI detection state
#' @noRd
resetCliDetection <- function() {
  cliEnv$cliMode <- NULL
  cliEnv$cliVersion <- NULL
  # Also reset the cached path in improveRcontributions
  tryCatch(improveRcontributions::cliEnv$cliPath <- NULL, error = function(e) NULL)
}

#' Get CLI mode
#' @noRd
cliMode <- function() {
  if (is.null(cliEnv$cliMode)) detectCli()
  cliEnv$cliMode
}

#' Get detected CLI version
#' @noRd
cliDetectedVersion <- function() {
  if (is.null(cliEnv$cliMode)) detectCli()
  cliEnv$cliVersion
}

#' Check if picocli (new jar CLI) is available
#' @noRd
hasPicocli <- function() {
  cliMode() == "jar"
}

#' Get CLI profile name
#' @noRd
cliProfileName <- function() {
  checkInit()
  cliEnv$userProfile
}

#' Get CICO API URL
#'
#' Retrieves1 the API URL from the configuration.
#'
#' @return The API URL as a string.
#' @noRd
getCICOApiURL <- function() {
  apiUrl <- conf()$repoUrl
  apiUrl <- substr(apiUrl, 0, nchar(apiUrl) - 4)
  return(apiUrl)
}

#' Check Initialization
#'
#' Ensures that the user profile is configured.
#' @noRd
checkInit <- function() {
  if (is.null(cliEnv$userProfile)) {
    configureUserProfile()
  }
}

#' Configure User Profile
#'
#' Configures the user profile for CLI usage. The profile name is derived
#' from a hash of the API URL so that each repository gets its own profile,
#' avoiding conflicts when switching between servers.
#'
#' @param userProfile A string representing the user profile name. If NULL
#'   (default), a name is generated from the API URL hash.
#' @noRd
configureUserProfile <- function(userProfile = NULL) {
  apiURL <- getCICOApiURL()

  if (is.null(userProfile)) {
    urlHash <- substr(as.character(openssl::md5(apiURL)), 1, 8)
    userProfile <- paste0("improveR_", urlHash)
  }

  # The standalone jar requires -checkCertificates to be set explicitly
  checkCert <- if (!is.null(cacheEnv$secure) && cacheEnv$secure == FALSE) "false" else "true"
  checkCertificates <- paste0(" -checkCertificates ", checkCert)

  command <- glue::glue("userProfile configure -userProfile {userProfile} -apiURL {apiURL}{checkCertificates}")
  executeCli(command)
  command <- glue::glue("userProfile oauth2 devicecode -userProfile {userProfile}")
  executeCli(command)
  cliEnv$userProfile <- userProfile
}




#' Get Local Repository Resource
#'
#' Retrieves2 the resource information for a local repository.
#'
#' @param localPath The local repository path.
#' @return The resource information.
#' @noRd
getLocalRepoResource <- function(localPath) {
  localRepoInfo <- readLocalRepoInfo(localPath)
  resource <- loadResource(localRepoInfo$pull.root.resourceId)
  return(resource)
}

#' Read Local Repository Info
#'
#' Reads the repository properties file for a local repository.
#'
#' @param localPath The local repository path.
#' @return A list of repository properties.
#' @noRd
readLocalRepoInfo <- function(localPath) {
  propPath <- normalizePath(
    file.path(localPath, ".improve/repository.properties")
  )
  localRepoInfo <- properties::read.properties(propPath)
}
