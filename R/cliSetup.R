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

#' Execute CLI Command
#'
#' Executes a CLI command using the appropriate CLI executable.
#'
#' @param cliString A string representing the CLI command to execute.
#' @noRd
executeCli <- function(cliString) {
  shellFile <- cliPath()

  result <- system(paste(shellFile,cliString))
  print(result)
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
#' Configures the user profile for CLI usage.
#'
#' @param userProfile A string representing the user profile name. Default is "improveR".
#' @noRd
configureUserProfile <- function(userProfile = "improveR") {
  apiURL <- getCICOApiURL()
  command <- glue::glue("userProfile configure -userProfile {userProfile} -apiURL {apiURL}")
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
