#' stores all cli related variables during the load time of the package
cliEnv <- new.env()

#' Filter OS Version
#'
#' Filters the possible folders to find the one matching the given OS key.
#'
#' @param osKey A string representing the OS key (e.g., "win32", "lnx", "macosx").
#' @param possibleFolders a list of folders that could contain the improve-cli script
#' @return The path to the script if found, otherwise NULL.
filterOsVersion <- function(osKey, possibleFolders) {
  folder <- possibleFolders[grepl(osKey, possibleFolders)]
  if (length(folder) == 1) {
    script <- dir(folder, full.names = TRUE, pattern = "improve-cli")
    if (length(script) == 1) {
      return(script)
    }
  }
}

#' Unpack OS Version
#'
#' Unpacks the CLI files for the specified OS key.
#'
#' @param osKey A string representing the OS key (e.g., "win32", "lnx", "macosx").
#' @param possibleFiles a list of zip files
unpackOsVersion <- function(osKey, possibleFiles) {
  dirPath <- file.path(system.file(".", package = "improveR"), paste0("improve-cli-", osKey))
  dir.create(dirPath)
  zipFile <- possibleFiles[grepl(osKey, possibleFiles)]
  zip::unzip(zipFile, exdir = dirPath)
}

#' Get CLI Path
#'
#' Determines the path to the CLI executable, unpacking it if necessary.
#'
#' @param unpack Logical. Whether to unpack the CLI files if not found.
#' @return The path to the CLI executable.
cliPath <- function(unpack = TRUE) {
  if (!is.null(cliEnv$cliPath)) {
    return(cliEnv$cliPath)
  }
  os <- Sys.info()['sysname']
  possibleAll <- dir(system.file(".", package = "improveR"), pattern = "improve-cli", full.names = TRUE, include.dirs = TRUE)
  possibleFolders <- Filter(dir.exists, possibleAll)
  script <- NULL
  if (length(possibleFolders) > 0) {
    if (os == "Windows") {
      script <- filterOsVersion("win32", possibleFolders)
    } else if (os == "Linux") {
      script <- filterOsVersion("lnx", possibleFolders)
    } else {
      script <- filterOsVersion("macosx", possibleFolders)
    }
  }
  if (!is.null(script)) {
    cliEnv$cliPath <- script
    return(script)
  }

  if (unpack) {
    possibleFiles <- possibleAll[!(possibleAll %in% possibleFolders)]

    if (os == "Windows") {
      unpackOsVersion("win32", possibleFiles)
    } else if (os == "Linux") {
      unpackOsVersion("lnx", possibleFiles)
    } else {
      unpackOsVersion("macosx", possibleFiles)
    }
    return(cliPath(FALSE))
  }
}

#' Execute CLI Command
#'
#' Executes a CLI command using the appropriate CLI executable.
#'
#' @param cliString A string representing the CLI command to execute.
executeCli <- function(cliString) {
  shellFile <- cliPath()
  result <- system(paste(shellFile, cliString))
  print(result)
}

#' Get CICO API URL
#'
#' Retrieves1 the API URL from the configuration.
#'
#' @return The API URL as a string.
getCICOApiURL <- function() {
  apiUrl <- conf()$repoUrl
  apiUrl <- substr(apiUrl, 0, nchar(apiUrl) - 4)
  return(apiUrl)
}

#' Check Initialization
#'
#' Ensures that the user profile is configured.
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
readLocalRepoInfo <- function(localPath) {
  propPath <- normalizePath(
    file.path(localPath, ".improve/repository.properties")
  )
  localRepoInfo <- properties::read.properties(propPath)
}
