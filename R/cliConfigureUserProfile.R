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
  refrToken = Sys.getenv("IMPROVER_REFRESH_TOKEN")
  #command <- glue::glue("userProfile oauth2 devicecode -userProfile {userProfile} -refreshToken {refrToken}")
  command <- glue::glue("userProfile oauth2 devicecode -userProfile {userProfile}")
  executeCli(command)
  cliEnv$userProfile <- userProfile
}
