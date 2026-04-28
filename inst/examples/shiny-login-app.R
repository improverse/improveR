library(shiny)
library(improveR)

# ============================================================================
# Configuration
# Set REPO_URL to your improve repository (not /repository/repository, just /repository)
# CLIENT_ID must allow device_code in the Keycloak configuration
# ============================================================================
REPO_URL <- "http://envhost2.hc.scintecodev.internal:14118/repository"
CLIENT_ID <- "improve-api-client"

# Set to TRUE to see full log output
verbose <- FALSE

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  titlePanel("OAuth Login"),

  mainPanel(
    uiOutput("login_ui"),
    verbatimTextOutput("log_output")
  )
)

# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {

  repo_url <- REPO_URL

  state <- reactiveValues(
    started = FALSE,
    is_connected = FALSE,
    auth_url = NULL,
    auth_provider = NULL,
    error = NULL,
    log = character(),
    polling = FALSE
  )

  addLog <- function(msg) {
    logMsg <- paste(Sys.time(), "-", msg)
    cat(logMsg, "\n")
    state$log <- c(state$log, logMsg)
  }

  # Start OAuth automatically when app loads
  observe({
    req(!state$started)

    state$started <- TRUE
    state$error <- NULL

    addLog("Starting OAuth login process...")
    addLog(paste("repo_url =", repo_url))

    tryCatch({
      # Use improveR internal OAuth functions
      authProvider <- improveR:::getAuthenticationProvider(repo_url)
      authProvider$clientId <- CLIENT_ID
      addLog("Auth provider received")

      authProvider <- improveR:::startOAuth(authProvider, withCodeVerifier = TRUE)
      addLog("Device code received")

      state$auth_provider <- authProvider
      state$auth_url <- authProvider$verification_uri_complete
      addLog(paste("Verification URL:", state$auth_url))

    }, error = function(e) {
      state$error <- paste("OAuth initialization failed:", e$message)
      addLog(paste("ERROR:", e$message))
    })
  })

  # Start polling when user clicks the auth link
  observeEvent(input$link_clicked, {
    state$polling <- TRUE
    addLog("Link clicked - starting to poll for authentication...")
  })

  # Poll for authentication after link is clicked
  observe({
    req(state$polling)
    req(!state$is_connected)
    req(!is.null(state$auth_provider))

    invalidateLater(5000, session)

    addLog("Polling for token...")

    tryCatch({
      pollResult <- improveR:::hasAuthenticated(state$auth_provider)
      addLog(paste("Poll response status:", pollResult$status_code))

      if (pollResult$status_code == 200) {
        addLog("Authentication successful!")
        tokenContent <- httr::content(pollResult)

        user <- jose::jwt_split(tokenContent$id_token)$payload$preferred_username

        # Set environment variables for improveConnect
        Sys.setenv(IMPROVER_REPO_URL = repo_url)
        Sys.setenv(IMPROVER_USER = user)
        Sys.setenv(IMPROVER_TOKEN = tokenContent$access_token)
        Sys.setenv(IMPROVER_REFRESH_TOKEN = tokenContent$refresh_token)
        Sys.setenv(IMPROVER_TOKEN_EXPIRATION = tokenContent$expires_in)
        Sys.setenv(IMPROVER_LAST_ACCESS = as.numeric(Sys.time()))

        addLog(paste("Logged in as:", user))
        state$is_connected <- TRUE
        state$polling <- FALSE

        # Connect to improve — all improveR functions now work
        improveR::improveConnect()
        addLog("improveConnect successful")
      }
    }, error = function(e) {
      addLog(paste("Poll error:", e$message))
    })
  })

  # Render login UI
  output$login_ui <- renderUI({
    if (!is.null(state$error)) {
      tags$div(
        style = "color: red; font-weight: bold;",
        state$error
      )
    } else if (state$is_connected) {
      tags$div(
        tags$div(
          style = "color: green; font-size: 24px; font-weight: bold;",
          "Connected to improve"
        ),
        tags$p(paste("User:", Sys.getenv("IMPROVER_USER")))
      )
    } else if (state$started && !is.null(state$auth_url)) {
      tags$div(
        tags$p("Click the link to authenticate:"),
        tags$a(
          href = state$auth_url,
          target = "_blank",
          style = "font-size: 16px;",
          onclick = "Shiny.setInputValue('link_clicked', Math.random()); return true;",
          state$auth_url
        ),
        tags$p(
          id = "status_msg",
          style = "margin-top: 20px; color: gray;",
          ""
        )
      )
    } else if (repo_url == "") {
      tags$div(
        tags$p(style = "color: red;", "IMPROVER_REPO_URL not set!"),
        tags$p("Please set IMPROVER_REPO_URL in .Renviron")
      )
    } else {
      tags$p("Initializing OAuth...")
    }
  })

  output$log_output <- renderText({
    req(verbose)
    paste(state$log, collapse = "\n")
  })
}

shinyApp(ui = ui, server = server)
