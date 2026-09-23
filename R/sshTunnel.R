# ── SSH Tunnel Management ─────────────────────────────────────────────────────
#
# Provides SSH tunnel functionality for connecting to interactive steps
# (e.g. RStudio, Jupyter) running on improve runservers.
#
# Requires:
#   - openssl (Imports, for key generation)
#   - processx (Suggests, for SSH process management)
#   - ssh / ssh-keygen (system commands)
#

# Package-level environment for tracking open tunnels
sshEnv <- new.env(parent = emptyenv())
sshEnv$tunnels <- list()

# Hardcoded server public key (EC nistp256, from IRunService.SSH_PUB_KEY).
# Same key the RCP client uses for host verification.
SSH_SERVER_PUB_KEY_B64 <- "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEw0tjQGg+u7v64BclybJZzRfu781JFc0wXAdh3bD2Kk+lpGbpXzICpUZQaXJTGsMCJzslwk6s9a1rKD3em3F1qw=="

# ── Internal helpers ──────────────────────────────────────────────────────────

#' Get SSH key directory
#'
#' Uses the improve internal directory infrastructure for cross-platform support.
#' Respects the IMPROVE_SSH_KEY_DIR environment variable if set.
#' @noRd
sshKeyDir <- function() {
  dir <- Sys.getenv("IMPROVE_SSH_KEY_DIR", "")
  if (dir != "") return(dir)
  file.path(getImproveInternalDir(), "ssh")
}

#' Get SSH key file path
#' @noRd
sshKeyFile <- function() {
  file.path(sshKeyDir(), "client_key.pem")
}

#' Get known_hosts file path
#' @noRd
sshKnownHostsFile <- function() {
  file.path(sshKeyDir(), "known_hosts")
}

#' Check that a system command exists
#' @noRd
requireCommand <- function(cmd) {
  path <- Sys.which(cmd)
  if (path == "") {
    if (.Platform$OS.type == "windows") {
      stop(
        "'", cmd, "' not found. ",
        "On Windows, install Git for Windows (https://gitforwindows.org) ",
        "or enable the OpenSSH optional feature.",
        call. = FALSE
      )
    } else {
      stop("'", cmd, "' not found. Install OpenSSH.", call. = FALSE)
    }
  }
  invisible(path)
}

#' Generate EC P-256 key pair if it does not exist
#' @return Path to the key file
#' @noRd
ensureSSHKey <- function() {
  keyFile <- sshKeyFile()
  if (file.exists(keyFile)) {
    return(invisible(keyFile))
  }

  keyDir <- sshKeyDir()
  if (!dir.exists(keyDir)) {
    dir.create(keyDir, recursive = TRUE, mode = "0700")
  }

  log_info("Generating EC P-256 key pair for SSH tunnel...")
  key <- openssl::ec_keygen("P-256")
  openssl::write_pem(key, keyFile)
  Sys.chmod(keyFile, mode = "0600")
  log_info("Key saved to ", keyFile)

  invisible(keyFile)
}

#' Copy key to a temp file with strict permissions
#'
#' On WSL/NTFS, ssh-keygen refuses keys with 0777 permissions.
#' This copies the key to a temp file with 0600 permissions.
#' @return Path to the temp copy (caller must NOT delete — cached for session)
#' @noRd
sshKeyFileSafe <- function() {
  keyFile <- sshKeyFile()
  if (!file.exists(keyFile)) {
    stop("No SSH key found at ", keyFile, ". Call improveSetupSSH() first.", call. = FALSE)
  }

  # Check if the key file already has safe permissions
  info <- file.info(keyFile)
  mode <- as.integer(info$mode)
  # 384 = 0600 in decimal
  if (!is.na(mode) && bitwAnd(mode, 63) == 0) {
    return(keyFile)
  }

  # Copy to temp with strict permissions
  if (is.null(sshEnv$safeKeyFile) || !file.exists(sshEnv$safeKeyFile)) {
    tmpKey <- tempfile(pattern = "improve_ssh_", fileext = ".pem")
    file.copy(keyFile, tmpKey, overwrite = TRUE)
    Sys.chmod(tmpKey, mode = "0600")
    sshEnv$safeKeyFile <- tmpKey
  }
  sshEnv$safeKeyFile
}

#' Compute SSH wire-format fingerprint
#'
#' Uses ssh-keygen to compute the fingerprint in the format the server expects.
#' This is NOT the same as hashing the DER public key with openssl.
#' @return Character string like "SHA256:abc123..."
#' @noRd
sshFingerprint <- function() {
  requireCommand("ssh-keygen")
  safeKey <- sshKeyFileSafe()

  output <- system2(
    "ssh-keygen",
    args = c("-l", "-E", "sha256", "-f", safeKey),
    stdout = TRUE, stderr = TRUE
  )

  if (!is.null(attr(output, "status")) && attr(output, "status") != 0) {
    stop("ssh-keygen failed: ", paste(output, collapse = "\n"), call. = FALSE)
  }

  fp <- sub("^\\d+\\s+(SHA256:\\S+)\\s+.*", "\\1", output[1])
  if (!grepl("^SHA256:", fp)) {
    stop("Could not parse fingerprint from ssh-keygen output: ", output[1], call. = FALSE)
  }
  fp
}

#' Write known_hosts file with the hardcoded server public key
#' @return Path to known_hosts file, or NULL if conversion failed
#' @noRd
setupKnownHosts <- function(host, port) {
  requireCommand("ssh-keygen")

  tmpPem <- tempfile(fileext = ".pem")
  on.exit(unlink(tmpPem), add = TRUE)

  writeLines(c(
    "-----BEGIN PUBLIC KEY-----",
    SSH_SERVER_PUB_KEY_B64,
    "-----END PUBLIC KEY-----"
  ), tmpPem)

  sshPubkey <- tryCatch({
    out <- system2("ssh-keygen", args = c("-i", "-m", "PKCS8", "-f", tmpPem),
                   stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(out, "status")) && attr(out, "status") != 0) NULL
    else out[1]
  }, error = function(e) NULL)

  if (is.null(sshPubkey) || length(sshPubkey) == 0 || sshPubkey == "") {
    log_warn("Could not convert server public key. Host key verification will be disabled.")
    return(NULL)
  }

  knownHostsFile <- sshKnownHostsFile()
  keyDir <- sshKeyDir()
  if (!dir.exists(keyDir)) dir.create(keyDir, recursive = TRUE, mode = "0700")

  entry <- if (port == 22) {
    paste(host, sshPubkey)
  } else {
    paste0("[", host, "]:", port, " ", sshPubkey)
  }
  writeLines(entry, knownHostsFile)
  Sys.chmod(knownHostsFile, mode = "0600")
  knownHostsFile
}

#' Start step with SSH public key hash
#' @noRd
runStepWithSSH <- function(resourceId, fingerprint) {
  authenticatedREST(
    "resources/{stepId}/run",
    urlParams = list(stepId = resourceId),
    data = list(sshPublicKeyHash = fingerprint),
    restType = "POST"
  )
}

#' Derive hostname from a runserver URL when the hostname field is NA
#' @noRd
deriveRunserverHostname <- function(runserverRow) {
  hostname <- runserverRow$hostname
  if (is.na(hostname) || hostname == "") {
    parsed <- httr::parse_url(runserverRow$url)
    hostname <- parsed$hostname
  }
  hostname
}

#' Resolve toolBrowserUrl template for tunnel access
#' @noRd
resolveBrowserUrl <- function(template, runId, localPort) {
  url <- template
  url <- gsub("<runid>", runId, url, fixed = TRUE)
  url <- gsub("<host>", "localhost", url, fixed = TRUE)
  url <- gsub("<port>", as.character(localPort), url, fixed = TRUE)
  url
}

#' Open the SSH tunnel process
#' @return processx::process object
#' @noRd
openSSHProcess <- function(runId, host, remotePort, sshPort, localPort) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop(
      "Package 'processx' is required for SSH tunnels. ",
      "Install with: install.packages('processx')",
      call. = FALSE
    )
  }

  requireCommand("ssh")
  keyFile <- sshKeyFileSafe()

  knownHosts <- setupKnownHosts(host, sshPort)
  if (!is.null(knownHosts)) {
    hostKeyOpts <- c(
      "-o", "StrictHostKeyChecking=yes",
      "-o", paste0("UserKnownHostsFile=", knownHosts)
    )
  } else {
    hostKeyOpts <- c(
      "-o", "StrictHostKeyChecking=no",
      "-o", "UserKnownHostsFile=/dev/null"
    )
  }

  sshArgs <- c(
    "-N",
    "-L", paste0(localPort, ":", host, ":", remotePort),
    "-p", as.character(sshPort),
    "-i", keyFile,
    "-o", "ServerAliveInterval=30",
    "-o", "ServerAliveCountMax=3",
    "-o", "TCPKeepAlive=yes",
    "-o", "ExitOnForwardFailure=yes",
    "-o", "IdentitiesOnly=yes",
    hostKeyOpts,
    paste0(runId, "@", host)
  )

  p <- processx::process$new(
    command = Sys.which("ssh"),
    args = sshArgs,
    stdout = "|",
    stderr = "|",
    cleanup = TRUE
  )

  Sys.sleep(3)

  if (!p$is_alive()) {
    err <- tryCatch(p$read_error_lines(), error = function(e) "")
    stop(
      "SSH tunnel failed to start.\n",
      paste(err, collapse = "\n"),
      call. = FALSE
    )
  }

  p
}

#' Register tunnel cleanup on session end
#' @noRd
registerTunnelCleanup <- function() {
  if (!"sshTunnelCleanup" %in% listCloseFunctions()) {
    registerCloseFunction("sshTunnelCleanup", function() {
      for (t in sshEnv$tunnels) {
        tryCatch(t$close(), error = function(e) {})
      }
      sshEnv$tunnels <- list()
    })
  }
}

# ── Exported functions ────────────────────────────────────────────────────────

#' Set Up SSH Keys for Tunnel Access
#'
#' Generates an EC P-256 key pair for SSH tunnel authentication. The key is
#' stored at \code{~/.improve/ssh/client_key.pem}. If a key already exists,
#' it is not overwritten.
#'
#' The fingerprint is printed and returned invisibly. This fingerprint is
#' automatically registered with the server when using \code{\link{improveOpenTunnel}}.
#'
#' @param keyDir Optional. Directory for key storage. Defaults to
#'   \code{~/.improve/ssh/} or the \code{IMPROVE_SSH_KEY_DIR} environment variable.
#'
#' @return The SHA256 fingerprint string (invisibly).
#'
#' @details
#' Requires \code{ssh-keygen} (OpenSSH) to be available on the system PATH.
#' On Windows, this is included with Git for Windows or the built-in OpenSSH feature.
#'
#' @examples
#' \dontrun{
#' improveSetupSSH()
#' # SHA256:8c/2N0l/ZmFgogfEDU+a74HaeKZK4rrk8VCuiqqcCmQ
#' }
#'
#' @export
improveSetupSSH <- function(keyDir = NULL) {
  if (!is.null(keyDir)) {
    Sys.setenv(IMPROVE_SSH_KEY_DIR = keyDir)
  }

  ensureSSHKey()
  fp <- sshFingerprint()
  log_info("SSH fingerprint: ", fp)
  cat("SSH fingerprint:", fp, "\n")
  invisible(fp)
}


#' Open SSH Tunnel to a Running Step
#'
#' Opens an SSH port-forwarding tunnel to an interactive step (e.g. RStudio, Jupyter)
#' running on an improve runserver. If the step is not yet running, it is started
#' with the SSH public key fingerprint registered for authentication.
#'
#' @param ident A step identifier. Can be a step path, resource ID, entity ID,
#'   or a step environment returned by \code{realise()}.
#' @param localPort Integer. The local port to bind. Defaults to 8787 (RStudio default).
#' @param browse Logical. If \code{TRUE} (default), opens the tunnel URL in the default browser.
#'
#' @return An environment (tunnel object) with:
#'   \describe{
#'     \item{\code{url}}{The URL to access the step (e.g. \code{http://<runId>.localhost:8787/})}
#'     \item{\code{isAlive()}}{Returns \code{TRUE} if the tunnel is still open}
#'     \item{\code{close()}}{Closes the tunnel}
#'     \item{\code{pid}}{The SSH process PID}
#'     \item{\code{runId}}{The run ID}
#'     \item{\code{stepIdent}}{The step identifier}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Generates an SSH key pair if one does not exist
#'   \item If the step is not running, starts it with the SSH fingerprint registered
#'   \item Waits for the step to reach RUNNING status
#'   \item Reads the dynamically assigned Docker port from run metadata
#'   \item Opens an SSH tunnel from \code{localhost:<localPort>} to the container port
#'   \item Optionally opens the browser
#' }
#'
#' The tunnel is automatically closed when the R session ends.
#'
#' Requires the \code{processx} package (Suggests) and \code{ssh}/\code{ssh-keygen}
#' on the system PATH.
#'
#' @examples
#' \dontrun{
#' stepEnv <- createStepTemplateEnv(treeIdent = myTree)
#' stepEnv$setStepRunserverLabel("runserver")
#' stepEnv$setStepToolLabel("R 3_Std")
#' stepEnv$setStepToolInstance("imRstudio 1.3")
#' step <- stepEnv$realise(run = FALSE)
#'
#' tunnel <- improveOpenTunnel(step, localPort = 8787)
#' # Opens browser to http://<runId>.localhost:8787/
#'
#' tunnel$isAlive()
#' tunnel$close()
#' }
#'
#' @seealso \code{\link{improveSetupSSH}}, \code{\link{improveCloseTunnel}}
#'
#' @export
improveOpenTunnel <- function(ident, localPort = 8787, browse = TRUE) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop(
      "Package 'processx' is required for SSH tunnels. ",
      "Install with: install.packages('processx')",
      call. = FALSE
    )
  }

  # Ensure key exists and get fingerprint
  ensureSSHKey()
  fingerprint <- sshFingerprint()

  # Resolve step
  stepRes <- refreshResource(ident)
  resourceId <- stepRes$resourceId

  # Start step if not running
  if (stepRes$runStatus == "RUNNING") {
    stop(
      "Step is already running. Cannot register SSH fingerprint on a running step. ",
      "Terminate the step first with terminateStepResource(), or create a new step.",
      call. = FALSE
    )
  }

  log_info("Starting step with SSH fingerprint: ", fingerprint)
  resp <- runStepWithSSH(resourceId, fingerprint)
  if (is.null(resp)) {
    stop("Failed to start step. Check lastRestError() for details.", call. = FALSE)
  }

  # Wait for RUNNING
  log_info("Waiting for step to start...")
  for (i in 1:30) {
    Sys.sleep(5)
    stepRes <- refreshResource(resourceId)
    if (stepRes$runStatus == "RUNNING") break
    if (stepRes$runStatus == "FINISHED") {
      stop("Step finished before tunnel could be established.", call. = FALSE)
    }
  }
  if (stepRes$runStatus != "RUNNING") {
    stop("Step did not reach RUNNING status within timeout. Status: ", stepRes$runStatus, call. = FALSE)
  }

  # Get process and run details
  procs <- loadProcessesForStep(resourceId)
  mainProc <- procs[procs$processType == "main", ]
  toolBrowserUrl <- mainProc$toolBrowserUrl

  # Get latest run (includes port, runId, etc.)
  runData <- getLatestRun(resourceId, mainProc$id)
  if (is.null(runData)) {
    stop("Failed to get run details for step.", call. = FALSE)
  }
  runId <- runData$id
  remotePort <- runData$port
  if (is.null(remotePort) || is.na(remotePort)) {
    stop("No port found in run metadata. The step may not expose a port.", call. = FALSE)
  }

  # Get runserver connection details
  runserverLabel <- mainProc$runserverLabel
  rs <- loadRunserver(runserverLabel)
  if (is.null(rs)) {
    stop("Runserver '", runserverLabel, "' not found.", call. = FALSE)
  }
  rsHostname <- deriveRunserverHostname(rs)
  sshPort <- rs$sshPort
  if (is.na(sshPort)) {
    stop("Runserver '", runserverLabel, "' has no SSH port configured.", call. = FALSE)
  }

  # Open tunnel
  log_info("Opening SSH tunnel: localhost:", localPort, " -> ", rsHostname, ":", remotePort,
           " via ", rsHostname, ":", sshPort)
  sshProcess <- openSSHProcess(runId, rsHostname, remotePort, sshPort, localPort)

  # Resolve browser URL
  if (!is.null(toolBrowserUrl) && !is.na(toolBrowserUrl) && toolBrowserUrl != "") {
    url <- resolveBrowserUrl(toolBrowserUrl, runId, localPort)
  } else {
    url <- paste0("http://", runId, ".localhost:", localPort, "/")
  }

  # Build tunnel object
  tunnel <- new.env(parent = emptyenv())
  tunnel$url <- url
  tunnel$pid <- sshProcess$get_pid()
  tunnel$runId <- runId
  tunnel$stepIdent <- resourceId
  tunnel$.process <- sshProcess

  tunnel$isAlive <- function() {
    tunnel$.process$is_alive()
  }
  tunnel$close <- function() {
    if (tunnel$.process$is_alive()) {
      tunnel$.process$kill()
      log_info("SSH tunnel closed (PID: ", tunnel$pid, ")")
    }
    # Remove from tracked tunnels
    sshEnv$tunnels <- Filter(function(t) !identical(t, tunnel), sshEnv$tunnels)
  }

  # Track for cleanup
  sshEnv$tunnels <- c(sshEnv$tunnels, list(tunnel))
  registerTunnelCleanup()

  log_info("Tunnel open: ", url)
  cat("Tunnel open:", url, "\n")

  if (browse) {
    utils::browseURL(url)
  }

  invisible(tunnel)
}


#' Close an SSH Tunnel
#'
#' Closes an SSH tunnel previously opened by \code{\link{improveOpenTunnel}}.
#'
#' @param tunnel A tunnel object returned by \code{\link{improveOpenTunnel}}.
#'
#' @examples
#' \dontrun{
#' tunnel <- improveOpenTunnel(step)
#' # ... use RStudio ...
#' improveCloseTunnel(tunnel)
#' }
#'
#' @seealso \code{\link{improveOpenTunnel}}
#'
#' @returns `NULL`, invisibly. Stops with an error if `tunnel` is not the object
#'   [improveOpenTunnel()] returned.
#' @export
improveCloseTunnel <- function(tunnel) {
  if (!is.environment(tunnel) || is.null(tunnel$close)) {
    stop("Invalid tunnel object. Use the object returned by improveOpenTunnel().", call. = FALSE)
  }
  tunnel$close()
  invisible(NULL)
}
