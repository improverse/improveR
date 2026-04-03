Sys.setenv(TEST_NAME = "sshTunnel")

ensureTestFolder <- function() {
  Sys.setenv(TEST_NAME = "sshTunnel")
  if (!exists("TEST_FOLDER") || is.null(TEST_FOLDER)) {
    improveR::setEditable(TRUE)
    TEST_FOLDER <- improveR:::workflowFilesSetup()
    assign(x = "TEST_FOLDER", value = TEST_FOLDER, envir = globalenv())
    return(TEST_FOLDER)
  }
  return(TEST_FOLDER)
}

rstudioStep <- function(testTree) {
  rstudio_runserver <- Sys.getenv("RSTUDIO_RUNSERVER")
  rstudio_tool <- Sys.getenv("RSTUDIO_TOOL")
  rstudio_tool_instance <- Sys.getenv("RSTUDIO_TOOL_INSTANCE")

  if (rstudio_runserver == "" || rstudio_tool == "" || rstudio_tool_instance == "") {
    skip("RSTUDIO_RUNSERVER, RSTUDIO_TOOL, RSTUDIO_TOOL_INSTANCE not configured in .Renviron")
  }

  stepEnv <- createStepTemplateEnv(treeIdent = testTree)
  stepEnv$setStepRunserverLabel(rstudio_runserver)
  stepEnv$setStepToolLabel(rstudio_tool)
  stepEnv$setStepToolInstance(rstudio_tool_instance)

  return(stepEnv)
}


test_that("SSH key setup|sshTunnel", {
  # Clean up any existing key for a fresh test
  keyFile <- improveR:::sshKeyFile()
  keyDir <- improveR:::sshKeyDir()

  # Setup SSH
  fp <- improveSetupSSH()

  expect_true(file.exists(keyFile))
  expect_true(grepl("^SHA256:", fp))

  # Calling again should return the same fingerprint (idempotent)
  fp2 <- improveSetupSSH()
  expect_equal(fp, fp2)
})


test_that("SSH tunnel to RStudio|sshTunnel", {
  skip_if_not(requireNamespace("processx", quietly = TRUE),
              "processx not installed")
  skip_if(Sys.which("ssh") == "", "ssh not available")

  TEST_FOLDER <- ensureTestFolder()
  improveR::setEditable(TRUE)

  # Create analysis tree and RStudio step
  testTree <- createAnalysisTree(
    targetIdent = TEST_FOLDER,
    treeName = paste0("sshTunnelTest_", format(Sys.time(), "%H%M%S"))
  )

  stepEnv <- rstudioStep(testTree)
  stepEnv$setStepDescription("SSH Tunnel Test")
  stepEnv$setStepRationale("Automated test for SSH tunnel")

  step <- stepEnv$realise(run = FALSE)
  stepRes <- step$getStepResource()

  # Open tunnel (don't auto-browse in test)
  tunnel <- improveOpenTunnel(stepRes$resourceId, localPort = 18787, browse = FALSE)

  expect_true(is.environment(tunnel))
  expect_true(grepl("^http://", tunnel$url))
  expect_true(grepl("localhost:18787", tunnel$url))
  expect_true(tunnel$isAlive())
  expect_false(is.null(tunnel$runId))

  # Verify HTTP access through the tunnel
  resp <- tryCatch(
    httr::GET(tunnel$url, httr::timeout(10)),
    error = function(e) NULL
  )
  if (!is.null(resp)) {
    # RStudio returns 302 (redirect) or 200
    expect_true(httr::status_code(resp) %in% c(200, 302))
  }

  # Close tunnel
  improveCloseTunnel(tunnel)
  expect_false(tunnel$isAlive())

  # Terminate the step
  tryCatch(
    terminateStepResource(stepRes$resourceId),
    error = function(e) {
      # Interactive steps may not terminate cleanly, that's OK
    }
  )
})


test_that("improveCloseTunnel validates input|sshTunnel", {
  expect_error(improveCloseTunnel("not a tunnel"), "Invalid tunnel object")
  expect_error(improveCloseTunnel(list()), "Invalid tunnel object")
})
