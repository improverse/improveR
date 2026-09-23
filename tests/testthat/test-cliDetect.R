# CLI-Erkennung. Bis 10.09.2026 war die ganze Datei R/cliDetect.R toter Code:
# funf ihrer Funktionen waren in R/cliSetup.R noch einmal definiert, DESCRIPTION
# hat kein Collate, R laedt alphabetisch - cliSetup.R gewann (IMR-272).
# Diese Tests halten fest, dass es genau eine Erkennung gibt und dass sie die
# Formen verarbeitet, die improveRcontributions::cliPath() tatsaechlich liefert.

test_that("es gibt nur eine Definition der Erkennungsfunktionen|ics2063", {
  # Der eigentliche Regressionsschutz: findet jemand die alte Fassung wieder
  # ein, faellt es hier auf statt erst im naechsten Qualifizierungslauf.
  ns <- asNamespace("improveR")
  # A fifth name went with the CLI surface it reported (IMR-282): improveR now
  # targets the released CLI only, and the flag had no meaning left.
  for (fn in c("detectCli", "cliMode", "cliDetectedVersion",
               "resetCliDetection")) {
    expect_true(exists(fn, envir = ns, inherits = FALSE), info = fn)
  }
  # cliDetect.R fuehrt Buch in cliInfoEnv, die abgeloeste Fassung tat es in
  # cliEnv$cliMode. Ist cliMode() die richtige, schreibt sie nach cliInfoEnv.
  improveR:::resetCliDetection()
  improveR:::cliMode()
  expect_false(is.null(get("cliInfoEnv", envir = ns)$detected))
})

test_that("die Version wird aus einem JAR-Dateinamen gelesen|ics2063", {
  expect_equal(improveR:::versionFromJarName("/x/improve-cli-4.4.5.jar"), "4.4.5")
  expect_equal(improveR:::versionFromJarName("improve-cli-4.5.0.jar"), "4.5.0")
  expect_equal(improveR:::versionFromJarName("/x/improve-cli.jar"), "unknown")
})

test_that("IMPROVE_CLI_PATH hat Vorrang und liefert die Version|ics2063", {
  stub <- tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", "echo 'improve-cli version: 4.4.5'"), stub)
  Sys.chmod(stub, "0755")
  old <- Sys.getenv("IMPROVE_CLI_PATH", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("IMPROVE_CLI_PATH") else Sys.setenv(IMPROVE_CLI_PATH = old)
    improveR:::resetCliDetection()
    unlink(stub)
  }, add = TRUE)

  Sys.setenv(IMPROVE_CLI_PATH = stub)
  improveR:::resetCliDetection()
  expect_equal(improveR:::cliMode(), "legacy_binary")
  expect_equal(improveR:::cliDetectedVersion(), "4.4.5")
})

test_that("eine CLI ohne Versionsausgabe meldet 'unknown' statt zu scheitern|ics2063", {
  stub <- tempfile(fileext = ".sh")
  writeLines(c("#!/bin/sh", "echo 'kein Versionsformat hier'"), stub)
  Sys.chmod(stub, "0755")
  on.exit({ unlink(stub); improveR:::resetCliDetection() }, add = TRUE)
  expect_equal(improveR:::versionFromCli(stub), "unknown")
})

test_that("versionFromCli bricht bei einem nicht ausfuehrbaren Befehl nicht ab|ics2063", {
  expect_equal(improveR:::versionFromCli("/gibt/es/nicht"), "unknown")
})
