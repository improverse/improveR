# restContent() ersetzt das blosse httr::content() auf einer authenticatedREST()-
# Antwort. Die alte Form bricht bei einem fehlgeschlagenen Aufruf mit
# "is.response(x) is not TRUE" ab - eine Meldung, die weder Status noch URL noch
# Operation nennt (IMR-270). Kein Server noetig: der Fehlerpfad liest nur
# lastRestError().
#
# Die Pruefungen richten sich ihr eigenes Logziel ein, statt die Konsole
# abzufangen. Grund: test-errorMessages.R setzt improver.logfile und stellt es
# nicht zurueck - danach schreibt logging in eine Datei und nicht mehr nach
# stdout. Eine Pruefung, die davon abhaengt, besteht allein und scheitert in der
# Suite. Genau die Reihenfolgeabhaengigkeit, die dieser Vorgang beseitigen soll.

withOwnLogFile <- function(code) {
  logFile <- tempfile(fileext = ".log")
  old <- Sys.getenv("improver.logfile", unset = NA)
  Sys.setenv(improver.logfile = logFile)
  improveR:::initImproveLogging("INFO")
  on.exit({
    if (is.na(old)) Sys.unsetenv("improver.logfile") else Sys.setenv(improver.logfile = old)
    improveR:::initImproveLogging("INFO")
    unlink(logFile)
  }, add = TRUE)
  force(code)
  if (file.exists(logFile)) paste(readLines(logFile, warn = FALSE), collapse = " ") else ""
}

test_that("ein fehlgeschlagener Aufruf liefert NULL statt abzubrechen|ics1082", {
  improveR:::clearLastRestError()
  expect_null(improveR:::restContent(NULL, "someOperation"))
})

test_that("ein erfolgreicher Aufruf reicht den Inhalt durch|ics1082", {
  # Alles, was nicht NULL und nicht FALSE ist, geht an httr::content() weiter -
  # hier reicht die Feststellung, dass der Fehlerpfad nicht genommen wird.
  improveR:::clearLastRestError()
  expect_error(improveR:::restContent("kein response-Objekt", "someOperation"))
})

test_that("die protokollierte Diagnose nennt Status, Methode und URL|ics1082", {
  improveR:::setLastRestError(403L, "https://host/repository/api/v1/groups", "POST", "Forbidden")
  logged <- withOwnLogFile(improveR:::restContent(NULL, "createGroup"))
  expect_true(grepl("createGroup", logged, fixed = TRUE))
  expect_true(grepl("403", logged, fixed = TRUE))
  expect_true(grepl("POST", logged, fixed = TRUE))
  expect_true(grepl("/groups", logged, fixed = TRUE))
})

test_that("ohne aufgezeichneten Fehler wird NULL geliefert und das gesagt|ics1082", {
  improveR:::clearLastRestError()
  logged <- withOwnLogFile(expect_null(improveR:::restContent(NULL, "someOperation")))
  expect_true(grepl("no error was recorded", logged, fixed = TRUE))
})
