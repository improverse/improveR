# Keine Funktion darf im Paket zweimal definiert sein.
#
# DESCRIPTION hat kein Collate, R laedt die Dateien in R/ alphabetisch. Eine
# zweite Definition ueberschreibt damit stillschweigend die erste - kein Fehler,
# keine Warnung, kein Hinweis beim Bauen. Am 10.09.2026 waren es sieben:
#
#   detectCli, cliMode, cliDetectedVersion, resetCliDetection and one further
#   capability function, since removed (IMR-282)
#       cliDetect.R verdeckt von cliSetup.R. Die gesamte reichere Erkennung war
#       unerreichbar; die Pruefumgebung lief dadurch gegen die unfreigegebene
#       CLI 4.5.0, ohne Hebel das umzustellen (IMR-272).
#   validateResource
#       resourceRelations.R verdeckt von reviews.R. Die beiden Fassungen
#       unterschieden sich in der Fehlerbehandlung (IMR-273).
#   timing
#       authenticatedREST.R verdeckt von prepareStep.R. Beide leer, harmlos.
#
# Gefunden wurden sie durch Zufall. Diese Pruefung findet die naechste sofort.

test_that("keine Funktion ist im Paket mehrfach definiert|ics1081", {
  rDir <- system.file("..", package = "improveR")
  # Gegen die Quelle pruefen, nicht gegen das installierte Paket: im Namespace
  # ist von zwei Definitionen nur noch eine sichtbar - genau das ist ja das
  # Problem. Sichtbar wird es nur in den Dateien.
  srcDir <- Sys.getenv("IMPROVER_SOURCE_DIR", unset = "")
  if (!nzchar(srcDir)) srcDir <- file.path("..", "..", "R")
  skip_if_not(dir.exists(srcDir),
              paste0("Quellverzeichnis nicht gefunden: ", srcDir))

  files <- list.files(srcDir, pattern = "\\.[Rr]$", full.names = TRUE)
  expect_true(length(files) > 0, info = "keine R-Quelldateien gefunden")

  defs <- list()
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    hits <- regmatches(lines, regexpr("^[A-Za-z_.][A-Za-z0-9_.]*(?=\\s*<-\\s*function)",
                                      lines, perl = TRUE))
    for (i in seq_along(hits)) {
      nm <- hits[i]
      defs[[nm]] <- c(defs[[nm]], basename(f))
    }
  }

  duplicated <- defs[vapply(defs, length, integer(1)) > 1]
  expect_equal(
    length(duplicated), 0L,
    info = if (length(duplicated) == 0) "" else paste0(
      "Mehrfach definiert: ",
      paste(sprintf("%s (%s)", names(duplicated),
                    vapply(duplicated, paste, character(1), collapse = ", ")),
            collapse = "; "))
  )
})
