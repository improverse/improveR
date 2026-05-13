#' Execute CLI Command
#'
#' Canonical executor for all CLI calls.  Handles two calling styles:
#'
#' \strong{Vector style} (preferred, used by all new picocli commands):
#' \code{args} is a character vector; each element is passed as a separate
#' argument to \code{system2()}, so paths with spaces are handled correctly
#' without manual quoting.
#'
#' \strong{String style} (legacy, used only by \code{configureUserProfile}):
#' \code{args} is a single string that is appended to the CLI command via
#' \code{system()}.  Callers are responsible for quoting any values that
#' contain spaces.
#'
#' @param args Character vector of CLI arguments, or a single string for legacy
#'   callers.
#' @param json Logical.  If \code{TRUE}, appends \code{--json} to the argument
#'   list and parses the captured output as JSON.  Only valid with vector style.
#' @return For \code{json = FALSE}: character vector of output lines (invisibly).
#'   For \code{json = TRUE}: parsed R list (invisibly).
#' @noRd
executeCli <- function(args, json = FALSE) {
  cliString <- cliPath()

  if (is.null(cliString) || nchar(cliString) == 0) {
    stop("No CLI available — cliPath() returned NULL or empty.", call. = FALSE)
  }

  # ── Vector style ─────────────────────────────────────────────────────────
  if (length(args) > 1 || json) {
    if (json) args <- c(args, "--json")

    # The CLI 4.5 JAR ships with two surfaces: a legacy positional parser and
    # a picocli flag parser. Subcommands like `clone`, `push`, `status`, `diff`,
    # `cache` exist in both, but the picocli flags (--access-token, -C, --profile,
    # --include-files, etc.) are only recognised under the picocli surface, which
    # is gated behind a global `--pico` switch. Inject it here so every call site
    # whose `if (hasPicocli())` branch was taken actually reaches that surface.
    if (hasPicocli()) {
      args <- c("--pico", args)
    }

    # cliPath() returns a compound string, e.g. "java -jar /path/to.jar".
    # Split on whitespace to extract the executable and its fixed base args,
    # then pass user args as a separate vector so system2() quotes them
    # individually — paths with spaces are handled correctly.
    parts    <- strsplit(cliString, " ", fixed = TRUE)[[1]]
    exe      <- parts[1]
    baseArgs <- if (length(parts) > 1) parts[-1] else character(0)
    allArgs  <- c(baseArgs, args)

    # R's system2 joins args with spaces but only shell-quotes the *command*,
    # not the args. Any arg containing whitespace (e.g. a -m comment with a
    # space) gets re-split by the shell and silently turns into extra
    # positional args. shQuote each element so the shell preserves them
    # exactly as the JAR receives them.
    raw    <- system2(exe, shQuote(allArgs), stdout = TRUE, stderr = TRUE)
    status <- attr(raw, "status")
    if (!is.null(status) && status != 0) {
      log_warn("CLI command failed (exit code ", status, "): ",
               paste(tail(raw, 5), collapse = "\n"))
    }

    if (json) {
      # The JAR emits diagnostic lines (e.g. "Info: AUTH METHOD: ...") on the
      # same stdout stream as the JSON payload, contrary to its own --json help
      # text. Locate the first line that begins a JSON value and parse from
      # there; surface the discarded prefix so it stays visible.
      json_start <- which(grepl("^\\s*[{\\[]", raw))[1]
      if (is.na(json_start)) {
        log_warn("CLI --json output had no JSON content. Raw output: ",
                 paste(utils::head(raw, 10), collapse = " | "))
        return(invisible(NULL))
      }
      if (json_start > 1) {
        log_info("CLI diagnostic prefix discarded before JSON: ",
                 paste(raw[seq_len(json_start - 1)], collapse = " | "))
      }
      json_text <- paste(raw[seq(json_start, length(raw))], collapse = "\n")
      # simplifyVector = TRUE (jsonlite default): JSON arrays-of-objects become
      # data.frames, which is what test code expects (e.g. statusCli's
      # `data$compare` is treated as a data.frame with localChanged columns).
      return(invisible(jsonlite::fromJSON(json_text, simplifyVector = TRUE)))
    }
    return(invisible(raw))
  }

  # ── String style (legacy single-string callers) ───────────────────────────
  result   <- system(paste(cliString, args), intern = TRUE)
  exitCode <- attr(result, "status")
  if (!is.null(exitCode) && exitCode != 0) {
    log_warn("CLI command failed (exit code ", exitCode, "): ",
             paste(tail(result, 5), collapse = "\n"))
  }
  invisible(result)
}

#' Warn about legacy-ignored arguments
#' @noRd
warnLegacyIgnored <- function(..., caller = "CLI") {
  dots <- list(...)
  ignored <- names(dots)[!sapply(dots, function(x) is.null(x) || identical(x, FALSE))]
  if (length(ignored) > 0) {
    cli::cli_warn(c(
      "{caller}() is running against the legacy CLI surface.",
      "i" = "The following arguments are ignored: {paste(ignored, collapse = ', ')}.",
      "i" = "Upgrade improveRcontributions to get the new CLI (4.4.2+)."
    ))
  }
}
