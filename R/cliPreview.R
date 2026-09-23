# Client-side preview for the CLI wrappers (IMR-283).
#
# The released CLI has no --preview. Before this, pushCli(preview = TRUE)
# declared a dry run, never passed the option on, and performed the write:
# preview only decided whether to invalidate caches afterwards. A dry run that
# writes is worse than no dry run.
#
# It is computed here instead of refused, because the clone carries everything
# needed to compute it exactly. `.improve/entities/0.entities` records, per
# file, the version it was checked out at and its hash at that moment:
#
#   Path  Type  CheckoutVersionId  LastModifiedTimeAtSync  SizeAtSync
#         HashAlgorithm  FileHashAtSync
#
# HashAlgorithm 1 is SHA-256, base64 encoded - verified against a live clone on
# 2026-09-15: both recorded hashes reproduce byte for byte.
#
# That makes the push side a local computation with no server call at all, and
# the pull side one read.

#' Read the baseline a clone was created from
#'
#' @param localPath The local repository.
#' @returns A data frame with one row per tracked file, or NULL when the
#'   repository carries no entity file.
#' @noRd
readCliBaseline <- function(localPath) {
  entityDir <- file.path(localPath, ".improve", "entities")
  if (!dir.exists(entityDir)) {
    return(NULL)
  }
  parts <- list.files(entityDir, pattern = "\\.entities$", full.names = TRUE)
  if (length(parts) == 0) {
    return(NULL)
  }
  rows <- list()
  for (p in parts) {
    lines <- readLines(p, warn = FALSE)
    start <- which(lines == "[entities]")
    if (length(start) == 0 || start + 1 > length(lines)) next
    header <- strsplit(lines[start + 1], "\t", fixed = TRUE)[[1]]
    body <- lines[seq(start + 2, length(lines))]
    body <- body[nzchar(trimws(body))]
    for (l in body) {
      f <- strsplit(l, "\t", fixed = TRUE)[[1]]
      if (length(f) != length(header)) next
      names(f) <- header
      rows[[length(rows) + 1]] <- as.list(f)
    }
  }
  if (length(rows) == 0) {
    return(NULL)
  }
  df <- do.call(rbind, lapply(rows, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  df$SizeAtSync <- suppressWarnings(as.numeric(df$SizeAtSync))
  df
}

#' Files the CLI does not track
#'
#' `repository.properties` carries `process.toolDeletePatterns` - the entries
#' the tool writes and the repository ignores (.RData, .Rhistory, conf.json and
#' friends). A preview that listed them would promise a push that never happens.
#' @noRd
cliIgnoredNames <- function(localPath) {
  props <- file.path(localPath, ".improve", "repository.properties")
  if (!file.exists(props)) {
    return(character(0))
  }
  lines <- readLines(props, warn = FALSE)
  hit <- grep("^process\\.toolDeletePatterns=", lines, value = TRUE)
  if (length(hit) == 0) {
    return(character(0))
  }
  raw <- sub("^process\\.toolDeletePatterns=", "", hit[1])
  # the value is stored with literal \r\n separators
  parts <- unlist(strsplit(raw, "\\\\r\\\\n|\\\\n|\\\\r"))
  trimws(parts[nzchar(trimws(parts))])
}

#' Hash a file the way the CLI records it
#'
#' @noRd
cliFileHash <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  openssl::base64_encode(openssl::sha256(con))
}

#' What a push would send
#'
#' Compares the working copy against the baseline the clone was made from. No
#' server call: the baseline holds the hash of every file at checkout, so the
#' comparison is local and exact.
#'
#' @param localPath The local repository.
#' @returns A data frame with columns \code{path}, \code{change}
#'   (\code{"added"}, \code{"modified"} or \code{"deleted"}) and \code{size}.
#'   Zero rows when nothing would be sent.
#' @noRd
previewPush <- function(localPath) {
  baseline <- readCliBaseline(localPath)
  ignored <- cliIgnoredNames(localPath)

  onDisk <- list.files(localPath, recursive = TRUE, all.files = FALSE, no.. = TRUE)
  onDisk <- onDisk[!startsWith(onDisk, ".improve")]
  if (length(ignored)) {
    onDisk <- onDisk[!basename(onDisk) %in% ignored]
  }

  known <- if (is.null(baseline)) character(0) else baseline$Path
  out <- list()

  for (rel in onDisk) {
    full <- file.path(localPath, rel)
    if (!rel %in% known) {
      out[[length(out) + 1]] <- data.frame(path = rel, change = "added",
                                           size = file.size(full),
                                           stringsAsFactors = FALSE)
      next
    }
    recorded <- baseline[baseline$Path == rel, ][1, ]
    # size first: it settles the common case without reading the file
    if (!is.na(recorded$SizeAtSync) && recorded$SizeAtSync != file.size(full)) {
      out[[length(out) + 1]] <- data.frame(path = rel, change = "modified",
                                           size = file.size(full),
                                           stringsAsFactors = FALSE)
      next
    }
    if (!identical(cliFileHash(full), recorded$FileHashAtSync)) {
      out[[length(out) + 1]] <- data.frame(path = rel, change = "modified",
                                           size = file.size(full),
                                           stringsAsFactors = FALSE)
    }
  }

  for (rel in setdiff(known, onDisk)) {
    out[[length(out) + 1]] <- data.frame(path = rel, change = "deleted",
                                         size = NA_real_, stringsAsFactors = FALSE)
  }

  if (length(out) == 0) {
    return(data.frame(path = character(0), change = character(0),
                      size = numeric(0), stringsAsFactors = FALSE))
  }
  result <- do.call(rbind, out)
  result[order(result$change, result$path), ]
}

#' What a pull would bring down
#'
#' Compares the baseline the clone was taken from against what the resource
#' holds now. One read, no write.
#'
#' The anchor is \code{CheckoutVersionId}, which is the child resource's
#' \code{resourceVersionId} - verified against a live clone on 2026-09-15.
#' Two other candidates were tried first and do NOT line up, so neither is used:
#' \code{pull.revisionId} from \code{repository.properties} matches no field of
#' the resource at all, and \code{entityVersionId} is a differently shaped
#' identifier. A comparison against either reports "changes available" on a
#' clone that is perfectly up to date.
#'
#' @param localPath The local repository.
#' @returns A data frame with \code{path} and \code{change} (\code{"modified"},
#'   \code{"added"} or \code{"deleted"}). Zero rows when nothing would be fetched.
#' @noRd
previewPull <- function(localPath) {
  props <- file.path(localPath, ".improve", "repository.properties")
  if (!file.exists(props)) {
    stop("not a local repository: ", localPath, call. = FALSE)
  }
  lines <- readLines(props, warn = FALSE)
  hit <- grep("^pull\\.root\\.resourceId=", lines, value = TRUE)
  if (length(hit) == 0) {
    stop("local repository carries no pull.root.resourceId: ", localPath, call. = FALSE)
  }
  rootId <- gsub("\\\\", "", sub("^pull\\.root\\.resourceId=", "", hit[1]))

  baseline <- readCliBaseline(localPath)
  children <- refreshChildResources(rootId)$data[[1]]
  if (is.null(children)) {
    children <- data.frame(name = character(0), resourceVersionId = character(0),
                           stringsAsFactors = FALSE)
  }

  known <- if (is.null(baseline)) character(0) else baseline$Path
  out <- list()

  for (i in seq_len(nrow(children))) {
    name <- as.character(children$name[i])
    serverVersion <- as.character(children$resourceVersionId[i])
    if (!name %in% known) {
      out[[length(out) + 1]] <- data.frame(path = name, change = "added",
                                           stringsAsFactors = FALSE)
      next
    }
    recorded <- baseline[baseline$Path == name, ][1, ]
    if (!identical(serverVersion, recorded$CheckoutVersionId)) {
      out[[length(out) + 1]] <- data.frame(path = name, change = "modified",
                                           stringsAsFactors = FALSE)
    }
  }

  for (name in setdiff(known, as.character(children$name))) {
    out[[length(out) + 1]] <- data.frame(path = name, change = "deleted",
                                         stringsAsFactors = FALSE)
  }

  if (length(out) == 0) {
    return(data.frame(path = character(0), change = character(0),
                      stringsAsFactors = FALSE))
  }
  result <- do.call(rbind, out)
  result[order(result$change, result$path), ]
}
