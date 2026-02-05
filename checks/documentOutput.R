#!/usr/bin/env Rscript
# documentOutput.R
# Automatically documents function output in roxygen2 @returns tag

#' Document Function Output
#'
#' @param function_name Name of the function to document (as string)
#' @param ident Identifier to pass to the function
#' @return Invisible NULL
documentOutput <- function(function_name, ident) {

  # Set CRAN mirror
  options(repos = c(CRAN = "https://cloud.r-project.org/"))

  # Load required packages
  if (!require(devtools, quietly = TRUE)) {
    install.packages("devtools")
  }
  library(devtools)

  # Load the package
  cat("Loading improveR package...\n")
  devtools::load_all(".")

  # Connect to API
  cat("Connecting to API...\n")
  fn_connect <- function() {
    Sys.setenv(
      IMPROVER_REPO_URL = "http://envhost1.hc.scintecodev.internal:6110/repository"
    )
    Sys.setenv(IMPROVER_AUTO_AUTH = TRUE)

    improveR::clearConnectionData()
    improveConnect()

    setEditable(TRUE)
  }

  fn_connect()

  # Call the function
  cat(sprintf("Calling %s('%s')...\n", function_name, ident))
  func <- get(function_name)
  result <- func(ident)

  # Collect column information
  column_info <- list()
  data_column_found <- FALSE

  if (!is.null(result) && is.data.frame(result)) {
    # Get top-level columns first
    top_level_cols <- names(result)
    cat(sprintf("Found %d top-level columns\n", length(top_level_cols)))

    for (col_name in top_level_cols) {
      col_class <- class(result[[col_name]])[1]
      column_info[[paste0("toplevel_", col_name)]] <- list(
        name = col_name,
        class = col_class,
        level = "top"
      )
    }

    # Check if there's a "data" column containing a nested dataframe
    if ("data" %in% names(result)) {
      # Extract the first element if data is a list column
      nested_data <- if (is.list(result$data) && length(result$data) > 0) {
        result$data[[1]]
      } else {
        result$data
      }

      if (is.data.frame(nested_data)) {
        cat("Found nested 'data' column - extracting column information\n")
        data_column_found <- TRUE

        nested_cols <- names(nested_data)
        cat(sprintf("Found %d nested columns in 'data'\n", length(nested_cols)))

        for (col_name in nested_cols) {
          col_class <- class(nested_data[[col_name]])[1]
          column_info[[paste0("nested_", col_name)]] <- list(
            name = col_name,
            class = col_class,
            level = "nested"
          )
        }
      }
    }
  }

  # Find the R source file for the function
  r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  source_file <- NULL

  for (r_file in r_files) {
    lines <- readLines(r_file, warn = FALSE)
    # Look for the function definition
    func_pattern <- sprintf("^%s\\s*<-\\s*function", function_name)
    if (any(grepl(func_pattern, lines))) {
      source_file <- r_file
      break
    }
  }

  if (is.null(source_file)) {
    cat(sprintf("ERROR: Could not find source file for function '%s'\n", function_name))
    quit(status = 1)
  }

  cat(sprintf("Found source file: %s\n", source_file))

  # Read the source file
  source_lines <- readLines(source_file, warn = FALSE)

  # First, find the function definition line
  func_pattern <- sprintf("^%s\\s*<-\\s*function", function_name)
  func_def_line <- NULL
  for (i in seq_along(source_lines)) {
    if (grepl(func_pattern, source_lines[i])) {
      func_def_line <- i
      break
    }
  }

  if (is.null(func_def_line)) {
    cat(sprintf("ERROR: Could not find function definition for '%s'\n", function_name))
    quit(status = 1)
  }

  cat(sprintf("Found function definition at line %d\n", func_def_line))

  # Now search backwards from the function definition to find the roxygen block
  # The roxygen block should end just before the function definition
  roxygen_start <- NULL
  roxygen_end <- NULL

  # Find the end of the roxygen block (line before function definition)
  for (i in (func_def_line - 1):1) {
    if (grepl("^#'", source_lines[i])) {
      roxygen_end <- i
      break
    }
  }

  if (!is.null(roxygen_end)) {
    # Find the start of the roxygen block
    for (i in roxygen_end:1) {
      if (!grepl("^#'", source_lines[i])) {
        roxygen_start <- i + 1
        break
      }
      if (i == 1) {
        roxygen_start <- 1
      }
    }
  }

  if (is.null(roxygen_start) || is.null(roxygen_end)) {
    cat(sprintf("ERROR: Could not find roxygen documentation block for function '%s'\n", function_name))
    quit(status = 1)
  }

  cat(sprintf("Found roxygen block: lines %d to %d\n", roxygen_start, roxygen_end))

  # Find the @return or @returns line within this roxygen block
  return_line_idx <- NULL
  for (i in roxygen_start:roxygen_end) {
    if (grepl("^#'\\s*@returns?\\s", source_lines[i])) {
      return_line_idx <- i
      break
    }
  }

  if (is.null(return_line_idx)) {
    cat("No @returns tag found - will create one\n")
    # Find the best insertion point (before @export, @examples, or at the end of roxygen block)
    insert_before_line <- roxygen_end + 1
    for (i in roxygen_start:roxygen_end) {
      if (grepl("^#'\\s*@(export|examples|seealso|references)", source_lines[i])) {
        insert_before_line <- i
        break
      }
    }
    return_line_idx <- insert_before_line
    cat(sprintf("Will insert @returns at line %d\n", return_line_idx))
  } else {
    cat(sprintf("Found @returns tag at line %d\n", return_line_idx))
  }

  # Generate the new @returns documentation
  returns_lines <- c()

  if (data_column_found) {
    # Add top-level columns
    returns_lines <- c(returns_lines, "#' @returns A dataframe with the following columns:")
    top_level <- column_info[grepl("^toplevel_", names(column_info))]
    for (col in top_level) {
      returns_lines <- c(returns_lines, sprintf("#'   - `%s` (%s)", col$name, col$class))
    }

    # Add note about nested data column
    returns_lines <- c(returns_lines, "#'   ")
    returns_lines <- c(returns_lines, "#'   The `data` column contains a nested dataframe with:")

    # Add nested columns without data/ prefix, but with backticks
    nested <- column_info[grepl("^nested_", names(column_info))]
    for (col in nested) {
      returns_lines <- c(returns_lines, sprintf("#'   - `%s` (%s)", col$name, col$class))
    }
  } else {
    # Only top-level columns
    returns_lines <- c(returns_lines, "#' @returns A dataframe with the following columns:")
    for (col in column_info) {
      returns_lines <- c(returns_lines, sprintf("#'   - `%s` (%s)", col$name, col$class))
    }
  }

  # Determine if we're replacing an existing @returns or inserting a new one
  is_existing_returns <- grepl("^#'\\s*@returns?\\s", source_lines[return_line_idx])

  if (is_existing_returns) {
    # Find the end of the existing @returns documentation section
    end_idx <- return_line_idx
    for (i in (return_line_idx + 1):length(source_lines)) {
      if (!grepl("^#'", source_lines[i])) {
        end_idx <- i - 1
        break
      }
      # Check if this is a new roxygen tag
      if (grepl("^#'\\s*@", source_lines[i])) {
        end_idx <- i - 1
        break
      }
    }

    cat(sprintf("Replacing lines %d to %d\n", return_line_idx, end_idx))

    # Replace the @returns section
    new_source_lines <- c()
    if (return_line_idx > 1) {
      new_source_lines <- source_lines[1:(return_line_idx - 1)]
    }
    new_source_lines <- c(new_source_lines, returns_lines)
    if (end_idx < length(source_lines)) {
      new_source_lines <- c(new_source_lines, source_lines[(end_idx + 1):length(source_lines)])
    }
  } else {
    # Insert new @returns section
    cat(sprintf("Inserting new @returns at line %d\n", return_line_idx))

    new_source_lines <- c()
    if (return_line_idx > 1) {
      new_source_lines <- source_lines[1:(return_line_idx - 1)]
    }
    # Add a blank comment line before @returns for spacing
    new_source_lines <- c(new_source_lines, "#'")
    new_source_lines <- c(new_source_lines, returns_lines)
    if (return_line_idx <= length(source_lines)) {
      new_source_lines <- c(new_source_lines, source_lines[return_line_idx:length(source_lines)])
    }
  }

  # Write the updated source file
  writeLines(new_source_lines, source_file)

  cat(sprintf("\n✓ Updated @returns documentation in: %s\n", source_file))
  cat("\nGenerated documentation:\n")
  cat(paste(returns_lines, collapse = "\n"))
  cat("\n")

  invisible(NULL)
}

# Command-line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 2) {
    cat("Usage: Rscript documentOutput.R <function_name> <ident>\n")
    cat("\nExample: Rscript documentOutput.R getParentalDescendant 'envhost1.hc.scintecodev.internal-6111:FI-42410'\n")
    quit(status = 1)
  }

  function_name <- args[1]
  ident <- args[2]

  # Change to package directory
  setwd("C:/dev/git-repos/improver-base/improveR")

  documentOutput(function_name, ident)
}
