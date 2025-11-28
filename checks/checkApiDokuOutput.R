#!/usr/bin/env Rscript
# checkApiDokuOutput.R
# Validates API function output against roxygen2 documentation

#' Check API Documentation Output
#'
#' @param function_name Name of the function to check (as string)
#' @param ident Identifier to pass to the function
#' @param expected_cols Character vector of expected column names, or path to file
#' @param output_file Path to output Excel file
#' @return Invisible NULL
checkApiDokuOutput <- function(function_name, ident, expected_cols,
                                output_file = "checks/checkApiDokuOutput.xlsx") {

  # Set CRAN mirror
  options(repos = c(CRAN = "https://cloud.r-project.org/"))

  # Load required packages
  if (!require(openxlsx, quietly = TRUE)) {
    install.packages("openxlsx")
  }
  library(openxlsx)

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

  # Get actual column names
  actual_cols <- character(0)
  data_column_unpacked <- FALSE

  if (!is.null(result) && is.data.frame(result)) {
    # Check if there's a "data" column containing a nested dataframe
    if ("data" %in% names(result)) {
      # Extract the first element if data is a list column
      nested_data <- if (is.list(result$data) && length(result$data) > 0) {
        result$data[[1]]
      } else {
        result$data
      }

      if (is.data.frame(nested_data)) {
        cat("Found nested 'data' column - extracting column names from nested dataframe\n")
        actual_cols <- sort(names(nested_data))
        data_column_unpacked <- TRUE
      } else {
        cat("'data' column found but not a dataframe, using top-level columns\n")
        actual_cols <- sort(names(result))
      }
    } else {
      actual_cols <- sort(names(result))
    }
  }

  cat(sprintf("Function returned %d columns\n", length(actual_cols)))

  # Parse expected columns
  cat("Processing expected columns...\n")
  if (file.exists(expected_cols)) {
    # Read from file
    documented_cols <- readLines(expected_cols, warn = FALSE)
    documented_cols <- trimws(documented_cols)
    documented_cols <- documented_cols[documented_cols != ""]
  } else {
    # Parse from comma-separated string
    documented_cols <- strsplit(expected_cols, ",")[[1]]
    documented_cols <- trimws(documented_cols)
  }

  # Remove "data/" prefix from column names (from roxygen2 documentation)
  documented_cols <- gsub("^data/", "", documented_cols)

  documented_cols <- sort(unique(documented_cols))

  cat(sprintf("Expected columns: %d\n", length(documented_cols)))

  # Create comparison table
  all_cols <- sort(unique(c(documented_cols, actual_cols)))

  comparison <- data.frame(
    Column_Name = all_cols,
    In_Expected_List = ifelse(all_cols %in% documented_cols, "YES", "NO"),
    In_Actual_Data = ifelse(all_cols %in% actual_cols, "YES", "NO"),
    Status = ifelse(all_cols %in% documented_cols & all_cols %in% actual_cols, "Match",
                    ifelse(all_cols %in% documented_cols, "Not in actual data", "Not in expected list")),
    stringsAsFactors = FALSE
  )

  # Add metadata rows
  metadata_rows <- c("", "Function Call:", sprintf("%s('%s')", function_name, ident))

  # Add note if data column was unpacked
  if (data_column_unpacked) {
    metadata_rows <- c(metadata_rows, "names of returned column 'data' considered")
  }

  metadata_rows <- c(metadata_rows,
                     "", "Summary:",
                     sprintf("Total columns: %d", nrow(comparison)),
                     sprintf("Matching: %d", sum(comparison$Status == "Match")),
                     sprintf("Not in actual data: %d", sum(comparison$Status == "Not in actual data")),
                     sprintf("Not in expected list: %d", sum(comparison$Status == "Not in expected list")))

  metadata <- data.frame(
    Column_Name = metadata_rows,
    In_Expected_List = rep("", length(metadata_rows)),
    In_Actual_Data = rep("", length(metadata_rows)),
    Status = rep("", length(metadata_rows)),
    stringsAsFactors = FALSE
  )

  final_table <- rbind(comparison, metadata)

  # Helper function to add/update README sheet
  add_readme_sheet <- function(wb) {
    readme_text <- "NOTE: THIS FILE CONTAINS A CONTRAST OF COLUMN NAMES RETURNED BY FUNCTION CALLS IN IMPROVER AND COLUMN NAMES AS DOCUMENTED IN THE API DOCUMENTATION (https://6110.envhost1.hc.scinteco.com:1443/repository/api/v1/apiDoc#). THE COMPARISON WAS CREATED BY CLAUDE CODE AND ONLY SERVES A STARTING POINT FOR THE IDENTIFICATION OF GAPS IN THE DOCUMENTATION"

    # Remove README sheet if it exists
    if ("README" %in% names(wb)) {
      removeWorksheet(wb, "README")
    }

    # Add README as first sheet
    addWorksheet(wb, "README", gridLines = FALSE)
    writeData(wb, "README", data.frame(Message = readme_text), colNames = FALSE)

    # Format the README sheet
    setColWidths(wb, "README", cols = 1, widths = 120)

    # Wrap text for better readability
    addStyle(wb, "README", style = createStyle(wrapText = TRUE, valign = "top"),
             rows = 1, cols = 1, gridExpand = TRUE)
  }

  # Write to Excel (append if exists)
  if (file.exists(output_file)) {
    cat(sprintf("Updating existing file: %s\n", output_file))
    wb <- loadWorkbook(output_file)

    # Add/update README sheet
    add_readme_sheet(wb)

    # Remove function sheet if it already exists
    if (function_name %in% names(wb)) {
      removeWorksheet(wb, function_name)
      cat(sprintf("Removed existing sheet: %s\n", function_name))
    }

    addWorksheet(wb, function_name)
    writeData(wb, function_name, final_table)

    # Auto-size columns
    setColWidths(wb, function_name, cols = 1:4, widths = "auto")

    saveWorkbook(wb, output_file, overwrite = TRUE)
  } else {
    cat(sprintf("Creating new file: %s\n", output_file))
    wb <- createWorkbook()

    # Add README sheet first
    add_readme_sheet(wb)

    # Add function results sheet
    addWorksheet(wb, function_name)
    writeData(wb, function_name, final_table)

    # Auto-size columns
    setColWidths(wb, function_name, cols = 1:4, widths = "auto")

    saveWorkbook(wb, output_file)
  }

  cat(sprintf("\n✓ Results saved to: %s (sheet: %s)\n", output_file, function_name))

  # Print summary
  cat("\nSummary:\n")
  cat(sprintf("  Matching columns: %d\n", sum(comparison$Status == "Match")))
  cat(sprintf("  Not in actual data: %d\n", sum(comparison$Status == "Not in actual data")))
  cat(sprintf("  Not in expected list: %d\n", sum(comparison$Status == "Not in expected list")))

  invisible(NULL)
}

# Command-line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 3) {
    cat("Usage: Rscript checkApiDokuOutput.R <function_name> <ident> <expected_columns>\n")
    cat("  <expected_columns> can be:\n")
    cat("    - Comma-separated column names: 'col1,col2,col3'\n")
    cat("    - Path to file with column names (one per line)\n")
    cat("\nExample: Rscript checkApiDokuOutput.R getParentalDescendant 'envhost1.hc.scintecodev.internal-6111:FI-42410' 'type,resourceId,name'\n")
    quit(status = 1)
  }

  function_name <- args[1]
  ident <- args[2]
  expected_cols <- args[3]

  # Change to package directory
  setwd("C:/dev/git-repos/improver-base/improveR")

  checkApiDokuOutput(function_name, ident, expected_cols)
}
