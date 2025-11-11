#' Generate BibTeX Bibliography from DOIs
#'
#' Creates a BibTeX (.bib) file from a data frame containing DOI information.
#' This function fetches bibliographic information from DOIs and writes them
#' to a .bib file suitable for use with LaTeX/BibTeX.
#'
#' @param data A data frame containing a column with DOI information
#' @param doi_column Name of the column containing DOIs (default: "doi")
#' @param bib_file Path to the output .bib file (default: "./refs.bib")
#' @param filter_dois Logical, whether to filter for valid DOI format (starts with "10.").
#'   If TRUE, only entries matching DOI pattern will be processed. If FALSE, all
#'   non-NA values in doi_column will be attempted. (default: TRUE)
#' @param overwrite Logical, whether to overwrite existing .bib file. If FALSE,
#'   new entries will be appended (default: FALSE)
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return Invisibly returns a character vector of successfully processed DOIs.
#'   Also creates/updates the specified .bib file.
#'
#' @details
#' This function uses `RefManageR::GetBibEntryWithDOI()` to fetch bibliographic
#' information from DOI.org. It requires an internet connection and makes API
#' calls, so processing large numbers of DOIs may take time.
#'
#' The function will:
#' - Filter for unique DOIs (and optionally for valid DOI format)
#' - Skip NA values
#' - Attempt to fetch each DOI's bibliographic information
#' - Write results to a .bib file
#' - Print warnings for DOIs that couldn't be processed
#'
#' @family writing functions
#' @seealso \code{\link{write_dockerfile}} for reproducibility
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using the built-in MAP reduction dataset
#' library(dplyr)
#' data(map_reduction_data)
#'
#' # Generate bibliography from DOIs
#' make_bib(
#'   data = map_reduction_data,
#'   doi_column = "doi",
#'   bib_file = "my_references.bib"
#' )
#'
#' # You can also provide just a vector of DOIs
#' dois <- c("10.1016/j.appet.2025.108233", "10.1017/bpp.2018.25")
#' doi_df <- data.frame(doi = dois)
#' make_bib(doi_df, bib_file = "test.bib")
#' }
make_bib <- function(data,
                     doi_column = "doi",
                     bib_file = "./refs.bib",
                     filter_dois = TRUE,
                     overwrite = FALSE,
                     verbose = TRUE) {

  # Check if RefManageR is available
  if (!requireNamespace("RefManageR", quietly = TRUE)) {
    stop("Package 'RefManageR' is required but not installed. ",
         "Install it with: install.packages('RefManageR')")
  }

  # Check if doi_column exists
  if (!doi_column %in% names(data)) {
    stop("Column '", doi_column, "' not found in data. ",
         "Available columns: ", paste(names(data), collapse = ", "))
  }

  # Extract DOIs
  dois <- data[[doi_column]]

  # Remove NAs
  dois <- dois[!is.na(dois)]

  # Filter for valid DOI format if requested
  if (filter_dois) {
    doi_pattern <- "^10\\."
    valid_dois <- grepl(doi_pattern, dois)
    if (sum(valid_dois) == 0) {
      stop("No valid DOIs found (DOIs should start with '10.'). ",
           "Set filter_dois = FALSE to process all values.")
    }
    if (sum(!valid_dois) > 0 && verbose) {
      message("Filtered out ", sum(!valid_dois), " non-DOI entries")
    }
    dois <- dois[valid_dois]
  }

  # Get unique DOIs
  dois <- unique(dois)

  if (verbose) {
    message("Processing ", length(dois), " unique DOI(s)")
  }

  # Remove existing file if overwrite = TRUE
  if (overwrite && file.exists(bib_file)) {
    if (verbose) {
      message("Removing existing file: ", bib_file)
    }
    file.remove(bib_file)
  }

  # Track successful DOIs
  successful_dois <- character(0)

  # Process each DOI
  for (i in seq_along(dois)) {
    doi <- dois[i]

    if (verbose) {
      message("Processing DOI ", i, "/", length(dois), ": ", doi)
    }

    tryCatch({
      bib_entry <- RefManageR::GetBibEntryWithDOI(doi = doi)
      RefManageR::WriteBib(bib = bib_entry, file = bib_file, append = TRUE)
      successful_dois <- c(successful_dois, doi)
    }, error = function(e) {
      warning("Failed to process DOI: ", doi, "\n  Error: ", e$message, call. = FALSE)
    })
  }

  if (verbose) {
    message("\nSuccessfully processed ", length(successful_dois), " of ",
            length(dois), " DOIs")
    message("Bibliography written to: ", normalizePath(bib_file, mustWork = FALSE))
  }

  invisible(successful_dois)
}
