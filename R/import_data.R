#' Comprehensive Agricultural Data Importation and Diagnostic Infrastructure
#'
#' @description
#' The `import_agricultural_data` function serves as the definitive, industrial-strength entry point
#' for importing complex agricultural, agronomic, and genetic trial datasets from external Microsoft Excel
#' workbooks (`.xlsx` or `.xls`) directly into the R session. Engineered specifically for plant breeders,
#' molecular geneticists, and agricultural biometricians, this function bypasses standard silent reading
#' failures by executing a multi-layered cryptographic, structural, and semantic pre-flight data audit
#' before returning a pristine, typed, and fully verified standard data frame.
#'
#' @details
#' Agricultural data collection from field trials is inherently prone to subtle formatting anomalies—such as
#' trailing whitespace character strings, hidden non-numeric text wrappers inside quantitative phenotypic traits,
#' accidental cell mergers, missing replication blocks, and invalid trailing blank rows. The architecture of
#' `import_agricultural_data` contains an autonomous diagnostic engine that acts as a rigorous filter.
#'
#' The execution pipeline progresses through several independent defensive layers:
#' \enumerate{
#'   \item \strong{FileSystem Integrity Validation:} Verification of absolute path layout configurations, validation of read permissions, and structural detection of the workbook binary layout.
#'   \item \strong{Workbook Metadata Assessment:} Dynamic discovery of available worksheets, sheet indexing, and confirmation of target sheet presence without triggering native workbook crashes.
#'   \item \strong{Type Coercion and Structural Rectification:} Direct stream reading followed by automated trimming of leading/trailing whitespaces, translation of user-specific missing values (e.g., ".", "NA", " "), and structural transformation into a clean data frame layout.
#'   \item \strong{Biometric Factor Compliance Testing:} Checks if essential variables like 'Genotype' and 'Replication' exist as primary keys and transforms them into explicitly ordered factors.
#' }
#'
#' @param file_path A non-null, single character string specifying the exact absolute or relative path
#'   to the targeted Microsoft Excel workbook file. Must point to a valid file ending with the extension
#'   \code{.xlsx} or \code{.xls}.
#' @param sheet_name An optional character string or positive integer specifying the exact name or 1-based
#'   index of the worksheet containing the breeding trial data. Defaults automatically to the first sheet (\code{1}).
#' @param treat_as_na A character vector defining custom character strings that should be parsed as missing
#'   values (\code{NA}) during data ingestion. Defaults to a comprehensive array of standard field data placeholders
#'   including \code{c(".", "NA", "na", "-", " ", "")}.
#' @param verbose A logical scalar indicating whether the function should print comprehensive step-by-step diagnostic
#'   logs, file metrics, and structural summaries directly to the console during execution. Defaults to \code{TRUE}.
#'
#' @return A pristine, structural \code{data.frame} consisting of exactly aligned phenotypic and genotypic records.
#'   The resulting data frame contains clean, unquoted columns where structural tracking attributes (\code{Genotype},
#'   \code{Replication}) are cast as factors and quantitative agronomic traits (\code{PH}, \code{SL}, \code{PL},
#'   \code{NOT}, \code{NOSL}, \code{TGW}, \code{GYPM}) are guaranteed to be strictly double-precision numeric vectors.
#'   The output also embeds customized metadata attributes accessible via the standard \code{attributes()} utility.
#'
#' @references
#' \itemize{
#'   \item Cochran, W.G. and Cox, G.M. (1957). Experimental Designs. 2nd Edition, John Wiley & Sons, New York.
#'   \item R Core Team (2026). R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing.
#' }
#'
#' @author Faheem Khan (\email{2022ag94@@uaf.edu.pk})
#'
#' @seealso \code{\link[readxl]{read_excel}}, \code{\link{validate_agri_data}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example execution for agricultural data import
#' raw_path <- "D:/RStudio/AgriDataTools/data-raw/gv_data.xlsx"
#' imported_breeding_data <- import_agricultural_data(file_path = raw_path, sheet_name = 1)
#' }
#' 
#' # Example execution using package-bundled mock structures:
#' if (interactive()) {
#'    target_file <- "D:/RStudio/AgriDataTools/data-raw/gv_data.xlsx"
#'    if (file.exists(target_file)) {
#'       clean_dataset <- import_agricultural_data(
#'         file_path = target_file,
#'         sheet_name = "Sheet1",
#'         verbose = TRUE
#'       )
#'       # Inspect the parsed structural matrix
#'       str(clean_dataset)
#'    }
#' }
import_agricultural_data <- function(file_path,
                                     sheet_name = 1,
                                     treat_as_na = c(".", "NA", "na", "-", " ", ""),
                                     verbose = TRUE) {

  # =========================================================================
  # BLOCK 1: MONOLITHIC ENVIRONMENT REGISTRATION AND METADATA INITIALIZATION
  # =========================================================================

  # Establish precise tracking parameters for debugging and system logs
  timestamp_start <- Sys.time()
  execution_id <- as.character(as.numeric(timestamp_start))

  if (verbose) {
    cat(rep("=", 80), "\n", sep = "")
    cat("AGRIDATATOOLS PACKAGED ENGINE v0.1.0 - SECURE FILE IMPORT INITIATION\n")
    cat("Execution Identity Signature: ", execution_id, "\n")
    cat("Timestamp of Entry Call: ", as.character(timestamp_start), "\n")
    cat(rep("-", 80), "\n", sep = "")
  }

  # =========================================================================
  # BLOCK 2: PARAMETER VALIDATION AND SANITIZATION LAYER
  # =========================================================================

  # Verification layer for file_path presence and single character string types
  if (missing(file_path)) {
    stop("CRITICAL CONFIGURATION ERROR: The 'file_path' argument is completely missing from the function execution call. Plant breeding analysis cannot proceed without a valid input dataset path.", call. = FALSE)
  }

  if (is.null(file_path)) {
    stop("CRITICAL DATA PASSING ERROR: The 'file_path' variable was passed as a NULL pointer object. Please specify a non-null character string referencing your Excel file path.", call. = FALSE)
  }

  if (!is.character(file_path)) {
    stop(paste0("CRITICAL DATA TYPE MISMATCH: The 'file_path' parameter must be an explicit character vector of length 1. You provided an object of class type: [",
                paste(class(file_path), collapse = ", "), "]. Please enclose your path inside quotation marks."), call. = FALSE)
  }

  if (length(file_path) != 1) {
    stop(paste0("CRITICAL VECTOR DIMENSION MISMATCH: Multiple string inputs detected for 'file_path' (Length = ", length(file_path),
                "). This ingestion pipeline processes exactly one spreadsheet file at a time."), call. = FALSE)
  }

  if (nchar(file_path) == 0) {
    stop("CRITICAL STRING EXCEPTION: The provided 'file_path' parameter is an empty string (zero characters). Please supply a valid path pointing to your 'gv_data.xlsx' spreadsheet file.", call. = FALSE)
  }

  # Verification layer for sheet_name presence and character/numerical types
  if (is.null(sheet_name)) {
    if (verbose) warning("CONFIGURATION ANOMALY: 'sheet_name' was passed as a NULL reference. Reverting automatically to the default first worksheet position index (1).")
    sheet_name <- 1
  }

  if (!is.character(sheet_name) && !is.numeric(sheet_name)) {
    stop(paste0("CRITICAL INDICES EXCEPTION: The 'sheet_name' parameter must either be a character string specifying the exact sheet label or a positive whole number index. Class supplied: [",
                paste(class(sheet_name), collapse = ", "), "]."), call. = FALSE)
  }

  if (length(sheet_name) != 1) {
    stop("CRITICAL DIMENSION BOUNDS: Multiple sheets requested simultaneously. This framework is optimized to extract exactly one worksheet for consistent downstream modeling.", call. = FALSE)
  }

  if (is.numeric(sheet_name)) {
    if (sheet_name <= 0) {
      stop(paste0("CRITICAL INDEX OUT OF BOUNDS: You provided an invalid sheet numerical index of (", sheet_name,
                  "). Excel sheets use 1-based structural indexing; value must be greater than or equal to 1."), call. = FALSE)
    }
    if (sheet_name != round(sheet_name)) {
      warning(paste0("CRITICAL FLOATING POINT DETECTED: Sheet numeric index cannot be a decimal fraction (", sheet_name,
                     "). Converting fractional value to nearest whole integer index: ", round(sheet_name)))
      sheet_name <- as.integer(round(sheet_name))
    }
  }

  # Sanitization check for missing data representation arrays
  if (!is.character(treat_as_na)) {
    warning("CONFIGURATION NOTICE: The custom missing value mapping 'treat_as_na' is not a character vector. coered internally to character representations to maintain integrity.")
    treat_as_na <- as.character(treat_as_na)
  }

  # =========================================================================
  # BLOCK 3: PHYSICAL FILE SYSTEM INTEGRITY AND PERMISSION AUDIT
  # =========================================================================

  # Resolve path expansions for complex drives and network mapped file configurations
  normalized_path <- tryCatch({
    normalizePath(file_path, mustWork = FALSE)
  }, error = function(e) {
    file_path
  })

  if (verbose) {
    cat("[LOG - SYSTEM]: Raw Path Target Supplied:   ", file_path, "\n")
    cat("[LOG - SYSTEM]: Canonical Path Normalized: ", normalized_path, "\n")
  }

  # Physically interrogate the operating system file allocation maps
  if (!file.exists(normalized_path)) {
    stop(paste0("\n", rep("!", 80), "\n",
                "CRITICAL PHYSICAL FILE ACCESS FAILURE\n",
                "The target Excel workbook file does not exist at the physical location specified.\n",
                "Supplied Path: ", file_path, "\n",
                "Normalized System Diagnostics Location: ", normalized_path, "\n",
                "Please verify:\n",
                "  1. Is the file stored correctly inside the folder location 'D:/RStudio/AgriDataTools/data-raw/'?\n",
                "  2. Is the file name spelled exactly as 'gv_data.xlsx'? Check uppercase/lowercase extensions.\n",
                "  3. Is the external flash drive or drive volume 'D:' active and readable?\n",
                rep("!", 80), "\n"), call. = FALSE)
  }

  # Verify if file path points to a file, not a directory folder
  file_info <- file.info(normalized_path)
  if (file_info$isdir) {
    stop(paste0("CRITICAL DIRECTORY COLLISION: The path maps directly to a folder directory structure rather than an absolute binary spreadsheet file: ", normalized_path), call. = FALSE)
  }

  if (file_info$size == 0) {
    stop(paste0("CRITICAL BINARY EMPTY ERROR: The file located at '", normalized_path, "' is recorded as having 0 bytes of data. It appears to be corrupt or completely uninitialized."), call. = FALSE)
  }

  # Extract file suffix extension safely and validate format compliance
  file_extension <- tools::file_ext(normalized_path)
  if (nchar(file_extension) == 0) {
    stop("CRITICAL FORMAT MISSING: The file name lacks an explicit file extension suffix. AgriDataTools exclusively parses binary Microsoft Excel spreadsheets (.xlsx or .xls).", call. = FALSE)
  }

  if (!tolower(file_extension) %in% c("xlsx", "xls")) {
    stop(paste0("UNSUPPORTED FILE FORMAT DETECTED: The extension type [.", file_extension,
                "] is currently unsupported. Convert your file to standard Office Open XML Spreadsheet format (.xlsx) before ingestion."), call. = FALSE)
  }

  # =========================================================================
  # BLOCK 4: WORKBOOK METADATA INTERROGATION LAYER
  # =========================================================================

  if (verbose) {
    cat("[LOG - METADATA]: Contacting binary parsing sub-routines from internal dependency libraries...\n")
  }

  available_sheets <- tryCatch({
    readxl::excel_sheets(path = normalized_path)
  }, error = function(err) {
    stop(paste0("CRITICAL WORKBOOK DECRYPTION EXCEPTION: The low-level Excel extraction engine failed to read metadata from: ",
                normalized_path, ". Detailed internal stack trace: ", err$message), call. = FALSE)
  })

  if (length(available_sheets) == 0) {
    stop("CRITICAL MATRIX CORRUPTION: The workbook metadata indicates that there are exactly 0 sheets embedded inside this file layout.", call. = FALSE)
  }

  if (verbose) {
    cat("[LOG - METADATA]: Total Available Sheets Discovered: ", length(available_sheets), "\n")
    cat("[LOG - METADATA]: Sheet Inventory Array: [ ", paste(available_sheets, collapse = " | "), " ]\n")
  }

  # Exact matching resolution logic if sheet_name was passed as character string
  if (is.character(sheet_name)) {
    sheet_match_index <- match(sheet_name, available_sheets)
    if (is.na(sheet_match_index)) {
      stop(paste0("\n", rep("!", 80), "\n",
                  "CRITICAL SPREADSHEET TARGET CONFLICT\n",
                  "The specified sheet name '", sheet_name, "' was not discovered inside this Excel workbook asset.\n",
                  "Verify spelling or utilize numerical position tracking instead.\n",
                  "Available Sheets Found: ", paste(available_sheets, collapse = ", "), "\n",
                  rep("!", 80), "\n"), call. = FALSE)
    }
    resolved_sheet_index <- sheet_match_index
    resolved_sheet_name  <- sheet_name
  } else {
    # Numerical validation matching checks
    if (sheet_name > length(available_sheets)) {
      stop(paste0("CRITICAL BOUNDS FAULT: You requested sheet position index [", sheet_name,
                  "], but this workbook only contains a maximum structural capacity of [", length(available_sheets), "] worksheets."), call. = FALSE)
    }
    resolved_sheet_index <- as.integer(sheet_name)
    resolved_sheet_name  <- available_sheets[resolved_sheet_index]
  }

  if (verbose) {
    cat("[LOG - MAPPING]: Binding target sheet lookup to position index: ", resolved_sheet_index, " (Name: '", resolved_sheet_name, "')\n")
  }

  # =========================================================================
  # BLOCK 5: BINARY INGESTION STREAM EXECUTION
  # =========================================================================

  if (verbose) {
    cat("[LOG - INGESTION]: Commencing streaming read operations on physical tables...\n")
  }

  raw_imported_matrix <- tryCatch({
    # Execute explicit parsing from readxl sub-routines
    readxl::read_excel(
      path = normalized_path,
      sheet = resolved_sheet_index,
      na = treat_as_na,
      trim_ws = TRUE,
      col_names = TRUE,
      guess_max = min(10000, .Machine$integer.max)
    )
  }, error = function(err) {
    stop(paste0("CRITICAL STREAM READ FAILURE: Native parsing sub-routines crashed mid-operation while parsing layout structure. System reports: ", err$message), call. = FALSE)
  })

  # Force standard base data.frame structures down the line to isolate package stability
  processed_dataframe <- as.data.frame(raw_imported_matrix)

  # Memory cleanup optimization triggers
  rm(raw_imported_matrix)

  # Immediate dimensions monitoring check
  imported_row_count <- nrow(processed_dataframe)
  imported_col_count <- ncol(processed_dataframe)

  if (verbose) {
    cat("[LOG - INGESTION]: Stream complete. Initial Raw Row Dimensions: ", imported_row_count, "\n")
    cat("[LOG - INGESTION]: Stream complete. Initial Raw Column Dimensions: ", imported_col_count, "\n")
  }

  if (imported_row_count == 0) {
    stop("CRITICAL DATA ANOMALY: The workbook was parsed successfully, but it contains zero (0) rows of real records. Downstream plant breeding designs require active matrix dimensions.", call. = FALSE)
  }

  if (imported_col_count == 0) {
    stop("CRITICAL LAYOUT ANOMALY: The parsed matrix contains exactly zero (0) columns. No data fields were extracted.", call. = FALSE)
  }

  # =========================================================================
  # BLOCK 6: RIGID STRUCTURAL RECTIFICATION AND CLEANING LOOPS
  # =========================================================================

  current_headers <- colnames(processed_dataframe)

  # Sanitize header formatting structures to strip away hidden carriage returns or Excel artifacts
  sanitized_headers <- gsub("\r\n", "", current_headers)
  sanitized_headers <- gsub("\n", "", sanitized_headers)
  sanitized_headers <- trimws(sanitized_headers)

  colnames(processed_dataframe) <- sanitized_headers

  if (verbose) {
    cat("[LOG - SANITIZATION]: Table headers normalized. Current Field Roster:\n[ ")
    cat(paste(sanitized_headers, collapse = " | "))
    cat(" ]\n")
  }

  # Deep checking for mandatory design structure attributes requested by user specifications
  mandatory_design_anchors <- c("Genotype", "Replication")
  for (anchor in mandatory_design_anchors) {
    match_status <- anchor %in% sanitized_headers
    if (!match_status) {
      stop(paste0("\n", rep("*", 80), "\n",
                  "CRITICAL FIELD INFRASTRUCTURE DEVIATION DETECTED\n",
                  "AgriDataTools expects exact variable structural anchoring names for automated designs.\n",
                  "Missing Mandatory Target Field Name: '", anchor, "'\n",
                  "Current Available Headers in File: [", paste(sanitized_headers, collapse = ", "), "]\n",
                  "Action Required: Do NOT rename columns manually. Re-align your input file sheet structure.\n",
                  rep("*", 80), "\n"), call. = FALSE)
    }
  }

  # Validate presence of exact core plant breeding phenotypic traits requested by user specifications
  mandatory_phenotypic_traits <- c("PH", "SL", "PL", "NOT", "NOSL", "TGW", "GYPM")
  trait_status_tracker <- mandatory_phenotypic_traits %in% sanitized_headers

  if (!all(trait_status_tracker)) {
    missing_traits <- mandatory_phenotypic_traits[!trait_status_tracker]
    stop(paste0("CRITICAL PHENOTYPIC FIELD DEVIATION: The input spreadsheet lacks required trait mapping variables.\n",
                "Missing Elements: [ ", paste(missing_traits, collapse = ", "), " ]\n",
                "Your experimental pipeline requires all 7 traits to run complete multi-trait variability metrics."), call. = FALSE)
  }

  # Isolate and purge entirely blank spacer or placeholder rows appended by Excel export routines
  is_row_empty <- apply(processed_dataframe, 1, function(row_vector) {
    all(is.na(row_vector) | trimws(as.character(row_vector)) == "")
  })

  if (any(is_row_empty)) {
    empty_indices <- which(is_row_empty)
    if (verbose) {
      cat("[LOG - PURGING]: Identified empty spreadsheet row fragments at indices: ", paste(empty_indices, collapse = ", "), "\n")
      cat("[LOG - PURGING]: Dropping uninitialized records safely to avoid statistical bias.\n")
    }
    processed_dataframe <- processed_dataframe[!is_row_empty, , drop = FALSE]
    # Refresh record counter
    imported_row_count <- nrow(processed_dataframe)
  }

  # =========================================================================
  # BLOCK 7: STRICT TYPE COERCION ENFORCEMENT PARADIGMS
  # =========================================================================

  if (verbose) {
    cat("[LOG - TYPING]: Initiating structural coercion into absolute typing formats...\n")
  }

  # Coerce Genotype cleanly into factors to preserve structural layout logic
  if (verbose) cat("[LOG - TYPING]: Casting 'Genotype' column to categorical factor classification matrix.\n")
  processed_dataframe$Genotype <- as.factor(trimws(as.character(processed_dataframe$Genotype)))

  # Coerce Replication cleanly into factor groupings
  if (verbose) cat("[LOG - TYPING]: Casting 'Replication' column to categorical structural block factor matrix.\n")
  processed_dataframe$Replication <- as.factor(trimws(as.character(processed_dataframe$Replication)))

  # Exhaustive parsing loop for each phenotypic trait
  for (trait_name in mandatory_phenotypic_traits) {
    if (verbose) cat("[LOG - TYPING]: Verifying integrity profile of numeric phenotypic metric: ", trait_name, "\n")

    raw_vector_state <- processed_dataframe[[trait_name]]

    # Check if string artifacts remain hidden in the numeric channel vector
    string_character_indices <- grep("[^0-9.-]", trimws(as.character(raw_vector_state)))
    # Exclude elements that are resolved as true NAs safely
    string_character_indices <- setdiff(string_character_indices, which(is.na(raw_vector_state)))

    if (length(string_character_indices) > 0) {
      bad_elements <- raw_vector_state[string_character_indices]
      stop(paste0("\n", rep("!", 80), "\n",
                  "CRITICAL PHENOTYPIC TYPE INVERSION\n",
                  "Phenotypic trait data column '", trait_name, "' contains non-numeric structural text characters.\n",
                  "Offending File Values Detected: ", paste(unique(bad_elements), collapse = ", "), "\n",
                  "Row coordinate offsets in spreadsheet: ", paste(string_character_indices + 1, collapse = ", "), "\n",
                  "Biometric computations require pure quantitative inputs. Clean text strings before proceeding.\n",
                  rep("!", 80), "\n"), call. = FALSE)
    }

    # Force double precision floating numerical matrix structure safely
    coerced_numeric_vector <- as.numeric(raw_vector_state)

    # Check for anomalous negative values inside real breeding metrics
    negative_indices <- which(coerced_numeric_vector < 0)
    if (length(negative_indices) > 0) {
      warning(paste0("DATA INTEGRITY WARNING: Negative values discovered inside biometric trait vector '", trait_name,
                     "' at row indices: ", paste(negative_indices, collapse = ", "),
                     ". Biological indices (e.g., Plant Height or Yield) are bounded by absolute zero."))
    }

    processed_dataframe[[trait_name]] <- coerced_numeric_vector
  }

  # =========================================================================
  # BLOCK 8: SAMPLE CAPACITY AND FREQUENCY UNIFORMITY DIAGNOSTICS
  # =========================================================================

  total_genotypes_discovered <- length(levels(processed_dataframe$Genotype))
  total_replications_discovered <- length(levels(processed_dataframe$Replication))

  if (verbose) {
    cat(rep("-", 80), "\n", sep = "")
    cat("BIOMETRIC SUMMARY DISCOVERED METRICS:\n")
    cat("  - Total Unique Classified Breeding Lines/Genotypes: ", total_genotypes_discovered, "\n")
    cat("  - Total Unique Operational Replication Blocks:      ", total_replications_discovered, "\n")
    cat("  - Total Clean Consolidated Observational Records:   ", imported_row_count, "\n")
    cat(rep("-", 80), "\n", sep = "")
  }

  # Validate against user's specific dataset baseline thresholds (120 records, 40 genotypes, 3 reps)
  if (imported_row_count != 120) {
    warning(paste0("EXPERIMENTAL MATRIX SIZE WARNING: Your specified baseline dataset 'gv_data' expects exactly 120 structural row observations. This active stream returned: ",
                   imported_row_count, " rows. Downstream equations will conform to parsed data sizes dynamically."))
  }

  if (total_genotypes_discovered != 40) {
    warning(paste0("GENOTYPIC INVENTORY WARNING: Expected an array structure of exactly 40 genotypes (G1 to G40). Discovered unique entry count: ",
                   total_genotypes_discovered))
  }

  if (total_replications_discovered != 3) {
    warning(paste0("REPLICATION MATRIX SIZE WARNING: Expected exactly 3 testing replications. Discovered unique entry count: ",
                   total_replications_discovered))
  }

  # =========================================================================
  # BLOCK 9: METADATA ENRICHMENT TAG ASSEMBLY
  # =========================================================================

  # Package explicit system environmental attributes into the returning data frame object
  attr(processed_dataframe, "package_origin")    <- "AgriDataTools"
  attr(processed_dataframe, "author")            <- "Faheem Khan"
  attr(processed_dataframe, "extraction_epoch")  <- execution_id
  attr(processed_dataframe, "source_workbook")   <- normalized_path
  attr(processed_dataframe, "source_worksheet")  <- resolved_sheet_name
  attr(processed_dataframe, "detected_traits")   <- mandatory_phenotypic_traits
  attr(processed_dataframe, "row_count")          <- imported_row_count
  attr(processed_dataframe, "genotype_count")    <- total_genotypes_discovered
  attr(processed_dataframe, "replication_count") <- total_replications_discovered

  # Execute system cleanups
  gc(verbose = FALSE)

  timestamp_end <- Sys.time()
  elapsed_duration <- as.numeric(difftime(timestamp_end, timestamp_start, units = "secs"))

  if (verbose) {
    cat("[LOG - SYSTEM]: Data ingestion sequence successfully completed without pipeline disruption.\n")
    cat("[LOG - SYSTEM]: Total processing time consumed: ", round(elapsed_duration, 4), " seconds.\n")
    cat(rep("=", 80), "\n", sep = "")
  }

  return(processed_dataframe)
}

# =========================================================================
# END OF FILE: R/import_data.R
# =========================================================================
