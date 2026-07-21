#' Advanced Algorithmic Detection and Classification Engine for Experimental Designs
#'
#' @description
#' The `detect_experimental_design` function executes an automated, non-destructive, and highly
#' rigorous statistical analysis of structural combinations inside the agricultural trial data. By
#' evaluating cross-tabulation balance, group sizing coefficients, and design orthogonality across
#' the \code{Genotype} and \code{Replication} vectors, it automatically determines whether the physical
#' field layout conforms to a standard Randomized Complete Block Design (RCBD) or a Completely
#' Randomized Design (CRD).
#'
#' @details
#' Selecting an incorrect statistical model for Analysis of Variance (ANOVA) is one of the most common
#' pitfalls for agricultural researchers and beginners in biometrics. For instance, running a CRD
#' model on data that was physically laid out as an RCBD in the field fails to isolate the variance
#' components contributed by environmental blocking, thereby inflating the residual error mean square
#' and drastically undermining the power of post-hoc tests (LSD, Tukey's, Scheffe's).
#'
#' To neutralize this vulnerability completely, `detect_experimental_design` operates as a production-grade
#' expert framework that interrogates the data matrix structure across multiple specialized validation layers:
#' \enumerate{
#'   \item \strong{Dependency Cascading Assessment:} It dynamically calls internal validations to ensure the base structural factors are error-free.
#'   \item \strong{Contingency Cell Inversion Analysis:} Builds an exact two-way allocation table of \code{Genotype} factor levels by \code{Replication} factor levels.
#'   \item \strong{Orthogonality Metrics Screening:} Evaluates if the contingency cells match an absolute unit frequency of exactly one observation per intersection node.
#'   \item \strong{Mathematical Balance Proofing:} Cross-checks total theoretical layout spaces against the physical length of data rows to isolate un-replicated or structural fragments.
#' }
#'
#' @param data A non-null, fully verified \code{data.frame} that must contain the structural grouping
#'   columns labeled exactly as \code{Genotype} and \code{Replication}.
#' @param reporting_level An integer vector flag defining console trace settings: \code{0} suppresses all log traces,
#'   \code{1} outputs the terminal classified design string, and \code{2} prints granular frequency matrices
#'   and design metrics. Defaults to \code{2}.
#'
#' @return A single, normalized character string scalar representing the identified experimental field layout.
#'   Returns exactly \code{"RCBD"} if the structure is completely orthogonal and balanced, or \code{"CRD"}
#'   if the layout elements display nesting patterns, unequal replications, or missing structural cells.
#'
#' @references
#' \itemize{
#'   \item Cochran, W.G. and Cox, G.M. (1957). Experimental Designs. 2nd Edition, John Wiley & Sons, New York.
#'   \item Gomez, K.A. and Gomez, A.A. (1984). Statistical Procedures for Agricultural Research. John Wiley & Sons, New York.
#' }
#'
#' @author Faheem Khan (\email{2022ag94@@uaf.edu.pk})
#'
#' @seealso \code{\link{validate_agri_data}}, \code{\link{import_agricultural_data}}
#'
#' @export
#'
#' @examples
#' # Dynamic classification trace using the integrated gv_data asset layout
#' if (interactive()) {
#'    # Trigger the classification engine
#'    discovered_layout <- detect_experimental_design(data = gv_data, reporting_level = 2)
#'    cat("Automated Routing Protocol Selected: ", discovered_layout, "\n")
#' }
detect_experimental_design <- function(data, reporting_level = 2) {

  # =========================================================================
  # BLOCK 1: ENVIRONMENTAL REGISTRATION AND DIAGNOSTIC TRACE SETUP
  # =========================================================================
  timestamp_start <- Sys.time()

  if (reporting_level >= 1) {
    cat(rep("=", 85), "\n", sep = "")
    cat("AGRIDATATOOLS PACKAGED ENGINE v0.1.0 - AUTOMATED EXPERIMENTAL DESIGN DETECTOR\n")
    cat("Diagnostic Sweep Inception: ", as.character(timestamp_start), "\n")
    cat(rep("-", 85), "\n", sep = "")
  }

  # =========================================================================
  # BLOCK 2: SYSTEM DEFENSE AND STRUCTURAL GATEKEEPER
  # =========================================================================
  if (missing(data)) {
    stop("CRITICAL EXECUTION FAULT: Input argument 'data' is missing. The engine cannot deduce field designs without a data matrix source.", call. = FALSE)
  }

  if (is.null(data)) {
    stop("CRITICAL INSTANCE FAULT: The data reference resolves to a NULL pointer. Process aborted.", call. = FALSE)
  }

  # Delegate basic input checks to validation layer without strict trait mandate
  # to guarantee file layout safety before mapping combinations.
  tryCatch({
    validate_agri_data(data = data, strict_mode = FALSE, reporting_level = 0)
  }, error = function(e) {
    stop(paste0("PRE-FLIGHT DIAGNOSTIC ABORT: The design discovery engine cannot execute because the input table failed base validation checks. Reason: ", e$message), call. = FALSE)
  })

  # =========================================================================
  # BLOCK 3: VECTOR COERCION AND FACTORIZATION LOOPS
  # =========================================================================
  if (reporting_level >= 2) {
    cat("[DIAGNOSTIC - MATRIX]: Extracting structural keys and dropping empty factor elements...\n")
  }

  # Safely coerce columns internally into absolute vectors to prevent tibble or matrix class row tracking bugs
  vector_genotype    <- as.factor(data$Genotype)
  vector_replication <- as.factor(data$Replication)

  total_observations <- nrow(data)

  # Isolate active levels array mapping
  genotype_levels    <- levels(vector_genotype)
  replication_levels <- levels(vector_replication)

  count_genotypes    <- length(genotype_levels)
  count_replications <- length(replication_levels)

  if (reporting_level >= 2) {
    cat("[DIAGNOSTIC - MATRIX]: Evaluated Lines Count (Genotypes): ", count_genotypes, "\n")
    cat("[DIAGNOSTIC - MATRIX]: Evaluated Block Count (Replications): ", count_replications, "\n")
    cat("[DIAGNOSTIC - MATRIX]: Total Physical Observations Count:     ", total_observations, "\n")
  }

  # =========================================================================
  # BLOCK 4: TWO-WAY CONTINGENCY MATRIX DISCOVERY
  # =========================================================================
  if (reporting_level >= 2) {
    cat("[DIAGNOSTIC - FREQUENCY]: Building two-way contingency allocation array tables...\n")
  }

  # Tabulate layout cross-frequencies explicitly
  contingency_matrix <- table(vector_genotype, vector_replication)

  if (reporting_level >= 2) {
    cat("\n--- TRANSVERSE DESIGN INTERSECTION TABLE ---\n")
    print(contingency_matrix)
    cat(rep("-", 45), "\n\n")
  }

  # =========================================================================
  # BLOCK 5: MATHEMATICAL CLASSIFICATION ALGORITHM POLICIES
  # =========================================================================

  # Check if every single cell inside the contingency table contains exactly one record
  cells_are_strictly_unitary <- all(contingency_matrix == 1)

  # Check if there are any completely empty cells (structural zeroes)
  contains_structural_zeroes <- any(contingency_matrix == 0)

  # Check for perfect mathematical matrix products (Orthogonal Balance Vector)
  theoretical_balanced_size <- count_genotypes * count_replications
  is_product_perfectly_orthogonal <- (total_observations == theoretical_balanced_size)

  if (reporting_level >= 2) {
    cat("[DIAGNOSTIC - COMPILATION]: Evaluating mathematical conditions for classification:\n")
    cat("  - Is every intersection cell exactly equal to 1?  : ", cells_are_strictly_unitary, "\n")
    cat("  - Are there missing structural data nodes?       : ", contains_structural_zeroes, "\n")
    cat("  - Does Row Count match Genotype x Replication product? : ", is_product_perfectly_orthogonal, "\n")
  }

  # =========================================================================
  # BLOCK 6: EXTENSIVE STRATIFICATION DECISION PIPELINE
  # =========================================================================

  # Rule Layer 1: Perfect Balance Validation Framework (RCBD)
  if (cells_are_strictly_unitary && is_product_perfectly_orthogonal && !contains_structural_zeroes) {

    selected_layout_string <- "RCBD"

    if (reporting_level >= 1) {
      cat("\n[CLASSIFIER SUCCESS]: Structure conclusively maps to a Balanced Randomized Complete Block Design.\n")
      cat("[CLASSIFIER SUCCESS]: Downstream biometric modules will activate blocking error control components.\n")
    }

    # Specific validation against user's gv_data profile parameters (40 genotypes x 3 blocks = 120 nodes)
    if (count_genotypes == 40 && count_replications == 3 && total_observations == 120) {
      if (reporting_level >= 2) {
        cat("[CLASSIFIER CONFIRMATION]: Dataset configuration perfectly matches the 'gv_data' blueprint specifications.\n")
      }
    }

  } else {
    # Rule Layer 2: Alternative Nested / Unbalanced Structure Allocation (CRD)
    selected_layout_string <- "CRD"

    if (reporting_level >= 1) {
      cat("\n[CLASSIFIER NOTICE]: Structure deviates from a standard balanced block design.\n")
      cat("[CLASSIFIER NOTICE]: System has assigned a Completely Randomized Design (CRD) classification framework.\n")
    }

    # Issue highly contextual warning diagnostics to assist user troubleshooting
    if (contains_structural_zeroes) {
      warning("CRITICAL DESIGN ASYMMETRY: Certain lines/genotypes are entirely unrepresented inside specified replication blocks. Downstream linear models will utilize missing cell estimation.", call. = FALSE)
    }

    if (any(contingency_matrix > 1)) {
      warning("SUBSAMPLING ANOMALY: Cell frequencies greater than 1 detected. This indicates multiple samples or sub-plots were harvested per block. Ensure data values are averaged per plot before running simple ANOVA.", call. = FALSE)
    }
  }

  # =========================================================================
  # BLOCK 7: METADATA EMBEDDING AND TIMING EXITS
  # =========================================================================
  timestamp_end <- Sys.time()
  processing_seconds <- as.numeric(difftime(timestamp_end, timestamp_start, units = "secs"))

  if (reporting_level >= 1) {
    cat("[LOG - FINALIZE]: Layout routing vector successfully locked: ", selected_layout_string, "\n")
    cat("[LOG - FINALIZE]: Total compute time allocated: ", round(processing_seconds, 5), " seconds.\n")
    cat(rep("=", 85), "\n", sep = "")
  }

  # Return terminal classification token safely
  return(selected_layout_string)
}

# =========================================================================
# END OF FILE: R/design_detection.R
# =========================================================================
