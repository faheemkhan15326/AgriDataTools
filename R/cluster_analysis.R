#' Hierarchical Cluster Analysis and Phenotypic Diversity Engine
#'
#' @description
#' The `analyze_clustering` function executes a high-precision agglomerative hierarchical
#' clustering routine over multi-trait breeding matrices. It establishes distance networks,
#' builds tree structures, and tracks cophenetic mapping correlations.
#'
#' @param data A verified \code{data.frame} containing phenotypic records.
#' @param traits A character vector specifying the quantitative trait columns to be integrated.
#' @param linkage_method A character string indicating the target clustering algorithm (e.g., "ward.D2", "complete"). Defaults to \code{"ward.D2"}.
#' @param reporting_level An integer vector flag defining console trace settings. Defaults to \code{2}.
#'
#' @return A structured \code{list} containing the cluster diagnostics details:
#'   \item{dist_matrix}{The absolute calculated multidimensional Euclidean spatial distance object.}
#'   \item{hc_object}{The raw hclust class tree payload configuration.}
#'   \item{cophenetic_corr}{The isolated scalar coefficient validation metric.}
#'   \item{genotype_means}{The underlying compressed line-wise values used across data frames.}
#'
#' @author Faheem Khan (\email{2022ag94@@uaf.edu.pk})
#'
#' @examples
#' # Load integrated genuine data matrix containing all 40 cultivars
#' data(gv_data, package = "AgriDataTools")
#' 
#' # Define exhaustive phenotypic traits vector for complete matrix matching
#' complete_traits <- c("PH", "SL", "PL", "NOT", "NOSS", "TGW", "GYPM")
#' 
#' # Run hierarchical cluster engine across all 40 lines and 7 traits
#' cluster_results <- analyze_clustering(
#'   data = gv_data,
#'   traits = complete_traits,
#'   linkage_method = "ward.D2"
#' )
#' 
#' # Extract total compressed line-wise values configuration
#' print(head(cluster_results$genotype_means))
#' @export
analyze_clustering <- function(data, traits, linkage_method = "ward.D2", reporting_level = 2) {
  timestamp_start <- Sys.time()
  
  # Trait code name mapping dictionary
  trait_lookup <- function(code) {
    switch(trimws(toupper(code)),
           "PH"   = "Plant Height",
           "SL"   = "Spike Length",
           "PL"   = "Peduncle Length",
           "NOT"  = "Number of Tillers",
           "NOSS" = "Number of Spikelets per Spike",
           "TGW"  = "Thousand Grain Weight",
           "GYPM" = "Grain Yield per Meter",
           code
    )
  }
  
  if (reporting_level >= 1) {
    cat(rep("=", 85), "\n", sep = "")
    cat("AGRIDATATOOLS PACKAGED ENGINE v0.1.0 - HIERARCHICAL CLUSTER MATRIX EVALUATION\n")
    cat("Analysis Inception: ", as.character(timestamp_start), "\n")
    cat(rep("-", 85), "\n", sep = "")
  }
  
  if (missing(data) || missing(traits)) {
    stop("CRITICAL CLUSTER FAULT: Missing core arguments.", call. = FALSE)
  }
  
  missing_cols <- traits[!traits %in% colnames(data)]
  if (length(missing_cols) > 0) {
    stop(paste("METADATA SYNTAX CRASH: Traits missing from source frame:", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  
  valid_methods <- c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")
  if (!linkage_method %in% valid_methods) {
    stop(paste("PARAMETER ERROR: Invalid linkage method:", linkage_method), call. = FALSE)
  }
  
  if ("Genotype" %in% colnames(data)) {
    data$Genotype <- as.factor(data$Genotype)
    formula_string <- as.formula(paste("cbind(", paste(traits, collapse = ","), ") ~ Genotype"))
    aggregated_means <- aggregate(formula_string, data = data, FUN = mean, na.rm = TRUE)
    row_identifiers <- as.character(aggregated_means$Genotype)
    numerical_matrix <- aggregated_means[, traits, drop = FALSE]
  } else {
    aggregated_means <- na.omit(data[, traits, drop = FALSE])
    row_identifiers <- as.character(1:nrow(aggregated_means))
    numerical_matrix = aggregated_means
  }
  
  # Set full publication names for clarity across terminal view matrix arrays
  colnames(numerical_matrix) <- sapply(colnames(numerical_matrix), trait_lookup)
  standardized_scores <- scale(numerical_matrix)
  
  # Duplicate identifiers ko unique banayein
  rownames(standardized_scores) <- make.unique(row_identifiers)
  
  euclidean_distance_matrix <- dist(standardized_scores, method = "euclidean")
  hierarchical_tree <- hclust(euclidean_distance_matrix, method = linkage_method)
  hierarchical_tree$labels <- row_identifiers
  
  cophenetic_distances <- cophenetic(hierarchical_tree)
  cophenetic_correlation_val <- cor(euclidean_distance_matrix, cophenetic_distances)
  
  if (reporting_level >= 1) {
    cat("\n", rep("-", 75), "\n", sep = "")
    cat(" CLUSTER ACCURACY DICTIONARY AND MATRIX VALIDATION\n")
    cat(rep("-", 75), "\n", sep = "")
    cat("  Selected Linkage Algorithm     : ", linkage_method, "\n")
    cat("  Total Clustered Elements Count : ", length(row_identifiers), " (Genotypes)\n")
    cat("  Cophenetic Metric Fit Score    : ", round(cophenetic_correlation_val, 5), "\n")
    cat("  Mapped Characters Evaluated    :\n")
    cat("    - ", paste(sapply(traits, trait_lookup), collapse = "\n    - "), "\n")
    cat(rep("=", 85), "\n\n", sep = "")
  }
  
  return(list(
    dist_matrix     = euclidean_distance_matrix,
    hc_object       = hierarchical_tree,
    cophenetic_corr = cophenetic_correlation_val,
    genotype_means  = aggregated_means
  ))
}