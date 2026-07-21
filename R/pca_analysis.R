#' Principal Component Analysis (PCA) Evaluation Engine
#'
#' @description
#' The `analyze_pca` function executes a high-precision multivariate principal component
#' transformation over agronomic datasets. It extracts exact eigenvalues, computes variance
#' percentages, documents rotation loads, and outputs exhaustive coordinate structures.
#'
#' @param data A verified \code{data.frame} containing the target quantitative matrix rows.
#' @param traits A character vector specifying the column headers of quantitative variables to evaluate.
#' @param scale A logical value indicating whether the variables should be standardized. Defaults to \code{TRUE}.
#' @param reporting_level An integer vector flag defining console trace settings. Defaults to \code{2}.
#'
#' @return A structured \code{list} containing the principal components output arrays:
#'   \item{pca_object}{The raw prcomp execution payload configuration.}
#'   \item{eigenvalues}{A structured data frame containing calculated eigenvalues and variance limits.}
#'   \item{loadings}{The precise rotation matrix mapping trait load coordinates.}
#'   \item{scores}{Data frame containing calculated individual components scores matched with Genotypes.}
#'
#' @author Faheem Khan (\email{2022ag94@@uaf.edu.pk})
#'
#' @examples
#' # Load integrated genuine breeding records containing 40 genotypes
#' data(gv_data, package = "AgriDataTools")
#' 
#' # Specify targeted multi-trait matrix headers containing all 7 metrics
#' complete_traits <- c("PH", "SL", "PL", "NOT", "NOSS", "TGW", "GYPM")
#' 
#' # Run multivariate singular value decomposition transformation pipeline
#' pca_results <- analyze_pca(
#'   data = gv_data,
#'   traits = complete_traits,
#'   scale = TRUE
#' )
#' 
#' # View all component eigenvalues and cumulative contribution summaries
#' print(pca_results$eigenvalues)
#' @export
analyze_pca <- function(data, traits, scale = TRUE, reporting_level = 2) {
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
    cat("AGRIDATATOOLS PACKAGED ENGINE v0.1.0 - MULTIVARIATE PCA DECOMPOSITION PIPELINE\n")
    cat("Analysis Inception: ", as.character(timestamp_start), "\n")
    cat(rep("-", 85), "\n", sep = "")
  }
  
  if (missing(data) || missing(traits)) {
    stop("CRITICAL PCA FAULT: Missing core arguments.", call. = FALSE)
  }
  
  target_matrix <- data[, traits, drop = FALSE]
  complete_rows <- complete.cases(target_matrix)
  clean_matrix <- target_matrix[complete_rows, , drop = FALSE]
  
  for (col in colnames(clean_matrix)) {
    clean_matrix[[col]] <- as.numeric(clean_matrix[[col]])
  }
  
  # Remap short codes to original explicit terminology names
  colnames(clean_matrix) <- sapply(colnames(clean_matrix), trait_lookup)
  
  pca_execution <- prcomp(clean_matrix, center = TRUE, scale. = scale)
  
  calculated_eigenvalues <- pca_execution$sdev^2
  total_variance_sum <- sum(calculated_eigenvalues)
  variance_proportions <- (calculated_eigenvalues / total_variance_sum) * 100
  cumulative_variance <- cumsum(variance_proportions)
  
  eigen_summary_table <- data.frame(
    "Component"          = paste0("PC", 1:length(calculated_eigenvalues)),
    "Eigenvalue"         = calculated_eigenvalues,
    "Variance_Percent"   = variance_proportions,
    "Cumulative_Percent" = cumulative_variance,
    stringsAsFactors     = FALSE
  )
  
  factor_loadings_matrix <- as.data.frame(pca_execution$rotation)
  individual_scores <- as.data.frame(pca_execution$x)
  
  if ("Genotype" %in% colnames(data)) {
    genotype_labels <- as.character(data$Genotype[complete_rows])
    # Duplicate row names ko handle karne ke liye make.unique ka istemal
    rownames(individual_scores) <- make.unique(genotype_labels) 
    individual_scores <- cbind(
      "Genotype" = as.factor(genotype_labels),
      individual_scores,
      stringsAsFactors = FALSE
    )
  }
  
  if (reporting_level >= 1) {
    cat("\n", rep("-", 75), "\n", sep = "")
    cat(" PRINCIPAL COMPONENT MULTIVARIATE SUMMARY EXTRACTION\n")
    cat(rep("-", 75), "\n", sep = "")
    
    # Sirf numeric columns ko round karein taake Math.data.frame error na aaye
    eigen_output <- eigen_summary_table
    numeric_cols <- sapply(eigen_output, is.numeric)
    eigen_output[, numeric_cols] <- round(eigen_output[, numeric_cols], 5)
    
    print(eigen_output, row.names = FALSE)
    cat("\n Trait Eigenvector Loadings Matrix Structure:\n")
    print(round(factor_loadings_matrix, 5))
    cat(rep("=", 85), "\n\n", sep = "")
  }
  
  return(list(
    pca_object = pca_execution,
    eigenvalues = eigen_summary_table,
    loadings = factor_loadings_matrix,
    scores = individual_scores
  ))
}