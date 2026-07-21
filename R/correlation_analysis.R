#' High-Precision Multi-Trait Phenotypic Correlation Analysis Engine
#'
#' @description
#' The `compute_correlation` function executes a robust statistical correlation sweep across multiple
#' numeric phenotypic traits. It calculates the correlation coefficient matrix (Pearson, Spearman, or Kendall)
#' alongside systematic t-tests to compile comprehensive probability significance ($p$-value) matrices.
#'
#' @details
#' Dissecting the co-inheritance or directional association between plant architectural traits and final
#' grain yield is a cornerstone of plant breeding. This engine evaluates associations using standard
#' algebraic covariance partitions:
#' \deqn{r = \frac{\sum (X_i - \bar{X})(Y_i - \bar{Y})}{\sqrt{\sum (X_i - \bar{X})^2 \sum (Y_i - \bar{Y})^2}}}
#' The significance of each coefficient is tested against the Student's t-distribution with \eqn{n - 2}
#' degrees of freedom:
#' \deqn{t = \frac{r \sqrt{n - 2}}{\sqrt{1 - r^2}}}
#' The function maps these metrics into clear matrices, ready for cascading downstream path analyses.
#'
#' @param data A verified \code{data.frame} containing the phenotypic experimental records.
#' @param traits A character vector specifying the numeric trait columns to correlate. If \code{NULL},
#'   the system automatically isolates all non-factor numeric fields. Defaults to \code{NULL}.
#' @param method A character string defining the calculation model: \code{"pearson"}, \code{"spearman"},
#'   or \code{"kendall"}. Defaults to \code{"pearson"}.
#' @param reporting_level An integer vector flag defining console trace settings: \code{0} for silent,
#'   \code{1} for critical matrices, and \code{2} for exhaustive evaluation reports. Defaults to \code{2}.
#'
#' @return A structured \code{list} containing the multi-layered matrix outputs:
#'   \item{correlation_matrix}{A symmetric data frame containing the calculated correlation coefficients ($r$).}
#'   \item{p_value_matrix}{A symmetric data frame containing the exact calculated two-tailed $p$-values.}
#'   \item{significance_matrix}{A structural matrix mapping significance flags (*, **, ***, ns) for direct publication layouts.}
#'
#' @author Faheem Khan (\email{2022ag94@uaf.edu.pk})
#'
#' @seealso \code{\link{compute_summary_stats}}, \code{\link{compute_path_analysis}}
#'
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' \dontrun{
#'   # Compute standard Pearson phenotypic correlation matrices
#'   corr_payload <- compute_correlation(data = gv_data, method = "pearson")
#' }
compute_correlation <- function(data, traits = NULL, method = "pearson", reporting_level = 2) {
  
  timestamp_start <- Sys.time()
  
  if (reporting_level >= 1) {
    cat(rep("=", 85), "\n", sep = "")
    cat("AGRIDATATOOLS PACKAGED ENGINE v0.1.0 - PHENOTYPIC CORRELATION ENGINE\n")
    cat("Analysis Inception: ", as.character(timestamp_start), "\n")
    cat(rep("-", 85), "\n", sep = "")
  }
  
  if (missing(data) || !is.data.frame(data)) {
    stop("CRITICAL MATRIX FAULT: Provided input data must be a valid structured data frame.", call. = FALSE)
  }
  
  method <- match.arg(tolower(method), c("pearson", "spearman", "kendall"))
  
  if (is.null(traits)) {
    ignore_fields <- c("Genotype", "Replication", "Rep", "Block", "Line", "Cultivar")
    all_cols      <- colnames(data)
    numeric_cols  <- all_cols[sapply(data, is.numeric)]
    traits        <- setdiff(numeric_cols, ignore_fields)
    
    if (length(traits) < 2) {
      stop("DISCOVERY FAULT: Insufficient numeric trait vectors found.", call. = FALSE)
    }
  }
  
  working_matrix <- na.omit(data.matrix(data[, traits, drop = FALSE]))
  n_samples      <- nrow(working_matrix)
  num_traits     <- length(traits)
  
  if (n_samples < 4) {
    stop("DATA TRUNCATION FAULT: Insufficient complete pairwise observations.", call. = FALSE)
  }
  
  if (reporting_level >= 2) {
    cat("[DIAGNOSTIC - CORRELATION]: Processing ", method, " arrays for ", num_traits, " traits...\n")
  }
  
  r_matrix     <- cor(working_matrix, method = method)
  p_matrix     <- matrix(0, nrow = num_traits, ncol = num_traits, dimnames = list(traits, traits))
  stars_matrix <- matrix("ns", nrow = num_traits, ncol = num_traits, dimnames = list(traits, traits))
  
  for (i in 1:num_traits) {
    for (j in i:num_traits) {
      if (i == j) {
        stars_matrix[i, j] <- "-"
        next
      }
      r_val <- r_matrix[i, j]
      t_stat <- (r_val * sqrt(n_samples - 2)) / sqrt(1 - r_val^2)
      p_val  <- 2 * pt(abs(t_stat), df = n_samples - 2, lower.tail = FALSE)
      
      p_matrix[i, j] <- p_matrix[j, i] <- p_val
      flag <- if (p_val <= 0.001) "***" else if (p_val <= 0.01) "**" else if (p_val <= 0.05) "*" else "ns"
      stars_matrix[i, j] <- stars_matrix[j, i] <- flag
    }
  }
  
  if (reporting_level >= 1) {
    cat("\n PHENOTYPIC CORRELATION ANALYSIS (N = ", n_samples, ")\n", sep = "")
    cat(rep("-", 50), "\n", sep = "")
    print(round(r_matrix, 4))
    cat("\n Significance Matrix:\n")
    print(stars_matrix, quote = FALSE)
    cat(rep("=", 50), "\n\n", sep = "")
  }
  
  return(invisible(list(
    correlation_matrix  = as.data.frame(r_matrix),
    p_value_matrix      = as.data.frame(p_matrix),
    significance_matrix = as.data.frame(stars_matrix)
  )))
}