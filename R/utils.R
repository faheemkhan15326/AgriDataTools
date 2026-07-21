#' @importFrom stats lm anova as.formula cor pt qt qf pf var sd qtukey ptukey prcomp dist hclust cutree kmeans aggregate qqnorm qqline complete.cases cophenetic reorder
#' @importFrom graphics abline arrows axis image par text
#' @importFrom grDevices heat.colors
#' @importFrom utils head
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Genotype", "Mean", "Grouping", "PC1", "PC2", 
    "variable_tags", "x", "y", "xend", "yend", "label"
  ))
}

#' Compute Standard Error (SE) and Critical Difference (CD/LSD)
#'
#' @description
#' Evaluated standard error (SE) of the mean and documented critical difference (CD) 
#' for phenotypic traits. Supports direct vector calculation for traits like Plant 
#' Height (PH) or ANOVA-based calculations using Mean Square Error (MSE).
#'
#' @param x A numeric vector of phenotypic observations (optional).
#' @param mean_square_error A numeric scalar representing MSE.
#' @param total_replications An integer scalar representing total replications.
#' @param na.rm Logical, default TRUE.
#'
#' @return A numeric scalar representing the calculated Standard Error.
#'
#' @author Faheem Khan
#'
#' @examples
#' data(gv_data, package = "AgriDataTools")
#' 
#' # Example 1: Analyzed standard error for Plant Height (PH)
#' se_ph <- compute_standard_error(x = gv_data$PH)
#' print(paste("Standard Error for Plant Height:", round(se_ph, 4)))
#' 
#' @export
compute_standard_error <- function(x = NULL, mean_square_error = NULL, total_replications = NULL, na.rm = TRUE) {
  
  # Mode 1: Vector-based SE calculation
  if (!is.null(x)) {
    sd_val <- sd(x, na.rm = na.rm)
    n_val <- length(if(na.rm) x[!is.na(x)] else x)
    return(sd_val / sqrt(n_val))
    
    # Mode 2: ANOVA-based SE of Difference
  } else if (!is.null(mean_square_error) && !is.null(total_replications)) {
    if (mean_square_error <= 0 || total_replications <= 0) {
      stop("UTILITY ERROR: Mean square error and replications must be strictly positive.", call. = FALSE)
    }
    return(sqrt((2 * mean_square_error) / total_replications))
    
  } else {
    stop("Provide either 'x' (vector) OR 'mean_square_error' and 'total_replications'.")
  }
}

#' Compute Critical Difference
#' 
#' @param mean_square_error Numeric. The MSE from ANOVA.
#' @param total_replications Numeric. Total number of replications.
#' @param error_degrees_of_freedom Numeric. Error degrees of freedom.
#' @param significance_level Numeric. Alpha level, defaults to 0.05.
#' 
#' @examples
#' # Example: Calculated CD/LSD for Grain Yield
#' cd_yield <- compute_critical_difference(mean_square_error = 0.85, 
#'                                         total_replications = 3, 
#'                                         error_degrees_of_freedom = 12)
#' print(paste("Critical Difference:", round(cd_yield, 4)))
#' 
#' @export
compute_critical_difference <- function(mean_square_error, total_replications, error_degrees_of_freedom, significance_level = 0.05) {
  
  # Calculate SED using the upgraded function
  se_diff <- compute_standard_error(mean_square_error = mean_square_error, total_replications = total_replications)
  
  t_crit <- qt(1 - (significance_level / 2), df = error_degrees_of_freedom)
  return(t_crit * se_diff)
}