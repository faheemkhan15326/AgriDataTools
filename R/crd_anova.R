#' Comprehensive Analysis of Variance (ANOVA) Engine for Completely Randomized Design (CRD)
#'
#' @description
#' The `anova_crd` function executes a complete, high-precision linear model analysis for
#' agricultural, laboratory, or greenhouse trials laid out under a Completely Randomized Design (CRD).
#' It computes partition sums of squares, hypothesis testing statistics, treatment variances,
#' significance flags, and the Coefficient of Variation (CV%).
#'
#' @details
#' In laboratory experiments, growth chamber studies, or field trials with completely homogeneous
#' environments, blocking is unnecessary. This function utilizes standard least-squares projection
#' to build the classic orthogonal CRD ANOVA matrix, modeling the response vector as a function
#' of treatment effects without blocking constraints:
#' \deqn{Y_{ij} = \mu + T_i + \varepsilon_{ij}}
#' Where \eqn{T_i} represents the treatment/genotype effect, and \eqn{\varepsilon_{ij}} is the
#' residual experimental error. The function handles both balanced and unbalanced data structures
#' perfectly, ensuring proper adjustments to degrees of freedom if replication numbers vary across lines.
#'
#' @param data A verified \code{data.frame} containing the columns \code{Genotype} and the
#'   target phenotypic trait response.
#' @param trait A single character string specifying the exact column name of the numeric trait to analyze.
#' @param reporting_level An integer vector flag defining console trace settings: \code{0} for silent,
#'   \code{1} for basic summary tables, and \code{2} for comprehensive descriptive metrics. Defaults to \code{2}.
#'
#' @return A structured \code{list} containing:
#'   \item{anova_table}{A data.frame acting as the standard ANOVA source matrix table for CRD.}
#'   \item{cv_percentage}{The computed Coefficient of Variation percentage scalar.}
#'   \item{mean_square_error}{The isolated Residual Error Mean Square (EMS), ready for downstream evaluation.}
#'   \item{grand_mean}{The general mean arithmetic value of the evaluated trait.}
#'
#' @author Faheem Khan (\email{2022ag94@uaf.edu.pk})
#'
#' @seealso \code{\link{anova_rcbd}}, \code{\link{detect_experimental_design}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Execute complete CRD partition on your actual target trait (GYPM)
#'   crd_results <- anova_crd(data = gv_data, trait = "GYPM")
#' }
anova_crd <- function(data, trait, reporting_level = 2) {
  
  # =========================================================================
  # BLOCK 1: ENVIRONMENTAL REGISTRATION AND DIAGNOSTIC TRACE SETUP
  # =========================================================================
  timestamp_start <- Sys.time()
  
  if (reporting_level >= 1) {
    cat(rep("=", 85), "\n", sep = "")
    cat("AGRIDATATOOLS PACKAGED ENGINE v0.1.0 - COMPLETELY RANDOMIZED DESIGN (CRD) ANOVA\n")
    cat("Analysis Inception: ", as.character(timestamp_start), "\n")
    cat(rep("-", 85), "\n", sep = "")
  }
  
  # =========================================================================
  # BLOCK 2: SYSTEM DEFENSE AND STRUCTURAL GATEKEEPER
  # =========================================================================
  if (missing(data) || missing(trait)) {
    stop("CRITICAL EXECUTION FAULT: Both 'data' and 'trait' arguments must be supplied.", call. = FALSE)
  }
  
  if (!trait %in% colnames(data)) {
    stop(paste0("TRAIT REGISTRATION FAULT: Target column '", trait, "' was not discovered."), call. = FALSE)
  }
  
  data$Genotype   <- as.factor(data$Genotype)
  response_vector <- as.numeric(data[[trait]])
  
  if (any(is.na(response_vector))) {
    stop("DATA TRUNCATION FAULT: Missing values (NA) detected. CRD pipeline aborted.", call. = FALSE)
  }
  
  # =========================================================================
  # BLOCK 3: LINEAR MODEL MATRIX ALGEBRA
  # =========================================================================
  if (reporting_level >= 2) {
    cat("[DIAGNOSTIC - MODEL]: Fitting one-way orthogonal linear model matrix formulas...\n")
  }
  
  formula_string <- paste0("`", trait, "` ~ Genotype")
  fitted_model   <- lm(as.formula(formula_string), data = data)
  raw_anova      <- anova(fitted_model)
  
  df_gen <- raw_anova["Genotype", "Df"]
  df_err <- raw_anova["Residuals", "Df"]
  df_tot <- df_gen + df_err
  ss_gen <- raw_anova["Genotype", "Sum Sq"]
  ss_err <- raw_anova["Residuals", "Sum Sq"]
  ss_tot <- ss_gen + ss_err
  ms_gen <- raw_anova["Genotype", "Mean Sq"]
  ms_err <- raw_anova["Residuals", "Mean Sq"]
  f_gen  <- raw_anova["Genotype", "F value"]
  p_gen  <- raw_anova["Genotype", "Pr(>F)"]
  
  grand_mean_val <- mean(response_vector)
  cv_val         <- (sqrt(ms_err) / grand_mean_val) * 100
  
  compiled_anova <- data.frame(
    "Source"  = c("Genotypes (Treatments)", "Error (Residual)", "Total"),
    "Df"      = c(df_gen, df_err, df_tot),
    "SS"      = c(ss_gen, ss_err, ss_tot),
    "MS"      = c(ms_gen, ms_err, NA),
    "F_value" = c(f_gen, NA, NA),
    "p_value" = c(p_gen, NA, NA),
    stringsAsFactors = FALSE
  )
  
  # =========================================================================
  # BLOCK 4: CONSOLE PRESENTATION PIPELINE
  # =========================================================================
  if (reporting_level >= 1) {
    cat("\n", rep("-", 75), "\n", sep = "")
    cat(" ANALYSIS OF VARIANCE (ANOVA) FOR CRD - TRAIT: ", trait, "\n")
    cat(rep("-", 75), "\n", sep = "")
    
    print_table <- compiled_anova
    print_table$SS <- round(print_table$SS, 4)
    print_table$MS <- round(print_table$MS, 4)
    print_table$F_value <- round(print_table$F_value, 3)
    print_table$p_value <- format.pval(print_table$p_value, digits = 4, eps = 0.001)
    
    print(print_table, row.names = FALSE)
    
    cat(rep("-", 75), "\n", sep = "")
    cat(" Trial Grand Mean                 : ", round(grand_mean_val, 4), "\n")
    cat(" Coefficient of Variation (CV%)   : ", round(cv_val, 2), "%\n")
    cat(rep("=", 85), "\n\n", sep = "")
  }
  
  # =========================================================================
  # BLOCK 5: METADATA EMBEDDING AND EXIT PAYLOAD
  # =========================================================================
  if (reporting_level >= 2) {
    processing_seconds <- as.numeric(difftime(Sys.time(), timestamp_start, units = "secs"))
    cat("[LOG - FINALIZE]: CRD matrix compilation finished in ", round(processing_seconds, 5), " seconds.\n")
  }
  
  return(invisible(list(
    anova_table = compiled_anova,
    cv_percentage = cv_val,
    mean_square_error = ms_err,
    grand_mean = grand_mean_val
  )))
}