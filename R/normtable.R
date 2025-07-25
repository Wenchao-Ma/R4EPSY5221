#' Create Norm Tables for Test Scores
#'
#' This function creates a comprehensive norm table that converts raw scores
#' to various standardized score types including percentile ranks, z-scores,
#' T-scores, normalized z-scores, stanines, and Normal Curve Equivalents (NCE).
#'
#' @param score A numeric vector of raw scores from which to derive norms.
#'              Missing values (NA) are automatically removed.
#' @param min_value A numeric value specifying the minimum raw score to include
#'                  in the norm table. If NULL (default), uses the minimum
#'                  observed score in the data.
#' @param max_value A numeric value specifying the maximum raw score to include
#'                  in the norm table. If NULL (default), uses the maximum
#'                  observed score in the data.
#' @param digits An integer specifying the number of decimal places for rounding
#'               most scores. Default is 2. PR is rounded to 1 decimal place,
#'               NCE and stanines are integers.
#' @param include_frequencies Logical. If TRUE, includes frequency counts and
#'                           cumulative frequencies in the output. Default is FALSE.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{rawscore}{Raw scores from max_value to min_value}
#'   \item{frequency}{Count of students at each score (if include_frequencies = TRUE)}
#'   \item{cumulative_freq}{Cumulative frequency up to each score (if include_frequencies = TRUE)}
#'   \item{PR}{Percentile rank (0-100)}
#'   \item{zscore}{Linear z-score (mean = 0, SD = 1)}
#'   \item{Tscore}{T-score (mean = 50, SD = 10)}
#'   \item{normalized_z}{Area-transformation normalized z-score}
#'   \item{stanines}{Stanine scores (1-9, mean = 5, SD = 2)}
#'   \item{NCE}{Normal Curve Equivalent (1-99, mean = 50, SD = 21.06)}
#' }
#'
#' @details
#' The function calculates several types of derived scores:
#'
#' \strong{Percentile Rank (PR):} The percentage of scores below a given raw score.
#' Formula: PR = (Number of scores below X / Total N) × 100
#'
#' \strong{Linear z-score:} Standard score with mean = 0, SD = 1.
#' Formula: z = (X - Mean) / SD
#'
#' \strong{T-score:} Linear transformation of z-score with mean = 50, SD = 10.
#' Formula: T = z × 10 + 50
#'
#' \strong{Normalized z-score:} Area-transformation that forces a normal distribution.
#' Based on the percentile rank and inverse normal transformation.
#'
#' \strong{Stanines:} 9-point scale with mean = 5, SD = 2.
#' Formula: Stanine = 5 + (normalized z × 2), bounded between 1 and 9.
#'
#' \strong{NCE (Normal Curve Equivalent):} Similar to percentiles but with equal
#' intervals. Scale of 1-99 with mean = 50, SD = 21.06.
#' Formula: NCE = (normalized z × 21.06) + 50, bounded between 1 and 99.
#' \strong{Deviation IQ:} IQ scores with mean = 100 and SD = 15.
#'
#' \strong{SAT_score:} Simulated SAT-like scores (mean = 500, SD = 100).
#'
#' \strong{Scaled_score:} Standard scale scores (mean = 10, SD = 3).
#'
#' \strong{Sten:} Standard ten scores from 1 to 10 (mean = 5.5, SD = 2).
#'
#' \strong{Quartile/Decile:} Rank-based grouping into 4 or 10 intervals based on PR.
#' @examples
#' # Generate sample test scores
#' set.seed(123)
#' test_scores <- round(rnorm(1000, mean = 75, sd = 12))
#'
#' # Create basic norm table
#' norms <- create_norm_table(test_scores)
#' head(norms)
#'
#' # Create norm table with specific score range
#' norms_range <- create_norm_table(test_scores, min_value = 50, max_value = 100)
#'
#' # Include frequency information
#' norms_freq <- create_norm_table(test_scores, include_frequencies = TRUE)
#'
#' # Custom precision
#' norms_precise <- create_norm_table(test_scores, digits = 3)
#'
#' @note
#' - The function assumes that higher raw scores represent better performance.
#' - Percentile ranks are calculated using the "below" method (strict inequality).
#' - NCE scores of 1, 50, and 99 correspond to percentiles 1, 50, and 99 respectively.
#' - Stanines divide the distribution into 9 parts with specified percentages.
#'
#'
#' @export
#' @importFrom stats cor sd qnorm
#' @rdname create_norm_table
create_norm_table <- function(score,
                              min_value = NULL,
                              max_value = NULL,
                              digits = 2,
                              include_frequencies = FALSE) {

  # Input validation
  if (!is.numeric(score)) {
    stop("'score' must be a numeric vector")
  }

  if (length(score) == 0) {
    stop("'score' cannot be empty")
  }

  # Remove missing values and warn if any found
  original_length <- length(score)
  score <- score[!is.na(score)]

  if (length(score) < original_length) {
    warning(paste("Removed", original_length - length(score), "missing values from score vector"))
  }

  if (length(score) == 0) {
    stop("No valid (non-missing) scores found")
  }

  if (!is.null(digits) && (!is.numeric(digits) || digits < 0 || digits != round(digits))) {
    stop("'digits' must be a non-negative integer")
  }

  # Set min and max values
  if (is.null(min_value)) {
    min_value <- min(score)
  }
  if (is.null(max_value)) {
    max_value <- max(score)
  }

  # Validate min/max values
  if (min_value > max_value) {
    stop("min_value cannot be greater than max_value")
  }

  # Create sequence of raw scores (from max to min for traditional norm table format)
  rawscore <- seq(from = max_value, to = min_value, by = -1)

  # Initialize norm table
  if (include_frequencies) {
    norm_table <- data.frame(
      rawscore = rawscore,
      frequency = NA_integer_,
      cumulative_freq = NA_integer_,
      PR = NA_real_,
      zscore = NA_real_,
      Tscore = NA_real_,
      normalized_z = NA_real_,
      stanines = NA_integer_,
      NCE = NA_integer_,
      deviation_IQ = NA_real_,
      SAT_score = NA_real_,
      scaled_score = NA_real_,
      sten = NA_integer_,
      quartile = NA_integer_,
      decile = NA_integer_
    )
  } else {
    norm_table <- data.frame(
      rawscore = rawscore,
      PR = NA_real_,
      zscore = NA_real_,
      Tscore = NA_real_,
      normalized_z = NA_real_,
      stanines = NA_integer_,
      NCE = NA_integer_,
      deviation_IQ = NA_real_,
      SAT_score = NA_real_,
      scaled_score = NA_real_,
      sten = NA_integer_,
      quartile = NA_integer_,
      decile = NA_integer_
    )
  }

  # Calculate frequency information if requested
  if (include_frequencies) {
    for (i in 1:length(rawscore)) {
      norm_table$frequency[i] <- sum(score == rawscore[i])
      norm_table$cumulative_freq[i] <- sum(score >= rawscore[i])
    }
  }

  # Calculate percentile ranks
  N_total <- length(score)
  for (i in 1:length(rawscore)) {
    N_lower <- sum(score < rawscore[i])
    norm_table$PR[i] <- (N_lower / N_total) * 100
  }

  # Calculate descriptive statistics
  score_mean <- mean(score)
  score_sd <- sd(score)

  if (score_sd == 0) {
    warning("Standard deviation is 0. All z-scores, T-scores, and normalized scores will be identical.")
  }

  # Calculate linear z-scores and T-scores
  norm_table$zscore <- (norm_table$rawscore - score_mean) / score_sd
  norm_table$Tscore <- norm_table$zscore * 10 + 50

  # Calculate normalized z-scores
  # Handle edge cases for percentile ranks of 0 and 100
  pr_for_norm <- norm_table$PR / 100
  pr_for_norm[pr_for_norm <= 0] <- 0.001  # Avoid -Inf
  pr_for_norm[pr_for_norm >= 1] <- 0.999  # Avoid +Inf

  norm_table$normalized_z <- qnorm(pr_for_norm)

  # Calculate stanines (1-9 scale)
  norm_table$stanines <- round(5 + norm_table$normalized_z * 2)
  norm_table$stanines <- pmax(1, pmin(9, norm_table$stanines))  # Clamp to 1-9 range

  # Calculate NCE (Normal Curve Equivalent, 1-99 scale)
  norm_table$NCE <- round(norm_table$normalized_z * 21.06 + 50, 0)
  norm_table$NCE <- pmax(1, pmin(99, norm_table$NCE))  # Clamp to 1-99 range

  # Calculate additional standard scores
  norm_table$deviation_IQ <- norm_table$zscore * 15 + 100  # Mean=100, SD=15
  norm_table$SAT_score <- norm_table$zscore * 100 + 500    # Mean=500, SD=100
  norm_table$scaled_score <- norm_table$zscore * 3 + 10    # Mean=10, SD=3

  # Calculate Sten scores (10-point scale, Mean=5.5, SD=2)
  norm_table$sten <- round(norm_table$normalized_z * 2 + 5.5)
  norm_table$sten <- pmax(1, pmin(10, norm_table$sten))    # Clamp to 1-10 range

  # Calculate quartiles and deciles
  norm_table$quartile <- ceiling(norm_table$PR / 25)
  norm_table$quartile[norm_table$PR == 0] <- 1  # Handle 0th percentile
  norm_table$quartile <- pmax(1, pmin(4, norm_table$quartile))

  norm_table$decile <- ceiling(norm_table$PR / 10)
  norm_table$decile[norm_table$PR == 0] <- 1  # Handle 0th percentile
  norm_table$decile <- pmax(1, pmin(10, norm_table$decile))

  # Round values according to digits parameter
  if (!is.null(digits)) {
    norm_table$PR <- round(norm_table$PR, 1)  # PR always to 1 decimal
    norm_table$zscore <- round(norm_table$zscore, digits)
    norm_table$Tscore <- round(norm_table$Tscore, digits)
    norm_table$normalized_z <- round(norm_table$normalized_z, digits)
    norm_table$deviation_IQ <- round(norm_table$deviation_IQ, digits)
    norm_table$SAT_score <- round(norm_table$SAT_score, 0)  # SAT scores are integers
    norm_table$scaled_score <- round(norm_table$scaled_score, digits)
  }

  # Add attributes for reference
  attr(norm_table, "sample_size") <- N_total
  attr(norm_table, "mean") <- score_mean
  attr(norm_table, "sd") <- score_sd
  attr(norm_table, "score_range") <- c(min(score), max(score))

  return(norm_table)
}

# Print method for norm tables
#' @export
print.norm_table <- function(x, ...) {
  cat("Norm Table\n")
  cat("Sample size:", attr(x, "sample_size"), "\n")
  cat("Mean:", round(attr(x, "mean"), 2), "\n")
  cat("SD:", round(attr(x, "sd"), 2), "\n")
  cat("Score range:", paste(attr(x, "score_range"), collapse = " - "), "\n\n")

  print(as.data.frame(x), ...)
}
