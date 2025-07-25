#' Create Item Analysis Scatter Plot
#'
#' This function creates a scatter plot showing the relationship between
#' item discriminability (point-biserial correlation) and item difficulty
#' (percent passing) for binary item response data.
#'
#' @param data A data frame or matrix where rows are students and columns are items.
#'             Each cell should contain 0 (incorrect) or 1 (correct).
#' @param difficulty_range A numeric vector of length 2 specifying the acceptable
#'                        difficulty range. Default is c(30, 70).
#' @param min_discrimination A numeric value specifying the minimum acceptable
#'                          discriminability. Default is 0.3.
#' @param item_labels A character vector of item labels. If NULL, uses column names
#'                   or numbers 1:ncol(data).
#' @param title A character string for the plot title. Default is "Item Analysis Plot".
#'
#' @return A ggplot object. Also invisibly returns a data frame with item statistics.
#' @examples
#' # Generate sample data
#' set.seed(123)
#' sample_data <- matrix(rbinom(500 * 10, 1, 0.6), nrow = 500, ncol = 10)
#' colnames(sample_data) <- paste0("Item_", 1:10)
#'
#' # Create plot
#' item_analysis_plot(sample_data)
#'
#' @import ggplot2
#' @export
item_analysis_plot <- function(data,
                               difficulty_range = c(30, 70),
                               min_discrimination = 0.3,
                               item_labels = NULL,
                               title = "Item Analysis Plot") {

  # Input validation
  if (!is.matrix(data) && !is.data.frame(data)) {
    stop("Data must be a matrix or data frame")
  }

  if (!all(data %in% c(0, 1, NA))) {
    stop("Data must contain only 0, 1, or NA values")
  }

  if (length(difficulty_range) != 2 || difficulty_range[1] >= difficulty_range[2]) {
    stop("difficulty_range must be a vector of two values where first < second")
  }

  # Convert to matrix if data.frame
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }

  # Calculate total scores for each student (excluding NA)
  total_scores <- rowSums(data, na.rm = TRUE)

  # Set up item labels
  if (is.null(item_labels)) {
    if (!is.null(colnames(data))) {
      item_labels <- colnames(data)
    } else {
      item_labels <- 1:ncol(data)
    }
  } else if (length(item_labels) != ncol(data)) {
    stop("Length of item_labels must equal number of columns in data")
  }

  # Calculate item statistics
  item_stats <- data.frame(
    item = item_labels,
    item_number = 1:ncol(data),
    difficulty = numeric(ncol(data)),
    discriminability = numeric(ncol(data)),
    n_responses = numeric(ncol(data))
  )

  # Calculate difficulty (percent correct) and discriminability for each item
  for(i in 1:ncol(data)) {
    # Remove students with missing responses on this item
    valid_responses <- !is.na(data[, i])
    item_responses <- data[valid_responses, i]
    item_total_scores <- total_scores[valid_responses]

    # Calculate difficulty (percentage correct)
    item_stats$difficulty[i] <- mean(item_responses, na.rm = TRUE) * 100

    # Calculate point-biserial correlation
    if (length(unique(item_responses)) > 1 && sd(item_total_scores) > 0) {
      item_stats$discriminability[i] <- cor(item_responses, item_total_scores, use = "complete.obs")
    } else {
      item_stats$discriminability[i] <- NA
    }

    # Count valid responses
    item_stats$n_responses[i] <- sum(valid_responses)
  }


  p <- ggplot(item_stats, aes(x = item_stats$discriminability, y = item_stats$difficulty)) +
    geom_point(size = 3, alpha = 0.7, color = "steelblue") +
    geom_text(aes(label = item_stats$item), vjust = -0.8, hjust = 0.5, size = 3.5) +

    # Add reference lines
    geom_hline(yintercept = difficulty_range, color = "red",
               linetype = "dashed", alpha = 0.7) +
    geom_vline(xintercept = min_discrimination, color = "red",
               linetype = "dashed", alpha = 0.7) +

    # Add shaded region for "good" items
    annotate("rect",
             xmin = min_discrimination, xmax = 1.0,
             ymin = difficulty_range[1], ymax = difficulty_range[2],
             fill = "orange", alpha = 0.3) +

    # Customize axes
    scale_x_continuous(
      name = "Discriminability (r_pbis with Total Score)",
      limits = c(min(item_stats$discriminability)-0.1, max(item_stats$discriminability)+0.1)
    ) +
    scale_y_continuous(
      name = "Difficulty (Percent Passing)",
      limits = c(min(min(item_stats$difficulty),20), max(max(item_stats$difficulty),80))
    ) +

    ggtitle(title) +

    # Theme
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )

  print(p)
  result <- p

  # Print summary statistics
  cat("\nItem Analysis Summary:\n")
  cat("Items in acceptable range (discrimination >=", min_discrimination,
      "& difficulty", difficulty_range[1], "-", difficulty_range[2], "%):",
      sum(item_stats$discriminability >= min_discrimination &
            item_stats$difficulty >= difficulty_range[1] &
            item_stats$difficulty <= difficulty_range[2], na.rm = TRUE), "\n")

  # Return item statistics invisibly
  invisible(item_stats)
}
