#' Evaluate Proportion of Z-Scores Beyond a Critical Value
#'
#' Calculates the proportion of values in a numeric vector or each column of a data frame
#' whose absolute z-scores fall outside the specified critical value.
#'
#' This is useful for assessing the proportion of data expected to be beyond a confidence interval threshold,
#' assuming data are standardized z-scores or roughly standard normal.
#'
#' @param data A numeric vector or a data frame containing numeric columns.
#' Each value is assumed to be a z-score or comparable standardized metric.
#' @param ci A single numeric value representing the critical value (e.g., 1.96 for 95% CI).
#'
#' @return
#' If `data` is a vector, a single numeric value is returned representing the proportion of
#' absolute values exceeding the critical value.
#' If `data` is a data frame, a one-row data frame is returned where each column contains
#' the proportion of absolute values in that column exceeding the critical value.
#'
#' @details
#' The function prints a title indicating the critical value used, then returns the computed
#' proportions. Missing values are ignored in the computation.
#'
#' @examples
#' # For a numeric vector
#' z_scores <- rnorm(100)
#' evaluate_data(z_scores, 1.96)
#'
#' # For a data frame with multiple columns
#' df <- data.frame(group1 = rnorm(100), group2 = rnorm(100))
#' evaluate_data(df, 1.96)
#'
#' @export

evaluate_data <- function(data, ci) {

  stopifnot(
            (is.numeric(data) || is.data.frame(data)),
            is.numeric(ci),
            length(ci) == 1
           )

  title <- paste0("Percentage of Z-Scores Outside", ci, " Critical Value for Each Sample Size")

  calculate_proportion <- function(x) {
    mean(abs(x) > ci, na.rm = TRUE)
  }

  if (is.data.frame(data)) {
    proportions <- sapply(data, calculate_proportion)
    result <- as.data.frame(t(proportions))
    colnames(result) <- names(proportions)
  } else {
    result <- calculate_proportion(data)
  }

  cat(title, "\n")
  return(result)
}
