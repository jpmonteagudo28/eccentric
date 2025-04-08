#' Simulate Sampling Distributions for the Central Limit Theorem
#'
#' This function simulates sampling distributions of standardized sample means
#' using various inverse distribution functions to illustrate the Central Limit Theorem.
#'
#' @param n A numeric vector of sample sizes. Each value in the vector represents a separate simulation size.
#' @param inverse A character string naming a built-in R distribution function that begins with "r" (e.g., "rnorm", "rbeta").
#' Must be one of a predefined set of supported inverse functions.
#' @param samples An integer indicating the number of samples to draw per sample size. Defaults to 249.
#' @param ... Additional parameters passed to the specified inverse distribution function (e.g., `shape1`, `shape2` for `rbeta`).
#'
#' @return A data frame with `samples` rows and `length(n)` columns. Each column corresponds to a sample size in `n`, and each row
#' represents a standardized sample drawn from the specified distribution and resampled with replacement.
#'
#' @details
#' The function first generates random data from the specified inverse function, optionally using extra parameters.
#' It then standardizes the values (z-scores), removes rows with `NA`, and performs resampling with replacement from each
#' column of standardized values. The result is a data frame of simulated CLT sampling distributions.
#'
#' @examples
#' # Simulate CLT sampling distributions from a Cauchy distribution
#' simulate_clt(n = c(10, 50, 100), inverse = "rcauchy")
#'
#' # Simulate with extra distribution parameters
#' simulate_clt(n = c(30, 60), inverse = "rbeta", shape1 = 2, shape2 = 5)
#'
#' @export
simulate_clt <- function(n = NULL,
                         inverse = NULL,
                         samples = 249,
                         ...){

  stopifnot(is.numeric(n),
            is.null(inverse) || is.character(inverse),
            is.null(samples) || is.numeric(samples)
           )

  if(min(n) <= 1 || !is.numeric(n)){
    stop("n must be an atomic vector of integers greater than 1")
  }

  # Capture extra arguments passed to inverse
  dots <- list(...)

  # Check if dots are empty or of an invalid class
  if(is_empty(dots) && !is.null(dots)){
    message("Default location and scale parameters will be used unless specified
    as extra arguments in the function")
  } else if(any(sapply(dots, function(x) is.character(x) || is.logical(x)))) {
    stop("Optional arguments must be numeric")
  } else {
    message("Extra arguments will be passed to inverse function")
  }

  # What values can the inverse arg take?
  inverse <- match.arg(
                       inverse,
                       choices = c(
                         'rbeta','rbinom','rcauchy'
                         ,'rchisq','rexp','rf'
                         ,'rgamma','rgeom','rhyper'
                         ,'rlnorm','rmultinorm','rnbinom'
                         ,'rnorm','rpois','rt'
                         ,'runif','rweibull','rsignrank'
                         ,'rwilcox','rlogis'
                                  ),
                       several.ok = FALSE
                       )

  # Set up vector of values for inverse function
  randomly_gen_data <- if (!is_empty(dots)) {
    lapply(n, function(size) {
      do.call(get(inverse), c(list(n = size), dots))
    })
  } else {
    lapply(n, function(size) {
      get(inverse)(n = size)
    })
  }

  randomly_gen_data <- Map(apply_na_padding, randomly_gen_data, n)

  # Combine padded data into a matrix
  randomly_gen_data_matrix <- suppressWarnings(
                                              {do.call(cbind, randomly_gen_data)}
                                              )
  randomly_gen_data_matrix <- randomly_gen_data_matrix[complete.cases(randomly_gen_data_matrix), ]

  # Standardize vector of values for inverse function
  centered_random_data <- scale(randomly_gen_data_matrix)

  # Resample with replacement from vector of values for inverse
  resampled_data <- lapply(1:length(n),
                              function(i) {
                                            sample(
                                              na.omit(
                                                     centered_random_data[, i]
                                                     ),
                                             size = samples,
                                            replace = TRUE
                                                  )
                                          }
                          )

  resampled_data_df <- data.frame(resampled_data)

  colnames(resampled_data_df) <- as.character(n)

  return(resampled_data_df)
}
