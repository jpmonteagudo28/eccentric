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
  inverse <- match.arg(inverse,
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
                                            replace = TRUE)
                                          }
                              )

  resampled_data_df <- data.frame(resampled_data)

  colnames(resampled_data_df) <- as.character(n)

  return(resampled_data_df)
}
