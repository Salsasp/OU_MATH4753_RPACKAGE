#' Calculate a 95% Confidence Interval for the Mean
#'
#' This function calculates a 95% confidence interval for the population mean (\eqn{\mu})
#' based on a single sample. It assumes a normally distributed population.
#'
#' @param x A numeric vector representing the sample data.
#' @return A numeric vector of length 2, containing the lower and upper bounds of the 95% confidence interval.
#' @examples
#' # Example usage with a sample of data
#' sample_data <- c(21.65, 17.48, 20.1, 21.57, 14.82)
#' myci(sample_data)
#' @export
myci <- function(x) {
  sample_mean <- mean(x)
  sample_sd <- sd(x)
  n <- length(x)
  df <- n - 1

  t_critical <- qt(0.975, df)

  se <- sample_sd / sqrt(n)

  lower_bound <- sample_mean - t_critical * se
  upper_bound <- sample_mean + t_critical * se

  c(lower_bound, upper_bound)
}
