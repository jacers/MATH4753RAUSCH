#' myci function
#'
#' @description This function calculates the confidence interval (CI) for either the mean or variance of a dataset `x`.
#' It can return the CI for the mean using the Student's t-distribution or for the variance using the Chi-square distribution.
#' The function also provides an option to conduct a t-test for comparison when calculating the CI for the mean.
#'
#' @param x values to calculate the confidence interval.
#' @param percent confidence level (default is 0.95 for a 95% confidence interval).
#' @param mode the type of confidence interval to compute.
#' `"mean"` (default) for the confidence interval of the mean and `"variance"` for the confidence interval of the variance.
#'
#' @return A list containing the confidence interval (`myci`) and the confidence interval from a t-test (`ttest_ci`) for the mean.
#' For variance, only the CI is returned. If the `mode` is invalid, a message indicating the issue is returned.
#' @export
#'
#' @examples
#' myci(x = 1:100, percent = 0.95, mode = "mean")
myci = function(x, percent = 0.95, mode = "mean") {
  # Variables used across the calculations
  ybar  = mean(x)
  n     = length(x)
  s     = sd(x)
  alpha = 1 - percent

  # Mean Interval
  if(mode == "mean") {
    # Variables specific to the mean calculation
    t_a2 = qt(1 - alpha / 2, (n - 1))

    # Interval calculations
    L     = ybar - (t_a2 * s) / sqrt(n) # Lower
    U     = ybar + (t_a2 * s) / sqrt(n) # Upper
    ttest = t.test(d, conf.level = percent) # T-Test for comparison

    # End of function
    return(list(myci = c(L, U), ttest_ci = ttest$con))
  }

  # Variance Interval
  if(mode == "variance") {
    # Interval calculations
    L = ((n - 1) * s^2) / qchisq(1 - alpha / 2, df = n - 1) # Lower
    U = ((n - 1) * s^2) / qchisq(    alpha / 2, df = n - 1) # Upper

    # End of function
    return(list(myci = c(L, U)))
  }

  # Invalid input for mode
  return("Mode must be set to either \"mean\" or\"variance\".")
}
