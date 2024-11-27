#' myttest
#'
#' The `myttest` function performs a one-sample t-test on the provided data,
#' compares it against a specified mean (`mu`), and returns key results
#' including the p-value, confidence interval, and a decision on the null hypothesis.
#'
#' @param data A numeric vector of data values. Default is a random sample of 30 values
#'   from a normal distribution with mean 25 and standard deviation 5.
#' @param mu A numeric value specifying the null hypothesis mean. Default is 22.
#'
#' @return A list with the following elements:
#'   \item{pvalue}{The p-value from the t-test.}
#'   \item{ci}{The confidence interval of the mean from the t-test.}
#'   \item{nullhypothesis}{A character string indicating whether the null hypothesis
#'     ("Accept H0" or "Reject H0") is accepted or rejected based on the p-value and confidence interval.}
#'
#' @examples
#' # Perform a t-test with default parameters
#' result <- myttest()
#' print(result)
#'
#' # Perform a t-test with custom data and null hypothesis mean
#' custom_data <- c(24, 25, 26, 23, 24, 25, 26, 27)
#' result <- myttest(data = custom_data, mu = 25)
#' print(result)
#'
#' @export
myttest <- function(data = rnorm(30, mean = 25, sd = 5), mu = 22) {
  # The t.test, which all of our returns will be based off pf
  ttest       <- t.test(data, mu = mu)

  # Variables that will be returned
  pvalue      <- ttest$p.value
  ci          <- ttest$con
  ci_in_range <- mu >= ci[1] && mu <= ci[2]

  # Return all of our return variables and test the null hypothesis
  return(list(pvalue = pvalue, ci = ci, nullhypothesis =
                if(ttest$p.value > 0.05 && ci_in_range) {
                  # H0 has both a pvalue > 0.05 and a legal ci range
                  print("Accept H0")
                } else {
                  # H0 has a pvalue < 0.05 and/or a legal ci range
                  print("Reject H0")
                }))
}
