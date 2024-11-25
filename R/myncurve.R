#' myncurve function
#'
#' @description This function plots the probability density function (PDF) of a normal distribution
#' with specified mean (`mu`) and standard deviation (`sigma`). It also returns a list containing
#' the input parameters for reference.
#'
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#'
#' @return A list containing the parameters:
#' \itemize{
#'   \item \code{mu}: The mean of the normal distribution.
#'   \item \code{sigma}: The standard deviation of the normal distribution.
#' }
#' @export
#'
#' @examples
#' # Plot a normal curve with mean 0 and standard deviation 1
#' myncurve(mu = 0, sigma = 1)
#'
myncurve = function(mu, sigma) {
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma))
  list(mu = mu, sigma = sigma)
}
