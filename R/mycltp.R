#' mycltp function
#'
#' @description This function demonstrates the Central Limit Theorem (CLT) by simulating sample means
#' from a Poisson distribution. It produces a histogram of sample means with an overlay of
#' a theoretical normal curve, as well as visualizations of the original data distribution.
#'
#' @param n The sample size for each iteration.
#' @param iter Number of iterations (number of samples to draw).
#' @param lambda Rate parameter (\eqn{\lambda}) of the Poisson distribution (default: 10).
#' @param ... Arguments to be passed to the `hist` function for customizing the histogram of sample means.
#'
#' @return None. This function produces multiple plots as side effects:
#' \itemize{
#'   \item A histogram of sample means with a theoretical normal curve overlay.
#'   \item A barplot of relative frequencies of sampled values.
#'   \item A plot of the Poisson probability mass function (PMF) for the given \eqn{\lambda}.
#' }
#' @export
#' @examples
#' mycltp(n = 30, iter = 100, lambda = 10)
#' mycltp(n = 50, iter = 200, lambda = 5, col = "lightblue", border = "blue")
mycltp = function(n, iter, lambda = 10, ...){
  ## r-random sample from the Poisson
  y = rpois(n * iter, lambda = lambda)

  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data = matrix(y, nr = n, nc = iter, byrow = TRUE)

  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w = apply(data, 2, mean)

  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param = hist(w, plot = FALSE)

  ## Since the histogram will be a density plot we will find the max density
  ymax = max(param$density)

  ## To be on the safe side we will add 10% more to this
  ymax = 1.1 * ymax

  ## Make a suitable layout for graphing
  layout(matrix(c(1, 1, 2, 3), nr = 2, nc = 2, byrow = TRUE))

  ## Now we can make the histogram
  hist(w, freq = FALSE,  ylim = c(0, ymax), col = rainbow(max(w)),
       main = paste("Histogram of sample mean", "\n", "sample size= ", n," iter=", iter, " lambda=", lambda, sep = ""),
       xlab = "Sample mean",...)

  ## add a density curve made from the sample distribution
  #lines(density(w),col="Blue",lwd=3) # add a density plot

  ## Add a theoretical normal curve
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda/n)), add = TRUE, col = "Red", lty = 2, lwd = 3) # add a theoretical curve

  # Now make a new plot
  # Since y is discrete we should use a barplot
  barplot(table(y) / (n * iter), col = rainbow(max(y)), main = "Barplot of sampled y", ylab = "Rel. Freq",xlab="y")
  x = 0:max(y)
  plot(x, dpois(x, lambda = lambda), type = "h", lwd = 5, col = rainbow(max(y)),
       main = "Probability function for Poisson", ylab = "Probability", xlab = "y")
}
