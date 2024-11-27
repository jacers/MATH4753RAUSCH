#' myboot2 function
#'
#' @description This function performs a bootstrap resampling procedure to calculate confidence intervals
#' for a specified statistic (e.g., mean, median). It visualizes the distribution of the bootstrap sample
#' statistics with a histogram, including a confidence interval and point estimate annotations.
#'
#' @param iter Number of bootstrap iterations (default: 10000).
#' @param x Sample data.
#' @param fun String or function specifying the statistic to compute (default: `"mean"`).
#' @param alpha Value between 0 and 1 specifying the significance level for the confidence interval (default: 0.05)
#' @param cx Size of text annotations on the plot (default: 1.5).
#' @param ... Additional arguments passed to the `hist` function for customizing the histogram.
#'
#' @return A list (invisible) containing:
#' \itemize{
#'   \item \code{ci}: The bootstrap confidence interval as a numeric vector of length 2.
#'   \item \code{fun}: The statistic function used.
#'   \item \code{x}: The original sample data.
#'   \item \code{xstat}: The bootstrap sample statistics as a numeric vector.
#' }
#' @export
#'
#' @examples
#' myboot2(iter = 5000, x = c(2, 4, 7, 9, 11), fun = "mean", alpha = 0.05)
#' myboot2(iter = 10000, x = c(2, 4, 7, 9, 11), fun = "median", col = "blue")
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...){  #Notice where the ... is repeated in the code
  n = length(x) # sample size

  y = sample(x, n * iter, replace = TRUE) # Line A
  rs.mat = matrix(y, nr = n, nc = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, fun) # xstat is a vector and will have iter values in it
  ci = quantile(xstat, c(alpha / 2, 1 - alpha / 2)) # Line B: Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para = hist(xstat, freq = FALSE, las = 1,
              main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha = ", alpha, " iter = ", iter, sep = ""),
              ...)

  # mat will be a matrix that contains the data, this is done so that I can use apply()
  mat = matrix(x, nr = length(x), nc = 1, byrow = TRUE)

  # pte is the point estimate
  # This uses whatever fun is
  pte = apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black") # Vertical line
  segments(ci[1], 0, ci[2], 0, lwd=4) # Make the segment for the ci
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)

  # plot the point estimate 1/2 way up the density
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x, xstat = xstat)) # Some output to use if necessary
}
