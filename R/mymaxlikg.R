#' mymaxlikg function
#'
#' @description This function performs a maximum likelihood estimation by evaluating a likelihood
#' across a range of parameter values (`theta`). It visualizes the likelihood function, highlights
#' the maximum likelihood estimate (MLE), and returns the parameter value corresponding to the MLE.
#'
#' @param lfun A likelihood function (default: `"logbin2"`)
#' @param theta Values to evaluate the likelihood function over.
#'
#' @return The parameter (`theta`) that maximizes the likelihood.
#' @export
#'
#' @examples
#' logbin2 = function(theta) {
#' log(dbinom(2, prob = theta, size = 6)) + log(dbinom(4, prob = theta, size = 10))
#' }
#' mymaxlikg(loglik, seq(0.01, 0.99, by = 0.01))
mymaxlikg = function(lfun = "logbin2", theta) { # default log lik is a combination bin
  nth = length(theta) # nu. of valuse used in theta
  thmat = matrix(theta, nr = nth, nc = 1, byrow = TRUE) # Matrix of theta
  z = apply(thmat, 1, lfun) # z holds the log lik values
  zmax = max(which(z == max(z))) # finding the INDEX of the max lik
  plot(theta, exp(z), type = "l") # plot of lik
  abline(v = theta[zmax], col = "Blue") #  verical line through max
  axis(3, theta[zmax], round(theta[zmax], 4)) # one tick on the third axis
  theta[zmax] # theta corresponding to max lik
}
