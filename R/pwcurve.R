#' pw.curve function
#'
#' @param x vector representing the independent variable.
#' @param coef vector of length 3 containing the coefficients for the piecewise curve.
#'
#' @return Calculate the piecewise curve
#' @importFrom stats
#' @export
#'
#' @examples
#' pw.curve(10, c(1, 2, 3)) # Example usage
#'
pwcurve = function(x, coef) {
  coef[1] + coef[2] * x + coef[3] * (x - 18) * (x - 18 > 0)
}
