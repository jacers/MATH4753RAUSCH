#' mysample function
#'
#' @description This function generates random samples, converts them to factors,
#' and creates barplots visualising multiple iterations.
#' @param n Size of the sample to draw in each iteration.
#' @param iter Number of iterations to perform.
#' @param time Pause duration (in seconds) between iterations.
#'
#' @return None.
#' @export
#'
#' @examples
#' mysample(n = 5)
#' mysample(n = 7, iter = 15, 0.5)
#'
mysample = function(n, iter = 10, time = 0.5) {
  for(i in 1:iter) {
    # Make a sample
    s=sample(1:10,n,replace=TRUE)

    # Turn the sample into a factor
    sf=factor(s,levels=1:10)

    # Make a barplot
    barplot(table(sf)/n, beside = TRUE, col = rainbow(10),
            main = paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim = c(0,0.2)
    )

    # Release the table
    # sys.sleep(time)
  }
}
