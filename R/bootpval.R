#' bootpval function
#'
#' @description This function performs a bootstrap hypothesis test and calculates the p-value for a given dataset.
#' It can perform a two-sided, upper, or lower tail test and generates a histogram of the bootstrap resampled t-statistics.
#'
#' @param x vector of data values for which the p-value is calculated.
#' @param conf.level confidence level for the confidence interval (default is 0.95).
#' @param iter number of bootstrap iterations (default is 3000).
#' @param mu0 null hypothesis value for the mean (default is 0).
#' @param test type of hypothesis test to perform.
#' Options are `"two"` (two-tailed), `"upper"` (upper tail), or `"lower"` (lower tail).
#'
#' @return A list containing the following:
#'   - `pvalue`: The calculated p-value based on the bootstrap resampling.
#'   - `tcalc`: The calculated t-statistic for the original data.
#'   - `n`: The sample size.
#'   - `x`: The original data vector.
#'   - `test`: The type of test performed.
#'   - `ci`: The confidence interval for the mean under the null hypothesis.
#' @export
#'
#' @examples
#' bootpval(x = x1, conf.level = 0.95, iter = 3000, mu0 = 5, test = "two")
bootpval<-function(x=x1,conf.level=0.95,iter=3000,mu0=0, test="two") {
  n=length(x)
  y=x-mean(x)+mu0  # transform the data so that it is centered at the NULL
  rs.mat<-c()    #rs.mat will become a resample matrix -- now it is an empty vector
  xrs.mat<-c()
  for(i in 1:iter){ # for loop - the loop will go around iter times
    rs.mat<-cbind(rs.mat,sample(y,n,replace=TRUE)) #sampling from y cbind -- column bind -- binds the vectors together by columns
    xrs.mat<-cbind(xrs.mat,sample(x,n,replace=TRUE)) #sampling from x cbind -- column bind -- binds the vectors together by columns
  }

  tstat<-function(z){ # The value of t when the NULL is assumed true (xbar-muo)/z/sqrt(n)
    sqrt(n)*(mean(z)-mu0)/sd(z)
  }

  tcalc=tstat(x) # t for the data collected
  ytstat=apply(rs.mat,2,tstat) # tstat of resampled y's, ytstat is a vector and will have iter values in it
  xstat=apply(xrs.mat,2,mean)  # mean of resampled x's
  alpha=1-conf.level # calculating alpha
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  pvalue=ifelse(test=="two",length(ytstat[ytstat>abs(tcalc) | ytstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(ytstat[ytstat>tcalc])/iter,
                       length(ytstat[ytstat<xstat])/iter))

  h=hist(ytstat,plot=FALSE)
  mid=h$mid
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }
  if(test=="upper"){
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Gray",length(mid)-ncolr),rep("Green",ncolr))
  }

  if(test=="lower"){
    ncoll=length(mid[mid<=  -abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll))
  }
  hist(ytstat,col=col,freq=FALSE,las=1,main="",xlab=expression(T[stat]))
  #segments(ci[1],0,ci[2],0,lwd=2)
  pround=round(pvalue,4)
  title(substitute(paste(P[value],"=",pround)))
  return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}
