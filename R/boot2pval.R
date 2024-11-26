#' Bootstrap-based Hypothesis Testing Function
#'
#' This function performs a bootstrap-based hypothesis test to compute a p-value for the difference
#' between two sample means. It also provides a histogram of the bootstrap t-statistics, highlighting
#' the critical regions based on the test type.
#'
#' @param x1 A numeric vector representing the first sample.
#' @param x2 A numeric vector representing the second sample.
#' @param conf.level A numeric value representing the confidence level for the test (default is 0.95).
#' @param iter An integer specifying the number of bootstrap iterations (default is 3000).
#' @param mudiff A numeric value representing the hypothesized difference in means under the null hypothesis (default is 0).
#' @param test A character string specifying the type of test: "two" for two-tailed, "upper" for upper-tailed, or "lower" for lower-tailed.
#'
#' @details The function uses resampling techniques to generate bootstrap samples from the input data.
#' It calculates the test statistic for each resample and uses these values to estimate the p-value
#' based on the type of hypothesis test specified. A histogram of the bootstrap t-statistics is plotted,
#' highlighting the rejection regions.
#'
#' @return A list with the following element:
#' \item{pvalue}{The estimated p-value for the hypothesis test.}
#'
#' @examples
#' # Example 1: Two-tailed test
#' set.seed(123)
#' x1 <- rnorm(10, mean = 5, sd = 2)
#' x2 <- rnorm(10, mean = 7, sd = 2)
#' result <- boot2pval(x1, x2, conf.level = 0.95, iter = 1000, mudiff = 0, test = "two")
#' print(result$pvalue)
#'
#' # Example 2: Upper-tailed test
#' result_upper <- boot2pval(x1, x2, conf.level = 0.95, iter = 1000, mudiff = 0, test = "upper")
#' print(result_upper$pvalue)
#'
#' @import graphics
#' @export
boot2pval<-function(x1,x2,conf.level=0.95,iter=3000,mudiff=0, test="two"){
  n1=length(x1)
  n2=length(x2)
  y1=x1-mean(x1)+mean(c(x1,x2))  # transform the data so that it is centered at the NULL
  y2=x2-mean(x2)+mean(c(x1,x2))
  y1rs.mat<-c()    #rs.mat will be come a resample matrix -- now it is an empty vector
  x1rs.mat<-c()
  y2rs.mat<-c()
  x2rs.mat<-c()
  for(i in 1:iter){ # for loop - the loop will go around iter times
    y1rs.mat<-cbind(y1rs.mat,sample(y1,n1,replace=TRUE)) #sampling from y cbind -- column bind -- binds the vectors together by columns
    y2rs.mat<-cbind(y2rs.mat,sample(y2,n2,replace=TRUE))

  }
  x1rs.mat<-y1rs.mat+mean(x1)-mean(c(x1,x2))
  x2rs.mat<-y2rs.mat+mean(x2)-mean(c(x1,x2))

  xbar1=mean(x1)
  xbar2=mean(x2)
  sx1sq=var(x1)
  sx2sq=var(x2)

  tcalc=(xbar1-xbar2-mudiff)/sqrt(sx1sq/n1+sx2sq/n2)

  sy1sq=apply(y1rs.mat,2,var)
  sy2sq=apply(y2rs.mat,2,var)
  y1bar=apply(y1rs.mat,2,mean)
  y2bar=apply(y2rs.mat,2,mean)

  tstat=(y1bar-y2bar-mudiff)/sqrt(sy1sq/n1+sy2sq/n2)


  alpha=1-conf.level # calculating alpha
  #ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  pvalue=ifelse(test=="two",length(tstat[tstat>abs(tcalc) | tstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(tstat[tstat>tcalc])/iter,
                       length(ytstat[tstat<tcalc])/iter))

  h=hist(tstat,plot=FALSE)
  mid=h$mid
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }
  hist(tstat,col=col,freq=FALSE)
  #segments(ci[1],0,ci[2],0,lwd=2)

  return(list(pvalue=pvalue))
  #return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}
