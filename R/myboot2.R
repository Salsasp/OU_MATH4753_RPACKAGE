#' Bootstrap Confidence Interval Estimation
#'
#' Generates a bootstrap confidence interval for a specified statistic (default is the mean)
#' by resampling the input data `x` a specified number of times. A histogram of the bootstrap
#' sample statistics is also generated.
#'
#' @param iter An integer. The number of bootstrap iterations to perform. Default is 10000.
#' @param x A numeric vector. The input data from which to bootstrap.
#' @param fun A character string. The name of the statistic function to be applied (e.g., `"mean"`, `"median"`). Default is `"mean"`.
#' @param alpha A numeric value. The significance level for the confidence interval. Default is 0.05.
#' @param cx A numeric value. Controls the size of the text in the plot. Default is 1.5.
#' @param ... Additional graphical parameters to be passed to the `hist` function.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{ci}{A numeric vector. The lower and upper bounds of the bootstrap confidence interval.}
#'   \item{fun}{A character string. The statistic function used.}
#'   \item{x}{The original data used for bootstrapping.}
#' }
#'
#' @details The function resamples the input data `x` with replacement for a specified number
#' of iterations (`iter`). The specified statistic (`fun`) is computed for each resampled dataset,
#' and a histogram of the bootstrap sample statistics is plotted. A confidence interval is
#' calculated based on the `alpha` level and is displayed on the histogram along with the point estimate.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30)
#' myboot2(iter=1000, x=x, fun="mean", alpha=0.05, cx=1.5)
#'
#' @export

myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
