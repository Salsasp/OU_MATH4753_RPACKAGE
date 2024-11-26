#' Maximum Likelihood Estimation for Beta Distribution Parameters
#'
#' Computes the maximum likelihood estimates (MLEs) for the parameters of a
#' beta distribution given a sample and grids of possible parameter values
#' for \code{alpha} and \code{beta}. This function uses a grid search over
#' the specified \code{alpha} and \code{beta} values and returns the MLEs
#' along with their likelihood values.
#'
#' @param x A numeric vector representing the sample data, assumed to be drawn from a beta distribution.
#' @param alpha A numeric vector of candidate values for the \code{alpha} parameter of the beta distribution.
#' @param beta A numeric vector of candidate values for the \code{beta} parameter of the beta distribution.
#' @param ... Additional arguments passed to the contour plot.
#'
#' @details This function performs a grid search over specified values of \code{alpha}
#' and \code{beta} to find the maximum likelihood estimates of these parameters.
#' It computes the log-likelihood values for each combination of \code{alpha} and
#' \code{beta}, identifies the values that maximize the likelihood, and returns them
#' along with the maximum likelihood.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{x}}{The original sample data.}
#'   \item{\code{coord}}{The coordinates of the MLEs in the \code{alpha} and \code{beta} grids.}
#'   \item{\code{maxl}}{The maximum likelihood value.}
#'   \item{\code{maxalpha}}{The MLE for the \code{alpha} parameter.}
#'   \item{\code{maxbeta}}{The MLE for the \code{beta} parameter.}
#' }
#'
#' @examples
#' # Sample data generated from a beta distribution
#' set.seed(123)
#' x <- rbeta(30, shape1 = 3, shape2 = 4)
#'
#' # Define grids for alpha and beta
#' alpha <- seq(2.5, 3.5, length.out = 100)
#' beta <- seq(3.5, 4.5, length.out = 100)
#'
#' # Run the mlbeta function
#' result <- mlbeta(x, alpha, beta)
#' print(result)
#'
#' @importFrom stats dbeta
#' @export
mlbeta=function(x,alpha,beta,...){  #x sample vector
  na=length(alpha) # number of values in alpha
  nb=length(beta)
  n=length(x) # sample size
  zz=c()    ## initialize a new vector
  lfun=function(x,a,b) log(dbeta(x,shape1=a,shape2=b))   # log lik for beta
  for(j in 1:nb){
    z=outer(x,alpha,lfun,b=beta[j]) # z a matrix
    # col 1 of z contains lfun evaluated at each x with first value of alpha,
    # col2 each x with 2nd value of a
    # all with b=beta[j]
    y=apply(z,2,sum)
    # y is a vector filled with log lik values,
    # each with a difft alpha and all with the same sig[j]
    zz=cbind(zz,y)
    ## zz is the matrix with each column containing log L values, rows difft alpha, cols difft betas
  }
  maxl=max(exp(zz))    # max lik
  coord=which(exp(zz)==maxl,arr.ind=TRUE)  # find the co-ords of the max
  aest=alpha[coord[1]] # mxlik estimate of alpha
  best=beta[coord[2]]
  contour(alpha,beta,exp(zz),las=3,xlab=expression(alpha),ylab=expression(beta),axes=TRUE,
  main=expression(paste("L(",alpha,",",beta,")",sep="")),...)
  abline(v=aest, h=best)
  points(aest,best,pch=19)
  axis(4,best,round(best,2),col="Red")
  axis(3,aest,round(aest,2),col="Red")
  return(list(x=x,coord=coord,maxl=maxl,maxalpha=aest,maxbeta=best))
}
