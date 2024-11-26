#' Samples from range 1-n over k iterations, produces a graph for each iteration.
#'
#' @param mu mean of the distribution
#' @param sigma standard deviation
#' @param a right most boundary of which probability is found under curve from -inf to a
#' @examples
#' myncurve(mu=5,sigma=3,a=4)
#'
#' @export
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))
  xvalues <- seq(mu-3*sigma,a,length=1000)
  yvalues <- dnorm(xvalues,mean=mu,sd=sigma)
  polygon(c(-Inf,xvalues,a),c(0,yvalues,0),col="purple",border=NA)

  probability <- pnorm(a,mean=mu,sd=sigma)

  list(mu = mu, sigma = sigma, prob_x_less_a = probability)
}
print(curve1 <- myncurve(5, 3, 4))
