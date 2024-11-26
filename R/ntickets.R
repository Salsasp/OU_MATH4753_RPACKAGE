#' Samples from range 1-n over k iterations, produces a graph for each iteration.
#'
#' @param N Number of seats on the plane
#' @param gamma Maximum desired probability that plane will be overbooked
#' @param p Probability that someone shows up for their flight
#' @examples
#' ntickets(N=200,gamma=0.02,p=0.95)
#'
#' @export
ntickets <- function(N, gamma, p) {
  #------------Find Optimal discrete n Value------------------
  maxVal <- -Inf
  n <- NA
  for(i in 0:N*2){
    val <- 1-pbinom(N,i,p)
    if(val <= gamma && val >= maxVal){
      max <- val
      n <- i
    }
  }

  #--------------Discrete----------------------------

  n_values <- seq(N, N + 20, by = 1)
  objective_discrete <- 1 - pbinom(N, n_values, p) #create objective function

  par(mfrow = c(2, 1)) #for putting 2 plots in one window
  plot(n_values, objective_discrete, type="b", col="blue", pch=19,
       ylim=c(0, 1), main=paste("Objective Vs n to find optimal tickets sold\n(",
                                n, ") gamma=", gamma, " N=", N, " discrete"),
       xlab="n", ylab="Objective")
  abline(h=0, col="red", lwd=2)
  abline(v=n, col="red", lwd=2)


  #--------Normal Approximation------------------
  objective_continuous <- function(n) {
    mu <- n * p                                  #mean
    sigma <- sqrt(n * p * (1 - p))               #sd
    return(1 - pnorm(N, mean = mu, sd = sigma))  #CDF
  }


  initial_objective_values <- sapply(n_values, objective_continuous)
  final_optimal_n <- N + qnorm(1 - gamma) * sqrt(N * p * (1 - p))
  plot(n_values, initial_objective_values, type = "l", col = "black",
       main=paste("Objective Vs n to find optimal tickets sold\n(",
                  final_optimal_n, ") gamma=", gamma, " N=", N, " continous"),
       ylim = c(0, 1),
       xlab = "n", ylab = "Objective")
  abline(h = 0, col = "blue", lwd = 2)
  abline(v = final_optimal_n, col = "blue", lwd = 2)

  #returning list
  return(list(n, final_optimal_n,N,p,gamma))
}
