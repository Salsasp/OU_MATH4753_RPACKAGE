#' Samples from range 1-n over k iterations, produces a graph for each iteration.
#'
#' @param n Sample size
#' @param iter Number of iterations
#' @param time Number of seconds between the creation of each plot
#' @examples
#' myf(n=10,iter=20,time=1)
#'
#' @export
myf=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
