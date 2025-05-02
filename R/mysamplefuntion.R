#' Below is a code create bar plot of random sampling
#' @param n data
#' @param iter iterations or looping variables
#' @param time delay time between function executions
#' @name myfunction
#' @concept sampling
#' @example
#' @return barplot
#'
#' @export
myf=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    s=sample(1:10,n,replace=TRUE)
    # factor will will random sample and code a vector
    sf=factor(s,levels=1:10)
    #create bar plot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            # graph pasting the label
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            # set y bar
            ylim=c(0,0.2)
    )
    # this wait loop to make it works correctly
    Sys.sleep(time)
  }
  #mf(100,iter=8,time=1.6)
}
