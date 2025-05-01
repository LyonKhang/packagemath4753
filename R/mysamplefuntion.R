#' Below is a code create bar plot of randomple sampling
#' @param n data
#' @param iter iterations or looping varaiables
#' @param time delay time between function executions
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
}
