#' Below is a code create bar plot of randomple sampling
#' @param n data
#' @param iter iterations or looping varaiables
#' @param time delay time between function executions
#' @return barplot
#'
#' @export
myf=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #loonig sample to fully randomize
    s=sample(1:10,n,replace=TRUE)
    sf=factor(s,levels=1:10)
    # barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    # timing
    Sys.sleep(time)
  }
}
