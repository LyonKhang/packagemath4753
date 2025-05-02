#' my Central Limit Function using runif, matrix, apply,creating hist
#' @param  iter looping
#' @param  x is sample size
#' @param fun mode of operation
#' @param alpha forming confidence interval
#' @param cx cex plot
#' @param ... label stuff
#' @name myboot2
#' @concept centrallimit
#' @importFrom graphics segments text
#' @importFrom stats quantile
#' @export

myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  # take input
  n=length(x) # x input use length as sample

  y=sample(x,n*iter,replace=TRUE) # sample by lenght of sample size
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # matrix
  xstat=apply(rs.mat,2,fun) # vector base on mode and  matrix of 2.
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# find conf interval with vector base
  # create histogram with vector base x,
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  # create a matrix that contain the given row of size of x by 1. sort by row
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)


  pte=apply(mat,2,fun) # point estamator with margin of 2 is columns and mean
  abline(v=pte,lwd=3,col="Black")# ver line base on point estimator
  segments(ci[1],0,ci[2],0,lwd=4)   # segments take point which to draw base on conf interval
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot  from point estimate by 2 to density using text draw string given
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))# tem put omit the list, if needed can change to return the list
  # example
  # set.seed(1); test=rnorm(5,mean=4,sd=6)
  # myboot2(iter=100, x=test, fun = "mean",alpha=0.5,col="blue",...)
}
