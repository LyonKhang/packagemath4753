#' my Central Limit Function using runif, matrix, apply,creatin hist
#' @param  iter looping
#' @param  x is sample size
#' @param fun mode of operation
#' @param alpha forming confidence interval
#' @param cx cex plot
#' @param ... labaled stuff
#' @importFrom graphics segments text
#' @importFrom stats quantile
#' @export

myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  # take input
  n=length(x) # x input use length as sample

  y=sample(x,n*iter,replace=TRUE) # sample by lenght of sample size
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # matrix
  xstat=apply(rs.mat,2,fun) # vector base on mode and  matrix of 2.
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# find conf intervel with vector base
  # create histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

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
