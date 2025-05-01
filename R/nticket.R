#' this is calculating ntickets
#'
#'
#'
#' @param N number of seat
#' @param gamma probaility a plain will be truly over booked
#' @param p probaility of show
#' @importFrom graphics abline layout points
#' @importFrom stats optimize pbinom qnorm
#'@export

ntickets <- function(N,gamma,p){

  ## finding nd and nc ##

  # standard generic , start from to with desired length
  n <- seq(from = 1, to = N+20,length=N+20)

  ## this will be use for plotting ##

  funtinond <- abs(1-gamma-pbinom(N,n,p))  # magnitude of differ function quantile nd
  #funtinond <- abs(pbinom(1-gamma,n,p))
  # smallest value using nd function
  nd <- which.min(funtinond)

  # using to find a normal approximation  using endpoint correction
  abs_nc <- function(n) {
    return(abs(qnorm(1-gamma, n *p, sqrt (n*p*(1-p))) - N - 0.5)) # a straight line with smallest min
    #return(abs( qbinom(1-gamma,n,p))) # this yield not accurate enough compare to endpoint correction
  }
  opt_nc <- optimize(abs_nc,c(1,N+20))
  nc<- opt_nc$minimum # min value for nc

  ## plotting ##
  # find nd using discrete distribution
  # smallest value using nd
  # use find n continuous equation
  functionnc <- function(n){
    # find different probability of normal distribution
    return(abs(1-gamma-pnorm(N+0.5,n *p,sqrt (n*p*(1-p))))) # 0.5 continuity correction
  }
  # optimize
  opt_functionnc <- optimize(functionnc,c(1,N+20))
  funtionncMIN <- opt_functionnc$minimum

  # layout
  layout(matrix(c(1:2),nrow=2,ncol=2))
  # discrete graph
  plot(funtinond,
       col="red4",
       xlim = c(N,N+20),
       xlab= "n",  ylab="Objective",
       main =  paste("Objective Vs n to find optimal tickets sold","\n","(",nd,")","gamma=",gamma,"N=",N,"discrete"),
       lwd=1, cex=0.45,type ="b")
  # change line to dashes lines
  lines(funtinond, col="olivedrab", type = "c", lwd=0.75)
  # create point
  points(funtinond, col = "royalblue4", pch=20,cex=0.75)
  # finding value nd MIN
  abline(v=nd,h=funtinond[nd],col="red")


  # continuous graph using fun
  curve(functionnc, col = "brown", xlim = c(N,N+20)
        ,xlab= "n",ylab="Objective",
        main =  paste("Objective Vs n to find optimal tickets sold","\n","(",nc,")","gamma=",gamma,"N=",N,"continuous"),
        lwd=1,
        type = "l"
  )
  # finding value nc MIN
  abline(v=funtionncMIN,h= opt_functionnc, col="navyblue")
  # print a named list containing nd,funtionncMIN,Np,gamma

  return(list(nd=nd,nc = nc, N= N, p=p,gamma = gamma ))
}
