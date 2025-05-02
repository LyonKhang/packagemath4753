#' create cruve with less than P(<=a)
#' @param  mu is mean
#' @param  sigma sd
#' @param a user input
#' @name myncurve
#' @concept curve
#' @return graph and area
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics curve polygon barplot
#' @importFrom stats dnorm pnorm
#' @importFrom utils globalVariables
#' @export
globalVariables(c("x")) # global variable can used by everyone in and out of the function
myncurve = function(mu, sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma) # this vary  empirical rule
  , lwd=2, ylab="Normal density", xlab = "Y")
  # x create normal sqeunces based user values a from,to, length
  xcurve = seq(0,a,length=1000) # take user input a and tested it
  ycurve = dnorm(xcurve, mean = mu, sd = sigma) # normal distribution of P(<=a). in this case x vals
  # draw area using polygon at x vectors take xurve to a. y takes y curve to 0.
  polygon(c(0,xcurve,a),c(0,ycurve,0),col="Red")
  prob= pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  list(mu = mu, sigma = sigma,area=a) # list the result
  return(prob)
}
