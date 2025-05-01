#' create cruve with less than P(<=a)
#' @param  mu is mean
#' @param  sigma sd
#' @param a user input
#' @name myncurve
#' @return graph and area
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics curve polygon barplot
#' @importFrom stats dnorm pnorm
#' @importFrom utils globalVariables
#' @export
globalVariables(c("x"))
myncurve = function(mu, sigma,a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma)
  , lwd=2, ylab="Normal density", xlab = "Y")
  # x values corresponding to the x - cords of points on the curve
  xcurve = seq(0,a,length=1000)
  # Y values corresponding t0 the x values
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  # Put in the text with the appropriate area
  # Area
  polygon(c(0,xcurve,a),c(0,ycurve,0),col="Red")
  prob= pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  list(mu = mu, sigma = sigma,area=a)
  return(prob)
}
