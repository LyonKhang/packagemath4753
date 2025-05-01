#' thi  step to create a dotlpot indicating dat distrbitbution with z
#' @param datainput one set of data must be in number to work with
#' @return none because it will create a plot
#'
#' @import lattice
#'
#' @importFrom lattice dotplot
#'
#' @importFrom stats sd
#'
#' @export
dotplotcolor <- function(datainput){
  # z value with current data input
  z =(datainput -mean(datainput))/sd(datainput)

  mycol = ifelse(abs(z)>3, "Red", # this will outlier because z wait to far from org
          ifelse(abs(z)>=2 &abs(z)<=3,"Blue", "Black")) # possible outliers # the ohter point is normal
 dotplot(datainput,col = mycol)
}
