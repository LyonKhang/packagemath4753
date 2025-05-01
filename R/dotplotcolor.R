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
  z =(datainput -mean(datainput))/sd(datainput)

  mycol = ifelse(abs(z)>3, "Red",
          ifelse(abs(z)>=2 &abs(z)<=3,"Blue", "Black"))
 dotplot(datainput,col = mycol)
}
