#' Fire damage
#'
#' A data set collected concerning home damage from fire
#'
#' @format A data frame with 15 rows and 2 variables:
#' \describe{
#' \item{DISTANCE}{distance from fire station in miles}
#' \item{DAMAGE}{X 1000 dollars in damage}
#' }
#' @source \url{https://www.crcpress.com/Statistics-for-Engineering-and-the-Sciences/Mendenhall-Sincich/p/book/9781498728850}
#' @importFrom utils read.csv
#' @import usethis
#' @importFrom usethis use_data
#'
"fire"

#fire <- utils::read.csv("data-raw/FIREDAM.csv")
absolute_path <- "~/Desktop/Spring2025/MATH4753/VINH4753SPR25/data-raw/FIREDAM.csv"
print(file.exists(absolute_path))
fire <- utils::read.csv(absolute_path)

usethis::use_data(fire,overwrite = TRUE)


