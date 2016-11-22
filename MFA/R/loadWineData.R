#' @name load-wine-data
#' @title Load Wine Data
#' @description loads wine data from csv to data frame
#' @return data frame with wine data
#' @export
loadWineData <- function() {
  ## read csv
  # get the raw data from the package itself:
  filename = system.file("extdata", "wines.csv", package = "MFA")
  d = read.csv(filename, header=TRUE, check.names=TRUE)
  d
}
