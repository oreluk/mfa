#' @name load-wine-data
#' @title Load Wine Data
#' @description loads wine data from csv to data frame
#' @return data frame with wine data
#' @param  checknames FALSE means repeated V1 is kept, TRUE means second is called V1.2
#' @export
loadWineData <- function(checknames=FALSE) {
  ## read csv
  # get the raw data from the package itself:
  filename = system.file("extdata", "wines.csv", package = "MFA")
  d = read.csv(filename, header=TRUE, check.names=checknames)
  d
}

#' @name load-wine-info
#' @title Load Wine Info
#' @description loads wine info (maps of variables V1, etc. to descriptions)
#' @return Map between V1 --> cattiness etc.
#' @export
loadWineInfo <- function() {
  ## var maps
  keys <- list('V1'='cat pee',
          'V2'='passion fruit',
          'V3'= 'green pepper',
          'V4'= 'mineral',
          'V5'= 'smoky',
          'V6'= 'citrus',
          'V7'= 'tropical',
          'V8'= 'leafy',
          'V9'= 'grassy',
          'V10'= 'flinty',
          'V11'= 'vegetal',
          'V12'= 'hay',
          'V13'= 'melon',
          'V14'= 'grass',
          'V15'= 'peach')
  keys
}
