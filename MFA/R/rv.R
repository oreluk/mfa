rv <- function(table1, table2){
  #' @title Rv Coefficient - rv
  #' @description Calculates Rv coefficient between two tables
  #' @param table1 is a list or matrix of the first input table to be included in the analysis
  #' @param table2 is a list or matrix of the second input table to be included in the analysis
  #' @export
  #'
  #' @examples
  #' filename = system.file("extdata", "wines.csv", package = "MFA")
  #' d = read.csv(filename, header=TRUE, check.names=FALSE)
  #' table1 = d[,seq(2,7)]
  #' table2 = d[,seq(8,13)]
  #' rv(table1,table2)
  #'

  table1 = as.matrix(table1)
  table2 = as.matrix(table2)

  # Equation shown in /tutorial/ slide 23
  # Numerator:
  num = sum(diag(tcrossprod(table1) %*% tcrossprod(table2)))

  # Denominator:
  den_part1 = sum(diag(tcrossprod(table1) %*% tcrossprod(table1)))
  den_part2 = sum(diag(tcrossprod(table2) %*% tcrossprod(table2)))
  den = sqrt(den_part1*den_part2)

  return(num/den)
}
