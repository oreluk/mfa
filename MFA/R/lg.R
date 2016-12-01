lg <- function(table1, table2){
  #' @title Lg Coefficient - lg
  #' @description Calculates Lg coefficient between two tables
  #' @param table1 is a list or matrix of the  first input table to be included in the analysis
  #' @param table2 is a list or matrix of the second input table to be included in the analysis
  #' @export
  #'
  #' @examples
  #' filename = system.file("extdata", "wines.csv", package = "MFA")
  #' d = read.csv(filename, header=TRUE, check.names=FALSE)
  #' table1 = d[,seq(2,7)]
  #' table2 = d[,seq(8,13)]
  #' lg(table1,table2)
  #'

  table1 = as.matrix(table1)
  table2 = as.matrix(table2)


  # Equation shown in /tutorial/ slide 24
  # Numerator:
  num = sum(diag(tcrossprod(table1) %*% tcrossprod(table2)))

  # Denominator:
  decomp_t1 = svd(table1)
  decomp_t2 = svd(table2)
  gamma_t1 = decomp_t1$d[1]
  gamma_t2 = decomp_t2$d[1]

  den = gamma_t1^2 * gamma_t2^2

  return(num/den)
}
