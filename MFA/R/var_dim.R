var_dim = function(x) UseMethod("var_dim")
var_dim.mfa = function(x) {
  #' @title Contribution: variable to dimension
  #' @description Calculates the constribution of a variable to a dimension.
  #' @param x is an mfa object
  #' @export
  #' @return a matrix n-by-m where n is the variables and m is the dimensions.
  #'

  # Initialize the matrix to store the variable contribution
  ctr_var = x$matrixLoadings

  # Eq.27 (the row of matrixLoadings is dimension?)
  for (j in 1:nrow(ctr_var)){
    ctr_var[j,] = x$alpha[j] * x$matrixLoadings[j,]^2
  }

  return(ctr_var)
}
