table_dim = function(x, ctr_var = 1, numvar_table = 1) UseMethod("table_dim")
table_dim.mfa = function(x, ctr_var = 1, numvar_table = 1) {
  #' @title Contribution: table to dimension
  #' @description Calculates the constribution of a table to the dimension
  #' @param x is an mfa object
  #' @param ctr_var is the contribution variable
  #' @param numvar_table is the variable index in the table
  #' @export
  #' @return a matrix n-by-m where n is the tables and m is the dimensions.
  #'

  # Initialize the matrix to store the table contribution
  ctr_var = matrix(rep(1,numvar_table*ncol(ctr_col)),
                 nrow = numvar_table, ncol = ncol(ctr_col))

  # Counter
  j = 1
  # Eq.28
  for (l in ncol(ctr_var)){
    for (k in 1:length(numvar_table)) {
      ctr_table[k,l] = sum( ctr_var[j:(j+numvar_table[k]), l] )
      j = j + numvar_table[k] + 1
    }
  }

  return(ctr_table)
}
