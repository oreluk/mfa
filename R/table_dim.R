#' @export
table_dim = function(x) UseMethod("table_dim")
table_dim.mfa = function(x) {
  #' @title Contribution: table to dimension
  #' @description Calculates the contributions of a table to the dimension
  #' @param x is an mfa object
  #' @export
  #' @return a matrix k-by-l where k is the tables and l is the dimensions
  #'

  # Initialize the matrix to store the table contribution
  ctr_tab = matrix(rep(1, length(x$sets) * length(x$eigenvalues)),
                   nrow = length(x$sets), ncol = length(x$eigenvalues))

  # var_dim returns a n-by-l matrix, where n is the number of variables in 
  #  the dataset and l is the dimensions
  res = var_dim(x)

  # Loop over dimensions taking sums of each table
  for (i in 1:length(x$sets)){
    idx = x$aVector == x$alpha[[i]]
    ctr_tab[i,] = colSums(res[idx,])
  }

  return(ctr_tab)
}
