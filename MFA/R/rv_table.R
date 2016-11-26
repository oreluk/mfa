rv_table <- function(dataset, sets) {
  #' @title Rv Coefficient Table - rv_table
  #' @description Calculates Rv coefficient table for multiple sets of a dataset
  #' @param dataset is the input data in the form of a  data.frame object
  #' @param sets is a list containing indices (or names) of the variables to be parsed for the analysis
  #' @export
  #'
  #' @examples
  #' filename = system.file("extdata", "wines.csv", package = "MFA")
  #' dataset = read.csv(filename, header=TRUE, check.names=FALSE)
  #' rv_table(dataset, sets = list(1:3, 4:5, 6:10))
  #'

  # Initialize rv table
  rvtable = matrix(rep(1,length(sets)^2), nrow = length(sets))

  # To calculate elements for rvtable: diagonal equals to 1, and
  # the matrix is symetric.
  for (i in 1:length(sets)) {
    for (j in 1:length(sets)) {
      if (i == j){
        rvtable[i,j] = 1
      } else if (j < i){
        rvtable[i,j] = rvtable[j,i]
      } else {
        rvtable[i,j] = rv(dataset[,sets[[i]]], dataset[,sets[[j]]])
      }
    }
  }
  return(rvtable)
}

