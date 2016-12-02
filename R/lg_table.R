lg_table <- function(dataset, sets){
  #' @title Lg Coefficient Table - lg_table
  #' @description Calculates Lg coefficient table between multiple sets from a dataset
  #' @param dataset is the input data in the form of a  data.frame object
  #' @param sets is a list containing indices (or names) of the variables to be parsed for the analysis
  #' @export
  #'
  #' @examples
  #' filename = system.file("extdata", "wines.csv", package = "MFA")
  #' dataset = read.csv(filename, header=TRUE, check.names=FALSE)
  #' lg_table(dataset, sets = list(1:3, 4:5, 6:10))
  #'

  # Initialize lg table
  lgtable = matrix(rep(1,length(sets)^2), nrow = length(sets))

  # Loop over initialized table to calculate elements of lgtable. Utilizing the fact the matrix 
  # is symmetric and diag(lgtable) = 1 
  for (i in 1:length(sets)) {
    for (j in 1:length(sets)) {
      if (i == j){
        lgtable[i,j] = 1
      } else if (j < i){
        lgtable[i,j] = lgtable[j,i]
      } else {
        lgtable[i,j] = lg(dataset[,sets[[i]]], dataset[,sets[[j]]])
      }
    }
  }
  return(lgtable)
}
