## Methods
#' @export
eigenvalueTable = function(x) UseMethod("eigenvalueTable",x)

#' @method eigenvalueTable mfa
#' @param  obj An object of class mfa
#' @title Eigenvalue Table
#' @name eigenvalue table
#' @description eigenvalue table from mfa obj
#' @export
eigenvalueTable.mfa = function(obj) {

  singularValues = obj$eigenvalues^(1/2)
  eig = obj$eigenvalues

  cumulative = cumsum(eig)
  percentInertia = eig/cumulative[length(cumulative)] * 100
  cumulativeInertia = cumsum(percentInertia)

  # Create Table
  formatedTable = rbind(singularValues, eig, cumulative, percentInertia, cumulativeInertia)
  return(formatedTable)
}


