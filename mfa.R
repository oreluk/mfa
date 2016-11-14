#'
#' @title Multiple Factor Analysis (MFA) 
#' @author Yulin Chen, Stephanie Wuerth, Eren Bilir, Jim Oreluk
#' 

check_inputs = function(data, sets, ncomps, center, scale) {
  #'
  #' @title check_inputs
  #' @description Used to verify valid inputs to the class constructor
  #'  
  # data
  if (!is.matrix(data) & 
      !is.data.frame(data) ) {
    stop("'data' must be a matrix or data.frame containing the data set")
  }
  
  # sets
  if (!is.list(sets)) {
    stop("'sets' must be a list containing vectors indicating the sets of variables")
  }
  
  # ncomps
  if (!is.integer(ncomps) & !is.null(ncomps) ) {
    stop("'ncomps' must be a integer indicating the number of components")
  }
  
  # Center
  if (!is.logical(center)) {
    if (!is.vector(center) &
        (is.vector(center) & length(center) != ncomps) ) {
          stop("'center' must be a logical or numeric 
             vector equal to the number of active variables")
    }
  }
  
  # Scale
  if (!is.logical(scale)) { 
    if (!is.vector(scale) & 
      (is.vector(scale) & length(scale) != ncomps )) {
        stop("'scale' must be a logical or numeric 
             vector equal to the number of active variables")
    }
  }
}



# MFA Constructor
mfa = function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE) {
  # Checks validity of inputs
  check_inputs(data, sets, ncomps, center, scale)
  
  # Attributes
  attr(data, "data") = data
  attr(data, "sets") = sets
  attr(data, "ncomps") = ncomps
  attr(data, "center") = center
  attr(data, "scale") = scale
  
  
  # Pre-process the data  -> center and scale
  # create tables from data, for each of the "sets"
  
  # 1) Calculate svd:  for each table (k tables)
  # X_k = U_k \Gamma_k V'_k
      # obtain factor scores   G_k = U_k %*% \Gamma_k
      # \alpha  = weights [ 1/ \Gamma_k[1]^2 ]   ??? a = [\alpha] # stack them all? what are the weights for the j-th table?
  
  # 2) Normalize each table by their first singular value
  
  # 3) Concatenate the K normalized tables
  # 4)  Do svd again
    # X = P \Delta Q
    # F = P \Delta   (factor scores)
  
  
  ev = eigenvalues(data)  # Returns Table
  fs = factorScores()
  pfs = pFactorScores()
  ml = matrixLoadings()
  
  # Derived Attributes
  attr(data, "EigenvalueTable") <- ev
  attr(data, "FactorScore") <- fs
  attr(data, "PartialFactorScore") <- pfs
  attr(data, "Loading") <- ml
  class(data) <- "mfa"
  data
}


print.mfa = function(x, ...) {
  #'
  #' @title print.mfa
  #' @description Overloading print method for mfa, returns basic info 
  #' 
  cat('object "mfa"\n')
  cat('Maximum Eigenvalue: ')
  print(max(x$EigenvalueTable[2,]))
  cat('Scores: ')
  print(x$FactorScore)
  cat('Partial Scores:  ')
  print(x$PartialFactorScore)
  cat('Loadings:  ')
  print(x$Loading)
}


# Eigenvalues 
eigenvalues.mfa = function(x) {
  #'
  #' @title Eigenvalue - eigenvalues.mfa
  #' @description Takes mfa object, returns a table including: 
  #' singular values, eigenvalues, cumulative, percentage of inertia,
  #' cumulative percentage of inertia, for all the extracted components.
  #' 
  val = svd(x)
  singularValues = val$d
  eig = singularValues^2  # eigenvalues of X'X  
  
  cumulative = cumsum(eig)
  pInertia = cumulative/cumulative[length(cumulative)]
  cumulativeInertia = cumsum(pInertia)
  
  # Create Table
  formatedTable = rbind(singularValues, eig, cumulative, pInertia, cumulativeInertia )
  return(formatedTable)
}


factorScores.mfa = function(x) {
  #'
  #' @title Factor Scores - factorScores.mfa
  #' @description Calculates factor scores
  #' 
  
  
  # Use  Eq 18, 20, 64
  
  # F = P \Delta = XAQ
  #     each row of F is a observation. each column is a component
  # P is the 
    return
}


pFactorScores.mfa = function(x) {
  #'
  #' @title Partial Factor Scores - pFactorScores.mfa
  #' @description Calculates partial factor scores
  #' 
  return
}


matrixLoadings.mfa = function(x) {
  #'
  #' @title matrixLoadings - matrixLoadings.mfa
  #' @description Calculates loading matrices
  #' 
  return
}




## Contributions

obs_dim.mfa = function(x) {
  #'
  #' @title Contribution of observation to dimension - obs_dim.mfa
  #' @description Calculates the contribution of a observation to a dimension
  #' 
}

var_dim.mfa = function(x) {
  #'
  #' @title Contribution of variable to dimension - var_dim.mfa
  #' @description Calculates the contribution of a variable to a dimension
  #' 
}

table_dim.mfa = function(x) {
  #'
  #' @title Contribution of table to dimension - var_dim.mfa
  #' @description Calculates the contribution of a variable to a dimension
  #' 
}




## Supplementary Functions

rv <- function(x){
  #'
  #' @title Rv Coefficient - rv
  #' @description Calculates Rv coefficient between two tables. 
  #' @example rv(table1, table2) 
  #' 
  return
}

rv_table <- function(x){
  #'
  #' @title Rv Coefficient Table - rv_table
  #' @description Calculates Rv coefficient table between multiple sets  
  #' @example rv_table(dataset, sets = list(1:3, 4:5, 6:10))  
  #'          returns a 3-by-3 symmetric matrix.
  #' 
  return
}


lg <- function(x){
  #'
  #' @title Lg Coefficient - lg
  #' @description Calculates Lg coefficient between two tables. 
  #' @example lg(table1, table2) 
  #' 
  return
}

lg_table <- function(x){
  #'
  #' @title Lg Coefficient Table - lg_table
  #' @description Calculates Lg coefficient table between multiple sets  
  #' @example lg_table(dataset, sets = list(1:3, 4:5, 6:10))  
  #'          returns a 3-by-3 symmetric matrix.
  #' 
  return
}


