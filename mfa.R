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
  if (!is.list(sets) & !is.character(sets)) {
    stop("'sets' must be a character vector or list containing vectors indicating the sets of variables")
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



## Methods 

eigenvalueTable = function(x, ...) UseMethod("eigenvalueTable")

eigenvalueTable.mfa = function(obj) {
  #'
  #' @title EigenvalueTable - eigenvalueTable.mfa
  #' @description Takes a matrix x, returns a table including: 
  #' singular values, eigenvalues, cumulative, percentage of inertia,
  #' cumulative percentage of inertia, for all the extracted components.
  #' 
  x = obj$X
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





# MFA Constructor
mfa = function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE) {
  # Checks validity of inputs
  check_inputs(data, sets, ncomps, center, scale)
  
  # Create list of tables
  if (is.character(sets)){  # character vectors
   # incomplete. two cases: unique names V1, V1.1 &   
   # if check.names = FALSE on import, multiple entries of V1, how to handle cases.  
  }
  else if (is.list(sets)) { 
    xTables = vector(mode = "list", length = length(sets))
    for (i in 1:length(sets)) {
        columns = sets[[i]]
        tab = data[,columns]
        xTables[[i]] = tab
    }
  }
  
  # Scale center each table in tableList
  for (i in 1:length(xTables)){
    xTables[[i]] = scale(xTables[[i]], center = center, scale = scale) # verify that this is normalizing each column for mean = 0, sum = 1...
  }
  
  # Factor scores, weights and Normalized Tables
  g = vector(mode = "list", length = length(xTables))
  a = vector()  # need to pre-allocate memory properly...
  alpha = vector(mode = "list", length = length(xTables))
  zTables = vector(mode = "list", length = length(xTables)) # Normalized Tables (Z)
  
  for (k in 1:length(xTables)) {
    val = svd(xTables[[k]])
    g[[k]] =  val$u %*% val$d
    a = c(a, (val$d)^-2 )
    alpha[[k]] = (val$d[1]^-2)
    zTables[[k]] = xTables[[k]] * val$d[1]^-1 
  }
  
  
  ## Concatenate normalized tables  
  for (j in 1:length(zTables)){
    if (j == 1) {
      X = zTables[[j]]
    } else {
      X = cbind(X, zTables[[j]])
    }
  }
  
  #- Concatenate can be done more elegantly by preallocating space.
  # Allocation X works. 
  # 
  # numCol = lapply(zTables, ncol)
  # numCol = Reduce("+", numCol) 
  # numRow = nrow(zTables[[1]])  
  # X = matrix(nrow = numRow, ncol = numCol
  #
  # FOR-LOOP DOES NOT WORK
  #
  #  for (j in 1:length(zTables)){
  #      nC = ncol(zTables[[j]])
  #      X[??] = zTables[[j]]
  #  }
  
  
  
  # Calculate output
  decomp = svd(X)
  eigenvalues = (decomp$d)^2
  factorScores = decomp$u %*% decomp$d
  pFactorScores = vector(mode = "list", length = length(xTables)) 
  for (k in 1:length(xTables)){
    a = svd(xTables[[k]])
    pFactorScores[[k]] = length(sets) * alpha[[k]] * xTables[[k]] %*% t(a$v)
  }
  matrixLoadings = decomp$v
  
  obj = list(data=data, sets=sets, ncomps=ncomps, center=center, scale=scale, 
             eigenvalues=eigenvalues, factorScores=factorScores, 
             partialFactorScores=pFactorScores, 
             matrixLoadings=matrixLoadings, 
             X = X)

  class(obj) <- "mfa"
  return(obj)
}


print.mfa = function(x, ...) {
  #'
  #' @title print.mfa
  #' @description Overloading print method for mfa, returns basic info 
  #' 
  cat('object "mfa"\n')
  cat('Maximum Eigenvalue: ')
  print(max(x$eigenvalues))
  cat('Scores: ')
  print(x$factorScores)
  cat('Partial Scores:  ')
  print(x$partialFactorScores)
  cat('Loadings:  ')
  print(x$maxtrixLoadings)
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


