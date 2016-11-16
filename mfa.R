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
  if ( !is.null(ncomps) ) {
    if ( (as.integer(ncomps) != ncomps) ){
      stop("'ncomps' must be a integer indicating the number of components")
    }
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
  if (is.character(sets)){  
    if(!is.data.frame(data)){
      stop('"data" is not data.frame object, unable to parse with a character vector.')
    }
  }
  yTables = vector(mode = "list", length = length(sets))
  for (i in 1:length(sets)) {
      columns = sets[[i]]
      tab = data[,columns]
      yTables[[i]] = tab
  }

  # Scale center each table in tableList
  xTables = vector(mode = "list", length = length(yTables))
  for (i in 1:length(xTables)){
    xTables[[i]] = scale(yTables[[i]], center = center, scale = scale) 
  }
  
  # Factor scores, weights and Normalized Tables
  G = vector(mode = "list", length = length(xTables))
  a = vector()  # can proabably  pre-allocate memory properly
  alpha = vector(mode = "list", length = length(xTables))
  zTables = vector(mode = "list", length = length(xTables)) 
  
  for (k in 1:length(xTables)) {
    # Only use first ncomps
    totalComp = ncol(xTables[[k]])
    if (is.null(ncomps)){
      components = totalComp
    } else {
      components = ncomps
    }
    
    val = svd(xTables[[k]])
    G[[k]] =  val$u[,1:components] %*% diag(val$d[1:components])
    alpha[[k]] = (val$d[1]^-2)
    a = c(a, rep(alpha[[k]], ncol(val$u[,1:components])) ) 
    zTables[[k]] = G[[k]] %*% t(val$v[,1:components]) * val$d[1]^-1 
  }
  
  # Mass Matrix
  nObs = nrow(xTables[[1]])
  m = rep(1/nObs, nObs)
  M = diag(m)
  
  # Concatenate normalized tables  
  for (j in 1:length(zTables)){
    if (j == 1) {
      X = zTables[[j]]
    } else {
      X = cbind(X, zTables[[j]])
    }
  }
  
  # Calculate output from combined matrix
  decomp = svd(X)
  eigenvalues = (decomp$d)^2
  factorScores = decomp$u %*% diag(decomp$d)
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
  print(x$matrixLoadings)
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



