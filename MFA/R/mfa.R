#' @title Multiple Factor Analysis (MFA)
#' @author Yulin Chen, Stephanie Wuerth, Eren Bilir, Jim Oreluk
#' @description Creates an object of class \code{"mfa"}
#' @param data A data frame or matrix which you'd like to analyze
#' @param sets A list of indices to group sets of variables
#' @param ncomps Number of principle components to retain
#' @param center either a logical value or a numeric vector of length equal to the number of active variables in the analysis
#' @param scale either a logical value or a numeric vector of length equal to the number of active variables in the analysis
#' @export
#' @examples
#' filename = system.file("extdata", "wines.csv", package = "MFA")
#' d = read.csv(filename, header=TRUE, check.names=FALSE)
#' s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
#'           seq(25,30), seq(31,35), seq(36,39), seq(40,45),
#'           seq(46,50), seq(51,54) )
#'
#' a = mfa(d, s)
#' print(a)
#'
mfa <- function(data, sets, ncomps = NULL, center = TRUE, scale = TRUE) {
  # Checks validity of inputs
  check_inputs(data, sets, ncomps, center, scale)

  # Create list of tables
  yTables = vector(mode = "list", length = length(sets))
  minVar = Inf
  for (i in 1:length(sets)) {
      columns = sets[[i]]
      if ( i == 1 | (min(columns) < minVar) ) {
        minVar = min(columns)
      }
      tab = data[,columns]
      yTables[[i]] = tab
  }
  if (is.character(minVar)){
    minVar = which(pmatch(names(data), minVar) == 1)
  }


  # Scale center each table in tableList
  xTables = vector(mode = "list", length = length(yTables))
  for (i in 1:length(xTables)){
    if (!is.logical(center)){
      idx = sets[[i]]
      if (is.character(idx)) {
        idx = which( !is.na(pmatch(names(data),  idx)), arr.ind=TRUE)
      }
      tableCenter = center[idx - (minVar - 1)]
    } else {
      tableCenter = center
    }
    if ( !is.logical(scale)) {
      idx = sets[[i]]
      if (is.character(idx)) {
        idx = which( !is.na(pmatch(names(data),  idx)), arr.ind=TRUE)
      }
      tableScale = scale[idx - (minVar - 1)]
    } else {
      tableScale = scale
    }
    xTables[[i]] = scale(yTables[[i]], center = tableCenter, scale = tableScale)
    xTables[[i]] = xTables[[i]] / sqrt(nrow(xTables[[i]])-1)  # this is used to match the paper results
  }

  # Factor scores, weights and Normalized Tables
  G = vector(mode = "list", length = length(xTables))
  a = vector()  # can proabably  pre-allocate memory properly
  alpha = vector(mode = "list", length = length(xTables))

  for (k in 1:length(xTables)) {
    val = svd(xTables[[k]])
    G[[k]] =  val$u %*% diag(val$d)
    alpha[[k]] = (val$d[1]^-2)
    a = c(a, rep(alpha[[k]], ncol(val$u)) )
  }

  # Calculating mass of observations
  nObs = nrow(xTables[[1]])
  m = rep(1/nObs, nObs)

  # Concatenate normalized tables
  for (j in 1:length(xTables)){
    if (j == 1) {
      X = xTables[[j]]
    } else {
      X = cbind(X, xTables[[j]])
    }
  }

  # Calculate GSVD
  # X = P \Delta Q^T
  xTilde = diag(m^(1/2)) %*% X %*% diag(a^(1/2))  # \tilde{X} = M^{1/2} A W^{1/2}
  xDecomp = svd(xTilde)  # \tilde{A} = P \Delta Q^T
  eigenvalues = (xDecomp$d)^2

  P = diag(m^(-1/2)) %*% xDecomp$u
  Q = diag(a^(-1/2)) %*% xDecomp$v

  if (is.null(ncomps)){
    components = length(eigenvalues)
  } else {
    components = ncomps
  }
  factorScores = P[,1:components] %*% diag(xDecomp$d[1:components])


  pFactorScores = vector(mode = "list", length = length(xTables))
  for (k in 1:length(xTables)) {
      pFactorScores[[k]] = length(sets) * alpha[[k]] * xTables[[k]] %*% Q[a == alpha[[k]],][,1:components]
  }

  matrixLoadings = Q[,1:components]

  matrixLoadingsList = vector(mode = "list", length = length(xTables))
  start_ind = 1
  for (k in 1:length(xTables)) {
    final_ind = start_ind+length(sets[[k]])-1
    matrixLoadingsList[[k]] = matrixLoadings[start_ind:final_ind,]
    start_ind = start_ind+length(sets[[k]])
    #set row names on individual tables to the variable names
    rownames(matrixLoadingsList[[k]]) <- colnames(data[,sets[[k]]])
  }

  obj = list(data=data, sets=sets, ncomps=ncomps, center=center, scale=scale,
             eigenvalues=eigenvalues,
             factorScores=factorScores,
             alpha = alpha,
             partialFactorScores=pFactorScores,
             matrixLoadings=matrixLoadingsList,
             X = X)

  class(obj) <- "mfa"
  return(obj)
}

# private function to check inputs
check_inputs = function(data, sets, ncomps, center, scale) {
  if (!is.matrix(data) &
      !is.data.frame(data) ) {
    stop("'data' must be a matrix or data.frame containing the data set")
  }

  # sets
  if (!is.list(sets)) {
    stop("'sets' must be a list containting a character vector or list containing vectors indicating the sets of variables")
  } else if ( is.character(sets[[1]]) ) {
    if(!is.data.frame(data)) {
      stop('"data" is not data.frame object, unable to parse with a character vector.')
    }
  }
  nVars = sum(lengths(sets))

  # ncomps
  if ( !is.null(ncomps) ) {
    if ( (as.integer(ncomps) != ncomps) ){
      stop("'ncomps' must be a integer indicating the number of components")
    }
  }

  # Center
  if (!is.logical(center)) {
    if (!is.vector(center) |
        (is.vector(center) & length(center) != nVars) ) {
      stop("'center' must be a logical or numeric
           vector equal to the number of active variables")
    }
    }

  # Scale
  if (!is.logical(scale)) {
    if (!is.vector(scale) |
        (is.vector(scale) & length(scale) != nVars )) {
      stop("'scale' must be a logical or numeric
           vector equal to the number of active variables")
    }
  }
}


## Methods
#' @param  x An object of class mfa
#' @export
#' @title Eigenvalue Table
#' @name eigenvalue table
#' @description eigenvalue table from mfa obj
eigenvalueTable = function(x) UseMethod("eigenvalueTable")

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


## Contributions

obs_dim.mfa = function(obj) {
  # Initialize the matrix to store the observation contribution
  ctr_obs=obj$factorScores
  # m value
  m=1/nrow(ctr_obs)
  # Eq.25
    for (l in 1: length(obj$eigenvalues)) {
      ctr_obs[,l] = m*obj$factorScores[,l]^2/obj$eigenvalues[l]
      }
  return(ctr_obs)
}


var_dim.mfa = function(obj) {
  # Initialize the matrix to store the variable contribution
  ctr_var=obj$matrixLoadings
  # Eq.27 (the row of matrixLoadings is dimension?)
  for (j in 1:nrow(ctr_var)){
    ctr_var[j,]= obj$alpha[j]*obj$matrixLoadings[j,]^2
  }
  return(ctr_var)
}


table_dim.mfa = function(obj, ctr_var, numvar_table) {
  # Initialize the matrix to store the table contribution
  ctr_var=matrix(rep(1,numvar_table*ncol(ctr_col)),
                 nrow = numvar_table, ncol = ncol(ctr_col))
  # Counter
  j=1
  # Eq.28
  for (l in ncol(ctr_var)){
    for (k in 1:length(numvar_table)) {
      ctr_table[k,l]=sum(ctr_var[j:(j+numvar_table[k]),l])
      j=j+numvar_table[k]+1
    }
  }
  return(ctr_table)
}


## Supplementary Functions

#' @title Rv Coefficient - rv
#' @description Calculates Rv coefficient between two tables
#' @param table1 is the first input table
#' @param table2 is the second input table
#' @examples
#' table1 ...
#' table2 ...
#' rv(table1,table2) returns the rv coefficient between table1 and table2
#'
rv <- function(table1, table2){
  # Numerator:
  num=sum(diag(tcrossprod(table1) %*% tcrossprod(table2)))
  # Denominator:
  den_part1=sum(diag(tcrossprod(table1) %*% tcrossprod(table1)))
  den_part2=sum(diag(tcrossprod(table2) %*% tcrossprod(table2)))
  den=sqrt(den_part1*den_part2)

  rv=num/den
}


#' @title Rv Coefficient Table - rv_table
#' @description Calculates Rv coefficient table multiple sets from a dataset
#' @param dataset is input data in as a data.frame object.
#' @param sets is the list of indices to be parsed from dataset
#' @examples
#' dataset = ...
#' rv_table(dataset, sets = list(1:3, 4:5, 6:10))
#' returns a 3-by-3 symmetric matrix
#'
rv_table <- function(dataset, sets) {
  # Initialize rv table
  rvtable=matrix(rep(1,length(sets)^2),nrow=length(sets))

  # To calculate elements for rvtable: diagonal equals to 1, and
  # the matrix is symetric.
  for (i in 1:length(sets)) {
    for (j in 1:length (sets)) {
      if (i==j){
        rvtable[i,j]=1
      } else if (j<i){
        rvtable[i,j]=rvtable[j,i]
      } else {
        rvtable[i,j]=rv(dataset[,sets[[i]]],dataset[,sets[[j]]])
      }
    }
  }
  return(rvtable)
}


#' @title Lg Coefficient - lg
#' @description Calculates Lg coefficient between two tables
#' @param table1 is the first input table
#' @param table2 is the second input table
#' @examples
#' table1 ...
#' table2 ...
#' lg(table1,table2) returns the rv coefficient between table1 and table2
#'
lg <- function(table1, table2){
  # Numerator:
  num=sum(diag(tcrossprod(table1) %*% tcrossprod(table2)))
  # Denominator: (need to have a function to calculate alpha)
  den= # new function for alpha is needed

  lg=num/den

}


#' @title Lg Coefficient Table - lg_table
#' @description Calculates Lg coefficient table between multiple sets from a dataset
#' @param dataset is input data in as a data.frame object.
#' @param sets is the list of indices to be parsed from dataset
#' @examples
#' filename = system.file("extdata", "wines.csv", package = "MFA")
#' dataset = read.csv(filename, header=TRUE, check.names=FALSE)
#' lg_table(dataset, sets = list(1:3, 4:5, 6:10))
#' returns a 3-by-3 symmetric matrix.
#'
lg_table <- function(dataset, sets){
  # Initialize lg table
  lgtable=matrix(rep(1,length(sets)^2),nrow=length(sets))

  # To calculate elements for lgtable: diagonal equals to 1, and
  # the matrix is symetric.
  for (i in 1:length(sets)) {
    for (j in 1:length(sets)) {
      if (i==j){
        lgtable[i,j]=1
      } else if (j<i){
        lgtable[i,j]=lgtable[j,i]
      } else {
        lgtable[i,j]=lg(dataset[,sets[[i]]],dataset[,sets[[j]]])
      }
    }
  }
  return(lgtable)
}



#' @title Bootstrapping Factor Scores
#' @description Calculates the bootstrap confidence intervals by sampling
#' @param x An object of class mfa
#' @return b bootstrap factorscore
#' from the set of tables. This approach also computes boostrap ratios for
#' each dimension.
#'
bootstrap_factorscore = function(x){
  # 1) Sample integers with replacement from 1 to K
  K = length(sets)
  idx = sample(1:K, K, replace = TRUE)
  # 2) Create a new dataset with these sampled tables. {X_1, X_1, X_3, X_12, ..}
  # 3) Build a matrix X^*_1
  # 4) USE MFA.
  # 5) Calculate Factor Scores (boot strapped)
  # 6) Repeat 1K times
  # 7) L bootstrapped matrices of factor scores F^*_l

}



