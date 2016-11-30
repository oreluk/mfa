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
#' # wine data example
#' # load the data. function is provided in this package
#' d = loadWineData()
#' # define list of index vectors that correspond to subtable indices
#' s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
#'           seq(25,30), seq(31,35), seq(36,39), seq(40,45),
#'           seq(46,50), seq(51,54) )
#' # run mfa on the data d and sets s
#' a = mfa(d, s)
#' print(a)
#'
#' # random data example
#' # create a data set of random integers ranging from 50 to 100
#' nrows=14 #number of observations (like the wines in first example)
#' nvars=33 #number of vars (across all tables)
#' d2 <- replicate(nvars,sample(50:100,size=nrows,rep=TRUE))
#' # define indices that represent 6 sub-tables of varying size
#' s2 = list(  seq(1,4), seq(5,10), seq(11,19), seq(20,22),
#'             seq(23,30), seq(31,33))
#' a2 = mfa(d2, s2, ncomps=3)
#' print(a2)
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


  # Scale and center each table in tableList
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

  # Individual Table SVDs to compute table weights
  aVector = vector()  
  alpha = vector(mode = "list", length = length(xTables))

  for (k in 1:length(xTables)) {
    val = svd(xTables[[k]])
    alpha[[k]] = (val$d[1]^-2)
    aVector = c(aVector, rep(alpha[[k]], ncol(val$u)) )
  }

  # Calculating mass of observations
  nObs = nrow(xTables[[1]])
  m = rep(1/nObs, nObs)

  # Concatenate individual tables into 'grand' table
  for (j in 1:length(xTables)){
    if (j == 1) {
      X = xTables[[j]]
    } else {
      X = cbind(X, xTables[[j]])
    }
  }

  # Calculate GSVD
  # X = P \Delta Q^T
  xTilde = diag(m^(1/2)) %*% X %*% diag(aVector^(1/2))  # \tilde{X} = M^{1/2} A W^{1/2}
  xDecomp = svd(xTilde)  # \tilde{A} = P \Delta Q^T
  eigenvalues = (xDecomp$d)^2

  P = diag(m^(-1/2)) %*% xDecomp$u
  Q = diag(aVector^(-1/2)) %*% xDecomp$v

  if (is.null(ncomps)){
    components = length(eigenvalues)
  } else {
    components = ncomps
  }
  factorScores = P[,1:components] %*% diag(xDecomp$d[1:components])

  # Calculating Partial Factor Scores ( Factor Scores for each table )
  pFactorScores = vector(mode = "list", length = length(xTables))
  iterate = 0
  for (k in 1:length(xTables)) {
    len = length(sets[[k]])
    if (k != 1) {
      iterate = 0
      for (i in 1:k) {
        if (i != k) {
          iterate = iterate + length(sets[[i]])
        }
      }
    }
    pFactorScores[[k]] = length(sets) * alpha[[k]] * xTables[[k]] %*% Q[(iterate+1):(iterate+length(sets[[k]])),][,1:components]
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
             aVector = aVector,
             partialFactorScores=pFactorScores,
             loadingByTable=matrixLoadingsList,
             matrixLoadings=matrixLoadings,
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
    stop("'sets' must be a list containing a character vector or list containing vectors indicating the sets of variables")
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


