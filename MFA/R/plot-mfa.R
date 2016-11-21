
#' ## Plot Methods
#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  sz size of point on scatter plot
#' @export
#' @title Plot Compromise
#' @name plot compromise
#' @description plots compromise
plot_compromise = function(x, dim1=1, dim2=2, sz=2) UseMethod("plot_compromise")
#' @method plot_compromise mfa
#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  sz size of point on scatter plot
#' @title Plot Compromise
#' @name plot compromise
#' @description plots compromise
#' @export
plot_compromise.mfa <- function(x, dim1=1, dim2=2, sz=2) {
  #should plot the first 2 factor scores
  X = x$factorScores[,dim1]
  Y = x$factorScores[,dim2]

  plot(X, Y,
       type = "p", pch=19, cex=sz,xlim=c(-1,1),ylim=c(-1,1))
  text(X,Y,labels=1:length(X),col='red',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
  abline(v=0,h=0)
  title(paste0('Factor Scores for dimensions : ', dim1, ' , ', dim2 ))
}

#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  table which table to plot (integer 1:K)
#' @param  sz size of point on scatter plot
#' @export
#' @title Plot Partial Fac
#' @name plot partial fac
#' @description plots partial factor scores
plot_partial_fac = function(x, table=1, dim1=1, dim2=2, sz=2) UseMethod("plot_partial_fac")
#' @method plot_partial_fac mfa
#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  table which table to plot (integer 1:K)
#' @param  sz size of point on scatter plot
#' @title Plot Partial Fac
#' @name plot partial fac
#' @description plots partial factor scores
#' @export
plot_partial_fac.mfa <- function(x, table=1, dim1=1, dim2=2, sz=2) {
  #should plot the partial factor scores for dim1 and dim2
  X = x$partialFactorScores[[table]][,dim1] #fac score first table, 1st comp
  Y = x$partialFactorScores[[table]][,dim2] #fac score first table, 2nd comp

  plot(X, Y,
       type = "p", pch=19, cex=2,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
  text(X,Y,labels=1:length(X),col='red',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
  abline(v=0,h=0)
  title(paste0('Partial Factor Scores for Table ', table, ' , dimensions : ', dim1, ' , ', dim2 ))
}


#Note : for variable loadings, we first select a table 1:K, then we can plot the varibale loadings for
# that table (figure 3)

#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  table which table to plot (integer 1:K)
#' @param  sz size of point on scatter plot
#' @export
#' @title Plot Loadings
#' @name plot loadings
#' @description plots loadings for object of class mfa
plot_loading = function(x, table=1, dim1=1, dim2=2, sz=2) UseMethod("plot_loading")
#' @method plot_loading mfa
#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  table which table to plot (integer 1:K)
#' @param  sz size of point on scatter plot
#' @title Plot Loadings
#' @name plot loading
#' @description plots loadings for object of class mfa
#' @export
plot_loading.mfa <- function(x, table=1, dim1=1, dim2=2, sz=2) {
  #should plot the first 2 factor scores
  #keep all rows to include all variables for the table of interest,
  X = x$matrixLoadings[[table]][,dim1]
  Y = x$matrixLoadings[[table]][,dim2]
  # note this will be much more useful if we have the variable names not just indices!
  plot(X, Y,
       type = "p", pch=19, col='cyan',cex=sz,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
  text(X,Y,labels=names(X),col='black',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
  abline(v=0,h=0)
  title(paste0('Variable Loadings for Table ', table, ' , dimensions : ', dim1, ' , ', dim2 ))
}
