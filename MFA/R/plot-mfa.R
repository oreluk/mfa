
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

#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  sz size of point on scatter plot
#' @export
#' @title Plot Compromise
#' @name plot compromise
#' @description plots compromise
plot_compromise.mfa <- function(x, dim1=1, dim2=2, sz=2) {
  #par(pty="s")
  #par(mar = c(5.1, 4.1, 0, 1))
  #should plot the first 2 factor scores
  X = x$factorScores[,dim1]
  Y = x$factorScores[,dim2]

  plot(X, Y,
       type = "p", pch=19, cex=sz,
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2),
       xlim=c(min(X)-0.4,max(X)+0.4),
       ylim=c(min(Y)-0.4,max(Y)+0.4))
  text(X,Y,labels=1:length(X),col='red')
  abline(v=0,h=0)
  title('Factor Scores')
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

#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  table which table to plot (integer 1:K)
#' @param  sz size of point on scatter plot
#' @export
#' @title Plot Partial Fac
#' @name plot partial fac
#' @description plots partial factor scores
plot_partial_fac.mfa <- function(x, table=1, dim1=1, dim2=2, sz=2) {
  #par(pty="s")
  #par(mar = c(5.1, 4.1, 0, 1))
  #should plot the partial factor scores for dim1 and dim2
  X = x$partialFactorScores[[table]][,dim1] #fac score first table, 1st comp
  Y = x$partialFactorScores[[table]][,dim2] #fac score first table, 2nd comp

  plot(X, Y,
       type = "p", pch=19, cex=2,
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2), # ,
       xlim=c(min(X)-0.4,max(X)+0.4),
       ylim=c(min(Y)-0.4,max(Y)+0.4))
  text(X,Y,labels=1:length(X),col='red')
  abline(v=0,h=0)
  title(paste0('Partial Factor Scores for Table ', table ))
}


#Note : for variable loadings, we first select a table 1:K, then we can plot the varibale loadings for
# that table (figure 3)

#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  table which table to plot (integer 1:K)
#' @param  sz size of point on scatter plot
#' @export
#' @title Plot Partial Fac
#' @name plot partial fac
#' @description plots partial factor scores
plot_loading = function(x, table=1, dim1=1, dim2=2, sz=2, varnames=NULL) UseMethod("plot_loading")

#' @param  x An object of class mfa
#' @param  dim1 dimension for x axis, default 1st component
#' @param  dim2 dimension for y axis, default 2nd component
#' @param  table which table to plot (integer 1:K)
#' @param  sz size of point on scatter plot
#' @export
#' @title Plot Partial Fac
#' @name plot partial fac
#' @description plots partial factor scores
plot_loading.mfa <- function(x, table=1, dim1=1, dim2=2, sz=2, varnames=NULL) {
  #par(pty="s")
  #par(mar = c(5.1, 4.1, 0, 1))
  #should plot the first 2 factor scores
  #keep all rows to include all variables for the table of interest,
  X = x$matrixLoadings[[table]][,dim1]
  Y = x$matrixLoadings[[table]][,dim2]
  #have labels be the names of X,Y if no varnames mapping is given:
  if (!is.null(varnames)){
    varlabels <- names(X)
    ind = 1
    for (n in names(X)){
      varlabels[ind] <- varnames[[n]]
      ind <- ind + 1
    }
  } else {
    varlabels <- names(X)
  }

  #sometimes (e.g. random data martix), the names(X) will be null
  # in this case, set varlabels to 1:length(X)
  if (is.null(names(X))){ varlabels <- 1:length(X)}

  # note this will be much more useful if we have the variable names not just indices!
  plot(X, Y,
       type = "p", pch=19, col='white',cex=sz, #,xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2),
       xlim=c(min(X)-0.4,max(X)+0.4),
       ylim=c(min(Y)-0.4,max(Y)+0.4))
  text(X,Y,labels=varlabels,col='black')
  abline(v=0,h=0)
  title(paste0('Variable Loadings for Table ', table ))
}
