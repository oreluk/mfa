#' @param  obj An object of class mfa
#' @export
#' @title Plot Loadings
#' @name plot matrix loadings
#' @description plots matrix loadings
plot_loading = function(obj, table=1, dim1=1, dim2=2, sz=2, varnames=NULL) UseMethod("plot_loading", obj)
plot_loading.mfa <- function(x, table=1, dim1=1, dim2=2, sz=2, varnames=NULL) {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  table which table to plot (integer 1:K)
  #' @param  sz size of text label in plot
  #' @param  varnames variable labels
  #' @export
  #' @title Plot Loadings
  #' @name plot matrix loadings
  #' @description plots matrix loadings

  #Note : for variable loadings, we first select a table 1:K, then we can plot the varibale loadings for
  # that table (figure 3)

  #should plot the first 2 factor scores
  #keep all rows to include all variables for the table of interest,
  X = x$loadingByTable[[table]][,dim1]
  Y = x$loadingByTable[[table]][,dim2]
  #have labels be the names of X,Y if no varnames mapping is given:
  if (!is.null(varnames)){
    varlabels <- names(X)
    ind = 1
    for (n in names(X)){
      varlabels[ind] <- varnames[n]
      ind <- ind + 1
    }
  } else {
    varlabels <- names(X)
  }
  library(RColorBrewer)
  darkcols <- brewer.pal(length(X), "Dark2")

  #sometimes (e.g. random data martix), the names(X) will be null
  # in this case, set varlabels to 1:length(X)
  if (is.null(names(X))){ varlabels <- 1:length(X)}
  #widen left margin
  par(mar=c(5.1,8.1,4.1,2.1))
  # note this will be much more useful if we have the variable names not just indices!
  plot(X, Y,
       type = "p", pch=19, col='white',
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2),
       xlim=c(min(X)-0.4,max(X)+0.4),
       ylim=c(min(Y)-0.4,max(Y)+0.4),
       cex=1,
       cex.axis = 1,
       cex.lab = 1.7)
  text(X,Y,labels=varlabels,col=darkcols,cex=sz)
  abline(v=0,h=0)
  title(paste0('Variable Loadings for Table ', table ),cex.main=2)
}
