#' @param  obj An object of class mfa
#' @export
#' @title Plot Partial Fac
#' @name plot partial fac
#' @description plots partial factor scores
plot_partial_fac = function(obj, table=1, dim1=1, dim2=2, sz=2, obsnames=NULL, textcolor='black') UseMethod("plot_partial_fac", obj)
plot_partial_fac.mfa <- function(x, table=1, dim1=1, dim2=2, sz=2, obsnames=NULL, textcolor='black') {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  table which table to plot (integer 1:K)
  #' @param  obsnames input labels for your observations if desired
  #' @param  textcolor color of text labeling points
  #' @param  sz size of point on scatter plot
  #' @export
  #' @title Plot Partial Fac
  #' @name plot partial fac
  #' @description plots partial factor scores

  #plot the partial factor scores for dim1 and dim2
  X = x$partialFactorScores[[table]][,dim1] #fac score first table, 1st comp
  Y = x$partialFactorScores[[table]][,dim2] #fac score first table, 2nd comp

  # get the observation names if available:
  if (!is.null(obsnames)){
    obslabels <- obsnames
  } else {
    obslabels <- 1:length(X)
  }
  #widen left margin
  par(mar=c(5.1,8.1,4.1,2.1))
  plot(X, Y,
       type = "p", pch=19, col='white',
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2),
       xlim=c(min(X)-0.4,max(X)+0.4),
       ylim=c(min(Y)-0.4,max(Y)+0.4),
       cex=1,
       cex.axis = 1,
       cex.lab = 1.7)
  text(X,Y,labels=obslabels,col=textcolor,cex=sz)
  abline(v=0,h=0)
  title(paste0('Partial Factor Scores for Table ', table ), cex.main=2)
}
