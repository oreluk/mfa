#' @export
plot_partial_fac = function(obj, table=1, dim1=1, dim2=2, sz=1, obsnames=NULL, textcolor='black',
                            cexmain=1,cexlab=1, cexaxis=0.8, app=FALSE) UseMethod("plot_partial_fac", obj)
plot_partial_fac.mfa <- function(x, table=1, dim1=1, dim2=2, sz=1, obsnames=NULL, textcolor='black',
                                 cexmain=1,cexlab=1, cexaxis=0.8, app=FALSE) {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  table which table to plot (integer 1:K)
  #' @param  obsnames input labels for your observations if desired
  #' @param  textcolor color of text labeling points, either a single color or a vector of colors equal to number of obs in mfa object
  #' @param  sz size of text labeling points on scatterplot
  #' @param  cexmain size for main title label, default 1, 2 used for app
  #' @param  cexlab size for axis labels, default 1, 1.7 used for app
  #' @param  cexaxis size for axis tick labels, default 0.8
  #' @param  app if TRUE, this is for the shiny app and margins are changed
  #' @export
  #' @title Plot Partial Factor Scores
  #' @name plot partial factor scores
  #' @description plots partial factor scores for 2 given dimensions/components of a given table in an mfa object

  # select the table and dimension of interest, and store in X (dim1) and Y (dim2)
  X = x$partialFactorScores[[table]][,dim1] #fac score first table, 1st comp
  Y = x$partialFactorScores[[table]][,dim2] #fac score first table, 2nd comp

  # get the observation names if available:
  if (!is.null(obsnames)){
    obslabels <- obsnames
  } else {
    obslabels <- 1:length(X)
  }

  # widen left margin to accommodate large axis label font
  if (app==TRUE) {par(mar=c(5.1,8.1,4.1,2.1))}

  # plot the data (white points are not visible, but text is added with text())
  plot(X, Y,
       type = "p", pch=19, col='white',
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2),
       xlim=c(min(X)-0.4,max(X)+0.4),
       ylim=c(min(Y)-0.4,max(Y)+0.4),
       cex=1,
       cex.axis = cexaxis,
       cex.lab = cexlab)
  text(X,Y,labels=obslabels,col=textcolor,cex=sz)
  abline(v=0,h=0)
  title(paste0('Partial Factor Scores for Table ', table ), cex.main=cexmain)
}
