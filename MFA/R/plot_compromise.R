#' @export
plot_compromise = function(obj, dim1=1, dim2=2, sz=1, obsnames=NULL, textcolor='black',
                           cexmain=1,cexlab=1, cexaxis=0.8, app=FALSE, dotcol='white') UseMethod("plot_compromise", obj)
plot_compromise.mfa <- function(x, dim1=1, dim2=2, sz=1, obsnames=NULL, textcolor='black',
                                cexmain=1,cexlab=1, cexaxis=0.8, app=FALSE, dotcol='white') {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  sz size of text label in scatter plot
  #' @param  obsnames input labels for your observations if desired
  #' @param  textcolor color of text labeling points, can be one color or a vector of length num obs colors
  #' @param  cexmain size for main title label, default 1, 2 used for app
  #' @param  cexlab size for axis labels, default 1, 1.7 used for app
  #' @param  cexaxis size for axis tick labels, default 0.8
  #' @param  dotcol choose color for the dots, default white (don't show up)
  #' @param  app if true, this is for the shiny app and margins are changed
  #' @export
  #' @title Plot Compromise
  #' @name plot compromise
  #' @description plots the compromise, i.e. the common factor scores, for 2 given dimensions of an mfa table
  X = x$factorScores[,dim1]
  Y = x$factorScores[,dim2]

  # get the observation names if available:
  if (!is.null(obsnames)){
    obslabels <- obsnames
  } else {
    obslabels <- 1:length(X)
  }

  # widen the left margin
  if (app==TRUE) {par(mar=c(5.1,8.1,4.1,2.1))}

  # plot the data (white points are not visible, but text is added with text())
  plot(X, Y,
       type = "p", pch=19, col=dotcol,
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2),
       xlim=c(min(X)-0.4,max(X)+0.4),
       ylim=c(min(Y)-0.4,max(Y)+0.4),
       cex=1,
       cex.axis = cexaxis,
       cex.lab = cexlab)
  text(X,Y,labels=obslabels,col=textcolor,cex=sz)
  abline(v=0,h=0)
  title('Factor Scores',cex.main=cexmain)
}

#save for app cex=1,cex.axis=1,cex.lab=1.7, cexmain=2
