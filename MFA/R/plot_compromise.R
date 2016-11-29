#' @export
plot_compromise = function(obj, dim1=1, dim2=2, sz=2, obsnames=NULL, textcolor='black') UseMethod("plot_compromise", obj)
plot_compromise.mfa <- function(x, dim1=1, dim2=2, sz=2, obsnames=NULL, textcolor='black') {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  sz size of text label in scatter plot
  #' @param  obsnames input labels for your observations if desired
  #' @param  textcolor color of text labeling points, can be one color or a vector of length num obs colors
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
  par(mar=c(5.1,8.1,4.1,2.1))

  # plot the data (white points are not visible, but text is added with text())
  plot(X, Y,
       type = "p", pch=19, col='white',
       xlab = paste0('Component ', dim1),
       ylab = paste0('Component ', dim2),
       xlim=c(min(X)-0.4,max(X)+0.4),
       ylim=c(min(Y)-0.4,max(Y)+0.4),
       cex=1,
       cex.axis = 1,
       cex.lab = 1.7)
  text(X,Y,labels=obslabels,col=textcolor,cex=sz)
  abline(v=0,h=0)
  title('Factor Scores',cex.main=2)
}
