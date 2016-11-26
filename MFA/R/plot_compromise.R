plot_compromise = function(x, dim1=1, dim2=2, sz=2) UseMethod("plot_compromise")
plot_compromise.mfa <- function(x, dim1=1, dim2=2, sz=2) {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  sz size of point on scatter plot
  #' @export
  #' @title Plot Compromise
  #' @name plot compromise
  #' @description plots compromise

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
