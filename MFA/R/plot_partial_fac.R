plot_partial_fac = function(x, table=1, dim1=1, dim2=2, sz=2) UseMethod("plot_partial_fac")
plot_partial_fac.mfa <- function(x, table=1, dim1=1, dim2=2, sz=2) {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  table which table to plot (integer 1:K)
  #' @param  sz size of point on scatter plot
  #' @export
  #' @title Plot Partial Fac
  #' @name plot partial fac
  #' @description plots partial factor scores

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
