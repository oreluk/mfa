#' @export
plot.mfa <- function(x, sz=2) {
  #should plot the first 2 factor scores
  X = x$factorScores[,1]
  Y = x$factorScores[,2]

  plot(X, Y,
       type = "p", pch=19, cex=sz,xlim=c(-1,1),ylim=c(-1,1))
  text(X,Y,labels=1:length(X),col='red',xlim=c(-1,1),ylim=c(-1,1))
  abline(v=0,h=0)
  title('Factor Scores for first 2 components')
}
