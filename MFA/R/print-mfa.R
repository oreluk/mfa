#' @export
print.mfa = function(x) {
  cat('object "mfa"\n')
  cat('Maximum Eigenvalue: ')
  print(max(x$eigenvalues))
  cat('Scores for first two components: ')
  print(x$factorScores[,1:2])
  cat('Partial Scores for the first two components:  ')
  for (i in 1:length(x$partialFactorScores)){
    print(paste0("Table ",i,":"))
    print(x$partialFactorScores[[i]][,1:2])
  }
  cat('Loadings for the first two components:  ')
  for (i in 1:length(x$matrixLoadings)){
    print(paste0("Table ",i,":"))
    print(x$matrixLoadings[[i]][,1:2])
  }
  invisible(x)
}
