#' @export
print.mfa = function(x) {
  cat('object "mfa"\n')
  cat('Maximum Eigenvalue: ')
  print(max(x$eigenvalues))
  cat('Scores: ')
  print(x$factorScores)
  cat('Partial Scores:  ')
  print(x$partialFactorScores)
  cat('Loadings:  ')
  print(x$matrixLoadings)
  invisible(x)
}
