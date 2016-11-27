bootstrap_factorscores = function(x) UseMethod("bootstrap_factorscores")
bootstrap_factorscores = function(x){
  #' @title Bootstrapping Factor Scores
  #' @description Calculates the bootstrap confidence intervals by sampling with replace of factor scores
  #' @param x is an mfa object
  #' @return returns a 3-by-1 list containing the mean bootstrapped factor scores, the variance of the bootstrapped factor scores and the associated T star statistic.
  #' @export
  #'
  #' @examples b bootstrap factorscore
  #' from the set of tables. This approach also computes boostrap ratios for
  #' each dimension
  #'

  nSamples = 1000
  K = length(x$sets)
  F = vector(mode = "list", length = nSamples)
  for (i in 1:nSamples) {
    idx = sample(1:K, K, replace = TRUE)
    newDS = mfa(x$data, x$sets[idx])
    if (i == 1) {
      Fsum = newDS$factorScores
    } else {
      Fsum = newDS$factorScores + Fsum
    }
    F[[i]] = newDS$factorScores
  }

  Fmean = Fsum/nSamples

  for (i in 1:nSamples) {
    if (i == 1) {
      Sig = (F[[i]] - Fmean)^2
    } else {
      Sig = Sig + (F[[i]] - Fmean)^2
    }
  }

  Sig = Sig/nSamples
  Tstar = Fmean/Sig

  bootResults = list(Fmean, Sig, Tstar)
  return(bootResults)
}
