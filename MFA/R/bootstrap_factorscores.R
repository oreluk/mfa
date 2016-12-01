#' @export
bootstrap_factorscores = function(x, nSamples = 1000) UseMethod("bootstrap_factorscores")
bootstrap_factorscores.mfa = function(x, nSamples = 1000){
  #' @title Bootstrapping Factor Scores
  #' @description Calculates the bootstrap confidence intervals by sampling with replace of factor scores
  #' @param x is an mfa object
  #' @param nSamples is the number of times to sample with replacement for bootstrap estimates
  #' @return returns a 3-by-1 list containing the mean bootstrapped factor scores, the variance of the bootstrapped factor scores and the associated T star statistic.
  #' @export
  #'
  #' @examples 
  #' d <- loadWineData() 
  #' s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
  #'         seq(25,30), seq(31,35), seq(36,39), seq(40,45),
  #'         seq(46,50), seq(51,54) )
  #' a = mfa(d, s)
  #' bootEst = boostrap_factorscores(a)   
  #' This approach will sample 1000 times for estimates. 
  #' Optional:  bootEst = boostrap_factorscores(a, nSamples = 5000) where nSamples can be specified. 

  nSamples = 1000
  K = length(x$sets)
  F = vector(mode = "list", length = nSamples)
  
  #  Calculating the sum of factor scores, nSample times
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

  # Estimating variance by taking the difference of sampled factor scores squared 
  for (i in 1:nSamples) {
    if (i == 1) {
      Bootvar = (F[[i]] - Fmean)^2
    } else {
      Bootvar = Bootvar + (F[[i]] - Fmean)^2
    }
  }

  Bootvar = Bootvar/nSamples
  Sig= sqrt(Bootvar)
  Tstar = Fmean/Sig

  bootResults = list(Fmean=Fmean, Sig=Sig, Tstar=Tstar)
  return(bootResults)
}
