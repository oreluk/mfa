#' @export
obs_dim = function(obj) UseMethod("obs_dim")
obs_dim.mfa = function(x) {
  #' @title Contribution: observation to dimension
  #' @description Calculates the contribution of an observation to a dimension.
  #' @param x is an mfa object
  #' @export
  #' @return a matrix n-by-m where n is the number of observations and m is the number of dimensions
  #'

  # Initialize the matrix to store the observation contribution
  ctr_obs = x$factorScores

  # m value
  m = 1/nrow(ctr_obs)

  # looping over all dimensions
  for (l in 1:length(x$eigenvalues)) {
    ctr_obs[,l] = m * x$factorScores[,l]^2 / x$eigenvalues[l]
  }
  return(ctr_obs)
}
