bootstrap_factorscores = function(x){
  #' @title Bootstrapping Factor Scores
  #' @description Calculates the bootstrap confidence intervals by sampling
  #' @param x An object of class mfa
  #' @export
  #'
  #' @examples b bootstrap factorscore
  #' from the set of tables. This approach also computes boostrap ratios for
  #' each dimensionnnnnnnnnnn.
  #'

  # 1) Sample integers with replacement from 1 to K
  K = length(sets)
  idx = sample(1:K, K, replace = TRUE)
  # 2) Create a new dataset with these sampled tables. {X_1, X_1, X_3, X_12, ..}
  # 3) Build a matrix X^*_1
  # 4) USE MFA.
  # 5) Calculate Factor Scores (boot strapped)
  # 6) Repeat 1K times
  # 7) L bootstrapped matrices of factor scores F^*_l

}
