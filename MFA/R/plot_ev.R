#' @param  obj An object of class mfa
#' @export
#' @title Plot EV bar graph
#' @name plot eigenvalue bar graph
#' @description plots bar graph of eigenvals
plot_ev = function(obj) UseMethod("plot_ev", obj)
plot_ev.mfa <- function(x) {
  #' @param  x An object of class mfa
  #' @export
  #' @title Plot EV bar graph
  #' @name plot eigenvalue bar graph
  #' @description plots bar graph of eigenvals

  # first get the ev table..
  evs <- eigenvalueTable(x)['eig',]
  # Simple Bar Plot
  counts <- table(mtcars$gear)
  barplot(evs, main="Eigenvalues",
          names.arg = 1:length(evs))
}
