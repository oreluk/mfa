#' @param  obj An object of class mfa
#' @export
#' @title Plot Bar Graph of Eigenvalues
#' @name plot eigenvalue bar graph
#' @description plots bar graph of eigenvals
plot_ev = function(obj) UseMethod("plot_ev", obj)
plot_ev.mfa <- function(x) {
  #' @param  x An object of class mfa
  #' @export
  #' @title Plot Bar Graph of Eigenvalues
  #' @name plot eigenvalue bar graph
  #' @description plots bar graph of eigenvals

  # first get the ev table..
  evs <- mfa_obj$eigenvalues
  # get nice colors with colorbrewer:
  darkcols <- rainbow(length(evs),s=0.5)
  par(mar=c(5.1,8.1,4.1,2.1))
  # Simple Bar Plot
  barplot(evs,
          names.arg = 1:length(evs),
          xlab = 'Eigenvalue',
          ylab = 'Magnitude',
          cex=1,
          cex.axis = 2,
          cex.names = 2,
          cex.lab = 2,
          col=darkcols)
  title("Eigenvalues for each component",cex.main=2)
          #col=rainbow(length(evs)))
}
