#' @param  obj An object of class mfa
#' @export
#' @title Plot Pie Chart of Inertia
#' @name plot inertia pie chart
#' @description plots bar graph of eigenvals
plot_inertia_pie = function(obj) UseMethod("plot_inertia_pie", obj)
plot_inertia_pie.mfa <- function(x) {
  #' @param  x An object of class mfa
  #' @export
  #' @title Plot Pie Chart of Inertia
  #' @name plot inertia pie chart
  #' @description plots a pie chart from mfa objects eigenvalue table

  # first get the inertias from the ev table..
  inertias <- eigenvalueTable(mfa_obj)['percentInertia',]
  darkcols <- rainbow(length(inertias),s=0.5)
  # Simple Pie Chart
  labels=1:length(inertias)
  maintitle = paste0('% Inertia for the ', length(inertias), ' components')
  pie(inertias, labels, col=darkcols, radius=1,
      cex=1.5)
  title(maintitle,cex.main=2)

}
