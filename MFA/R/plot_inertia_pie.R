#' @export
plot_inertia_pie = function(obj, cexmain = 1, cexlab = 1, radius = 1, app = FALSE) UseMethod("plot_inertia_pie", obj)
plot_inertia_pie.mfa <- function(x, cexmain = 1, cexlab = 1, radius = 1, app = FALSE) {
  #' @param  x An object of class mfa
  #' @param  cexmain size for main title
  #' @param  cexlab size for labels of pie slices
  #' @param  app set to TRUE if you are using this in the shiny app context
  #' @param  radius radius of circle, default 1
  #' @export
  #' @title Plot Pie Chart of Inertia
  #' @name plot inertia pie chart
  #' @description plots a pie chart of percent inertia (from mfa objects eigenvalue table)

  # first get the inertias from the ev table..
  inertias <- eigenvalueTable(x)['percentInertia',]

  # define a color palette
  darkcols <- rainbow(length(inertias),s=0.5)

  # create the pie chart
  labels <- 1:length(inertias)
  maintitle <- paste0('% Inertia for the ', length(inertias), ' components')
  pie(inertias, labels, col = darkcols, radius = radius,
      cex = cexlab, xaxs = "r", yaxs = "r")
  title(maintitle, cex.main = cexmain)

  #if not the app, make margins smaller
  if (app == FALSE){par(mar=c(1.1,1.1,1.1,1.1))}
}
