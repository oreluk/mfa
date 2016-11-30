#' @export
plot_ev = function(obj,cexaxis=1,cexnames=1, cexlab=1,
                   cexmain=1,app=FALSE) UseMethod("plot_ev", obj)
plot_ev.mfa <- function(x,cexaxis=1,cexnames=1, cexlab=1,
                        cexmain=1,app=FALSE) {
  #' @param  x An object of class mfa
  #' @param cexaxis, size axis labels
  #' @param cexnames, size names
  #' @param cexlab, size labels
  #' @param cexmain size main title, default 1
  #' @param app if using shiny, set to true to fix margins
  #' @export
  #' @title Plot Bar Graph of Eigenvalues
  #' @name plot eigenvalue bar graph
  #' @description plots bar graph of eigenvalues from an mfa object

  # first get the ev table..
  evs <- x$eigenvalues
  # get nice colors with colorbrewer:
  darkcols <- rainbow(length(evs),s=0.5)
  if (app==TRUE) {par(mar=c(5.1,8.1,4.1,2.1))}
  # Simple Bar Plot
  barplot(evs,
          names.arg = 1:length(evs),
          xlab = 'Eigenvalue',
          ylab = 'Magnitude',
          cex=1,
          cex.axis = cexaxis,
          cex.names = cexnames,
          cex.lab = cexlab,
          col=darkcols)
  title("Eigenvalues for each component",cex.main=cexmain)
          #col=rainbow(length(evs)))
}
#cexaxis=2, cexnames=2 cexlab=2,cexmain=2,app=FALSE #for app
