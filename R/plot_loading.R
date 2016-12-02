#' @export
plot_loading = function(obj, table = 1, dim1 = 1, dim2 = 2, sz = 1, varnames = NULL,
                        cexmain = 1, cexlab = 1, cexaxis = 0.8, app=FALSE,
                        dotcol = 'white') UseMethod("plot_loading", obj)
plot_loading.mfa <- function(x, table = 1, dim1 = 1, dim2 = 2, sz = 1, varnames=NULL,
                             cexmain = 1, cexlab = 1, cexaxis = 0.8, app = FALSE,
                             dotcol = 'white') {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  table which table to plot (integer 1:numtables)
  #' @param  sz size of text label in plot
  #' @param  varnames variable labels if available
  #' @param  cexmain size for main title label, default 1, 2 used for app
  #' @param  cexlab size for axis labels, default 1, 1.7 used for app
  #' @param  cexaxis size for axis tick labels, default 0.8
  #' @param  dotcol choose color for the dots, default white (don't show up)
  #' @param  app if TRUE, this is for the shiny app and margins are changed
  #' @export
  #' @title Plot Loadings
  #' @name plot variable loadings
  #' @description plots variable loadings for 2 given dimensions of a given table in an mfa object

  #select the table and dimension of interest, and store in X (dim1) and Y (dim2)
  X = x$loadingByTable[[table]][,dim1]
  Y = x$loadingByTable[[table]][,dim2]

  # define labels for points on the plot:
  # if varnames are provided, use them as labels on the plot
  # if they are not provided, try to use the names of the items in X
  # if names(X) is null, set labels as integers ranging from 1 to length(X)
  if (!is.null(varnames)){
    varlabels <- names(X)
    ind = 1
    for (n in names(X)){
      varlabels[ind] <- varnames[n]
      ind <- ind + 1
    }
  } else {
    varlabels <- names(X)
  }

  # sometimes (e.g. random data matrix), the names(X) will be null
  # in this case, set varlabels to 1:length(X)
  if (is.null(names(X))){ varlabels <- 1:length(X)}

  # define a nice color palette for the variables, dark colors
  darkcols <- c("#1B9E77","#D95F02","#7570B3","#E7298A",
                "#66A61E","#E6AB02","#A6761D","#666666")


  # widen left margin to accommodate the large font of axis label
  if (app == TRUE) {par(mar = c(5.1,8.1,4.1,2.1))}

  # plot the data (white points are not visible, but text is added with text())
  plot(X, Y,
       type = "p", pch=19, col=dotcol,
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2),
       xlim = c(min(X)-0.4,max(X)+0.4),
       ylim = c(min(Y)-0.4,max(Y)+0.4),
       cex = 1,
       cex.axis = cexaxis,
       cex.lab = cexlab)
  text(X, Y, labels = varlabels, col = darkcols, cex = sz)
  abline(v = 0, h = 0)
  title(paste0('Variable Loadings for Table ', table ),cex.main=cexmain)
}
