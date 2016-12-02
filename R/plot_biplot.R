#' @export
plot_biplot = function(obj, table = 1, dim1 = 1, dim2 = 2, sz = 1, varnames = NULL,
                       cexmain = 1, cexlab = 1, cexaxis = 0.8, app = FALSE,
                       textcolor = 'black', obsnames = NULL,
                       dotcol_vl = 'white', dotcol_pf = 'white',
                       cexlegend = 0.8) UseMethod("plot_biplot", obj)
plot_biplot.mfa <- function(x, table = 1, dim1 = 1, dim2 = 2, sz = 1, varnames = NULL,
                            cexmain = 1, cexlab = 1, cexaxis = 0.8, app = FALSE,
                            textcolor = 'black', obsnames = NULL,
                            dotcol_vl = 'white', dotcol_pf = 'white',
                            cexlegend = 0.8) {
  #' @param  x An object of class mfa
  #' @param  dim1 dimension for x axis, default 1st component
  #' @param  dim2 dimension for y axis, default 2nd component
  #' @param  table which table to plot (integer 1:K)
  #' @param  sz size of text label in plot
  #' @param  varnames variable labels if available
  #' @param  obsnames observation labels if available
  #' @param  cexmain size for main title label, default 1, 2 used for app
  #' @param  cexlab size for axis labels, default 1, 1.7 used for app
  #' @param  cexaxis size for axis tick labels, default 0.8
  #' @param  cexlegend size for legend
  #' @param  dotcol_vl choose color for the variable loading dots, default white (don't show up)
  #' @param  dotcol_pf choose color for the dots partial factor dots, default white (don't show up)
  #' @param  app if TRUE, this is for the shiny app and margins are changed
  #' @param  textcolor color of text labeling points, either a single color or a vector of colors equal to number of obs in mfa object
  #' @export
  #' @title Plot Biplot
  #' @name plot biplots
  #' @description plots variable loadings and partial factor scores on same plot, for 2 given dimensions of a given table in an mfa object

  #select the table and dimension of interest, and store in X (dim1) and Y (dim2)
  X = x$loadingByTable[[table]][,dim1]
  Y = x$loadingByTable[[table]][,dim2]

  # select the table and dimension of interest, and store in X (dim1) and Y (dim2)
  X2 = x$partialFactorScores[[table]][,dim1] #fac score first table, 1st comp
  Y2 = x$partialFactorScores[[table]][,dim2] #fac score first table, 2nd comp

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

  # get the observation labels (for partial factor scores)
  # get the observation names if available:
  if (!is.null(obsnames)){
    obslabels <- obsnames
  } else {
    obslabels <- 1:length(X)
  }

  # widen left margin to accommodate the large font of axis label
  if (app == TRUE) {par(mar=c(5.1,8.1,4.1,2.1))}

  #set max and min based on which of X,X2 and Y,Y2 are larger
  if (max(X) > max(X2)) { maxX <- max(X)} else {maxX <- max(X2)}
  if (max(Y) > max(Y2)) { maxY <- max(Y)} else {maxY <- max(Y2)}
  if (min(X) < min(X2)) { minX <- min(X)} else {minX <- min(X2)}
  if (min(Y) < min(Y2)) { minY <- min(Y)} else {minY <- min(Y2)}

  # plot the data (white points are not visible, but text is added with text())
  plot(X, Y,
       type = "p", pch=19, col=dotcol_vl,
       xlab = paste0('Dimension ', dim1),
       ylab = paste0('Dimension ', dim2),
       xlim = c(minX-0.4,maxX+0.4),
       ylim = c(minY-0.4,maxY+0.4),
       cex = 1,
       cex.axis = cexaxis,
       cex.lab = cexlab)
  text(X, Y, labels = varlabels, col = darkcols, cex = sz)
  points(X2, Y2, col = dotcol_pf)
  text(X2, Y2, labels = obslabels, col = textcolor, cex = sz)
  abline(v = 0, h = 0)
  title(paste0('Variable Loadings and Partial Factor Scores \n for Table ', table ), cex.main = cexmain)
  legend("topleft",
         legend = c('variable loadings (colors)','partial factor scores (black)'),
         lty = c(1,1),
         lwd = c(2.5,2.5),
         col = c(darkcols[1],textcolor[1]),
         cex = cexlegend,
         bty = 'n')
}
