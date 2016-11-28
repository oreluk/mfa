#' @export
print.mfa = function(x) {
cat("Summary of multifactor analysis object: This object contains information on the data inputs",
        " as well as on the analysis outputs. The input-related information includes the raw data ",
        "('data'), the number of active variables across all the tables ('sets'), the number of factors",
        " computed in the analysis ('ncomps'), and information about whether centering and scaling was ",
        "performed ('center' and 'scale'). The analysis output information includes the computed ",
        "eigenvalues ('eigenvalues'), common factor scores ('factorScores'), table weights ('alpha'),",
        " partial factor scores ('partialFactorScores'), variable loadings ('matrixLoadings'), and the",
        " processed concatenated data table ('X'). You can access the full contents of any of these ",
        "components using name_mfaObject$component_name. For example, if you are interested in viewing",
        " the eigenvalues for an mfa object called 'x', you would type 'x$eigenvalues'. \n ",
        "Below are the component names of your mfa object: \n", fill=100)
  print(names(x))
  cat('Number of tables considered in this multi-factor analysis:')
  print(length(x$partialFactorScores))
  cat('Number of active variables: ')
  print(sum(lengths(x$sets)))
  cat('Maximum Eigenvalue: ')
  print(max(x$eigenvalues))

  cat('Factor scores for first two components: \n')
  print(x$factorScores[,1:2])

  ## Partial Factor Scores
  par_fac_tables<-readline(paste0("If you would like to see partial factor scores for a single table,
  enter the corresponding table number (1 through ", length(x$partialFactorScores), "). To see partial
  factor scores for all tables, type ALL. To skip seeing any partial factor scores, type NONE."))

  if (par_fac_tables=="ALL"){
    cat('Partial factor scores for the first two components in all tables:  ')
    for (i in 1:length(x$partialFactorScores)){
      print(paste0("Table ",i,":"))
      print(x$partialFactorScores[[i]][,1:2])}
  }
  if (par_fac_tables=="NONE"){
    cat('No partial factor scores displayed.')
  }
  else {
    print(paste0("Partial factor scores for the first two components, showing table ", par_fac_tables, " of ", length(x$partialFactorScores), ": "))
    print(x$partialFactorScores[[as.numeric(par_fac_tables)]][,1:2])
  }


  ## Variable loadings
  table_loadings<-readline(paste0("If you would like to see variable loadings for a single table,
  enter the corresponding table number (1 through ", length(x$matrixLoadings), "). To see variable
  loadings for all tables, type ALL. To skip seeing any variable loadings, type NONE."))

  if (table_loadings=="ALL"){
    cat('Loadings for the first two components in all tables:  ')
    for (i in 1:length(x$matrixLoadings)){
      print(paste0("Table ",i,":"))
      print(x$matrixLoadings[[i]][,1:2])}
  }
  if (table_loadings=="NONE"){
    cat('No variable loadings displayed.')
  }
  else {
    print(paste0("Loadings for the first two components, showing table ", table_loadings, " of ", length(x$matrixLoadings), ": "))
    print(x$matrixLoadings[[as.numeric(table_loadings)]][,1:2])
  }
}
