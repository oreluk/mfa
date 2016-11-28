#' @export
print.mfa = function(x) {
  print(paste0("Summary information for your mfa object:"))
  cat('Maximum Eigenvalue: ')
  print(max(x$eigenvalues))
  cat('Number of active variables: ')
  print(sum(lengths(x$sets)))
  cat('Factor scores for first two components: ')
  print(x$factorScores[,1:2])

  ## Partial Factor Scores
  par_fac_tables<-readline(paste0("If you would like to see partial factor scores for a single table,
  enter the corresponding table number (1 through ", length(x$partialFactorScores), "). To see partial
  factor scores for all tables, type ALL. To skip seeing any partial factor scores, type NONE."))

  if (par_fac_tables=="ALL"){
    cat('Partial factor scores for the first two components in all tables:  ')
    for (i in 1:length(x$matrixLoadings)){
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
