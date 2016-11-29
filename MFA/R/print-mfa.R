#' @export
print.mfa = function(x) {
  cat("Summary of Multiple Factor Analysis (mfa) object: \n",
      "This object contains information on the data inputs as well as on the analysis outputs.",
      "You can access the full contents of any of these components using name_mfaObject$component_name. ",
      "For example, if you are interested in viewing the eigenvalues for an mfa object called 'x', ",
      "you would type 'x$eigenvalues'. \n",
      "\nYour MFA object contains the following items : ", fill=100)
  descriptions <- c('raw data input by user',
                    'number of active variables across all the tables',
                    'number of factors computed in the analysis',
                    'whether centering was performed on the data',
                    'whether scaling was performed on the data',
                    'eigenvalues',
                    'common factor scores',
                    'table weights',
                    'partial factor scores',
                    'variable loadings (list form)',
                    'variable loadings (matrix form)',
                    'the processed concatenated data table')
  cat('################################################################################\n')
  cat(sprintf('%25s','Component Name') , '   ::   ', 'Description\n')
  cat('################################################################################\n')
  for (n in 1:length(names(x))){
    cat(sprintf('%25s',names(x)[n]), '   ::   ', descriptions[n],'\n')
  }
  cat('################################################################################\n')
  cat('\nNumber of tables considered in this multiple factor analysis: ')
  cat(length(x$partialFactorScores))
  cat('\nNumber of active variables: ')
  cat(sum(lengths(x$sets)))
  cat('\nMaximum Eigenvalue: ')
  cat(max(x$eigenvalues))

  cat('\nCommon factor scores for first two components: \n')
  #make a pretty table for the common factor scores, using cat
  cat(sprintf('%13s','Dim1'),sprintf("%7s",'Dim2'))
  cat('\n')
  table_of_interest <- x$factorScores[,1:2]
  nrows <- length(table_of_interest[,1])
  for (s in 1:nrows) {
    cat(sprintf('%5s',s), sprintf(': %+0.2f', table_of_interest[s,1]), sprintf(': %+0.2f',table_of_interest[s,2]), "\n"
    )}
  #print(x$factorScores[,1:2])

  ## Partial factor scores
  cat('\n################################################################################\n')
  cat('\nPartial Factor Scores:\n')
  cat('################################################################################\n')
  cat('To see partial factor scores for a single table, ',
      "enter the corresponding table number (1 through ", length(x$partialFactorScores), ")")
  cat("\nTo see partial factor scores for all tables, type ALL. (case-sensitive)")
  cat("\nTo skip seeing any partial factor scores, type NONE (case-sensitive) or Return")
  par_fac_tables<-readline("Which table would you like to display ? ")

  if (par_fac_tables=="ALL"){
    cat('Partial factor scores for the first two components in all tables:  ')
    for (i in 1:length(x$partialFactorScores)){
      print(paste0("Table ",i,":"))
      print(x$partialFactorScores[[i]][,1:2])}
  }
  else if (par_fac_tables=="NONE" | par_fac_tables=='' | is.na(as.integer(par_fac_tables))) {
    cat('No partial factor scores displayed.')
  }
  else if (as.integer(par_fac_tables) < 1 | as.integer(par_fac_tables) > length(x$partialFactorScores)) {
    cat('No partial factor scores displayed, Index out of range.')
  }
  else {
    #check that par_fac_tables is valid entry

    cat("Partial factor scores for the first two components: ")
    cat("\nTable ", as.integer(par_fac_tables)," of ", length(x$loadingByTable))
    cat('\n')
    cat(sprintf('%13s','Dim1'),sprintf("%7s",'Dim2'))
    cat('\n')
    table_of_interest <- x$partialFactorScores[[as.integer(par_fac_tables)]][,1:2]
    nrows <- length(table_of_interest[,1])
    for (s in 1:nrows) {
      cat(sprintf('%5s',s), sprintf(': %+0.2f', table_of_interest[s,1]), sprintf(': %+0.2f',table_of_interest[s,2]), "\n"
      )}
    }


  ## Variable loadings
  cat('\n################################################################################\n')
  cat('\nVariable loadings:\n')
  cat('################################################################################\n')
  cat('To see variable loadings for a single table, ',
      "enter the corresponding table number (1 through ", length(x$loadingByTable), ")")
  cat("\nTo see variable loadings for all tables, type ALL. (case-sensitive)")
  cat("\nTo skip seeing any variable loadings, type NONE (case-sensitive) or Return")
  table_loadings<-readline("Which table would you like to display ? ")

  if (table_loadings=="ALL"){
    cat('Loadings for the first two components in all tables:  ')
    for (i in 1:length(x$loadingByTable)){
      print(paste0("Table ",i,":"))
      print(x$loadingByTable[[i]][,1:2])}
  }
  else if (table_loadings=="NONE" | table_loadings==''| is.na(as.integer(table_loadings))){
    cat('No variable loadings displayed.')
  }
  else if (as.integer(table_loadings) < 1 | as.integer(table_loadings) > length(x$partialFactorScores)) {
    cat('No variable loadings displayed. Index out of range.')
  }
  else {
    cat(paste0("Loadings for the first two components:"))
    cat("\nTable ", as.integer(table_loadings)," of ", length(x$loadingByTable))
    cat('\n')
    cat(sprintf('%13s','Dim1'),sprintf("%7s",'Dim2'))
    cat('\n')
    table_of_interest <- x$loadingByTable[[as.integer(table_loadings)]][,1:2]
    rowlabels <- rownames(table_of_interest)
    for (s in 1:length(rowlabels)) {
      cat(sprintf('%5s',rowlabels[s]), sprintf(': %+0.2f', table_of_interest[s,1]), sprintf(': %+0.2f',table_of_interest[s,2]), "\n"
          )
    }

  }
}
