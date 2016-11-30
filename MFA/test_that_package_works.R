# stephanie's testing that the package actually works --

path_to_file="/Users/stephanie/classes/STAT243/MFA/MFA_0.1.0.tar.gz"
install.packages(path_to_file, repos = NULL, type="source")
library(MFA)

# WINE DATA
#load the wine data and define indices for the sub-tables within the data table
d <- loadWineData() #wine data
# sets, list of vectors indexing into d
s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
           seq(25,30), seq(31,35), seq(36,39), seq(40,45),
           seq(46,50), seq(51,54) )
# use mfa constructor on d,s
a = mfa(d, s)

# APP TESTING
# The app will use whatever mfa object in your workspace that is called
# mfa_obj. Make sure this is set to the object you want to use in the app!
# If you have variable names associated with your dataset, make sure
# to store those in active_var_names
# If you have observation names you'd like to use with your dataset,
# make sure to store those in active_obs_names.
# If you have a color or vector of colors you'd like to use for the variables,
# make sure to store that as active_col_vec
# If you don't have observation names, variable names, or colors for variables,
# make sure these are set to NULL.

# TEST APP (wine data)
mfa_obj <- a #name needed for app
active_var_names <- loadWineInfo()$varkeys #name needed for app
active_obs_names <- loadWineInfo()$obskeys #name needed for app
active_col_vec <- loadWineInfo()$colors #name needed for app
MFA::runExample()

# RANDOM DATA
# create a data set of random integers ranging from 50 to 100
nrows=14 #number of observations (like the wines)
nvars=33 #number of vars
d2 <- replicate(nvars,sample(50:100,size=nrows,rep=TRUE))
# sets -- indexing into d2, so should cover range 1:nvars
s2 = list(  seq(1,4), seq(5,10), seq(11,19), seq(20,22),
           seq(23,30), seq(31,33))
a2 = mfa(d2, s2)
# test run app with RANDOM DATA
mfa_obj <- a2 #name needed for app
active_var_names <- NULL
active_obs_names <- NULL
active_col_vec <- NULL
MFA::runExample()






#test print method
print(a)

#test eigenvalue
eigenvalueTable(a)

# test plotting methods
#first get obsnames and varnames:
obskeys<-loadWineInfo()$obskeys
varkeys<-loadWineInfo()$varkeys
obslabels<-obskeys
varlabels<-varkeys
#
plot_compromise(a)
plot_compromise(a,dim1=3,dim2=1)
plot_partial_fac(a,table=1)
plot_partial_fac(a,table=1,dim1=3,dim2=4)
plot_partial_fac(a,table=1,dim1=1,dim2=2,obsnames=obskeys)
plot_loading(a,table=1)
plot_loading(a,table=1,varnames=varkeys)
plot_loading(a,table=1,dim1=3,dim2=4,varnames=varkeys)
#bar graph
plot_ev(a)
plot_inertia_pie(a)

#biplot...
plot_biplot(a, obsnames=obskeys, varnames = varkeys, textcolor = loadWineInfo()$colors)
plot_biplot(a, obsnames=obskeys, varnames = varkeys)
plot_biplot(a, obsnames=obskeys, varnames = varkeys, table=2)
plot_biplot(a, obsnames=obskeys, varnames = varkeys, table=3)

par(mfrow=c(1,2))
plot_biplot(a, obsnames = obslabels, varnames = varlabels, table=2,
            cexmain=0.9, cexaxis=0.6, cexlab=0.8, sz=0.8 )
plot_biplot(a, obsnames = obslabels, varnames = varlabels, table=4,
            cexmain=0.9, cexaxis=0.6, cexlab=0.8, sz=0.8 )


# does the documentation exist?
#now you should get documentation for things:
?mfa #should give html
?eigenvalueTable.mfa
?rv_table
?rv
?lg
?lg_table
?loadWineData
