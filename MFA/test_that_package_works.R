# stephanie's testing that the package actually works --

# should just be able to use load_all() from devtools and the package will
# be loaded into your workspace (neglects unit tests, etc.)
# devtools::load_all()

# WINE DATA
#load the wine data and information on var names
d <- loadWineData()
keys <- loadWineInfo()
s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
           seq(25,30), seq(31,35), seq(36,39), seq(40,45),
           seq(46,50), seq(51,54) )
a = mfa(d, s)

# RANDOM DATA
# create a data set of random integers ranging from 50 to 100
nrows=14 #number of observations (like the wines)
nvars=33 #number of vars
d2 <- replicate(nvars,sample(50:100,size=nrows,rep=TRUE))
# sets -- indexing into d2, so should cover range 1:nvars
s2 = list(  seq(1,4), seq(5,10), seq(11,19), seq(20,22),
           seq(23,30), seq(31,33))
a2 = mfa(d2, s2)

# NOW RUN APP WITH EITHER DATASET
# the way the app can use different data sets right now is
# by the names mfa_obj and active_var_names
# make sure you set these before running...
# test run app: WINE DATA
mfa_obj <- a #name needed for app
active_var_names <- loadWineInfo()
MFA::runExample()
# test run app with RANDOM DATA
mfa_obj <- a2 #name needed for app
active_var_names <- NULL
MFA::runExample()


#test print method
print(a)

#test eigenvalue
eigenvalueTable(a)

# test plotting methods
plot_compromise(a)
plot_compromise(a,dim1=3,dim2=1)
plot_partial_fac(a,table=1)
plot_partial_fac(a,table=1,dim1=3,dim2=4)
plot_loading(a,table=1)
plot_loading(a,table=1,varnames=keys)
plot_loading(a,table=1,dim1=3,dim2=4,varnames=keys)

#shouldnt work:
plot_compromise(a,dim1=3,dim2=100)
plot_partial_fac(a,table=100)

# does the documentation exist?
#now you should get documentation for things:
?mfa #should give html
?eigenvalueTable
?rv_table
?rv
?lg
?lg_table
?loadWineData
