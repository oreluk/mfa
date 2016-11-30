# stephanie's testing that the package actually works --

# should just be able to use load_all() from devtools and the package will
# be loaded into your workspace (neglects unit tests, etc.)
devtools::load_all()

# WINE DATA
#load the wine data and information on var names
d <- loadWineData()
s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
           seq(25,30), seq(31,35), seq(36,39), seq(40,45),
           seq(46,50), seq(51,54) )
a = mfa(d, s)

v = vardim(a)
for (seti in a$sets){ print(length(seti))}



# TEST APP (wine data)
mfa_obj <- a #name needed for app
active_var_names <- loadWineInfo()$varkeys
active_obs_names <- loadWineInfo()$obskeys
active_col_vec <- loadWineInfo()$colors
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

# the way the app can use different data sets right now is
# by the names mfa_obj and active_var_names
# make sure you set these before running...
# also active_obs_names and active_col_vec should be set to null if you
# dont have obs labels or colors youd like your obs to be labeled with!




#test print method
print(a)

#test eigenvalue
eigenvalueTable(a)

# test plotting methods
#first get obsnames and varnames:
obskeys<-loadWineInfo()$obskeys
varkeys<-loadWineInfo()$varkeys
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
