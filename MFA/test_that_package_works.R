# stephanie's testing that the package actually works --

# should just be able to use load_all() from devtools and the package will
# be loaded into your workspace!
# RUN THIS LINE IF PLOTTING DIDNT WORK WHEN YOU RUN
# devtools::load_all()

#if that fails, try doing each step individually using devtool_setup.R

#now you should get documentation for things:
?mfa #should give html
?eigenvalueTable
?rv_table
?rv
?lg
?lg_table
?loadWineData
#?plot_compromise #doesn't work ...

#load the data
d <- loadWineData()
keys <- loadWineInfo()
s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
           seq(25,30), seq(31,35), seq(36,39), seq(40,45),
           seq(46,50), seq(51,54) )

a = mfa(d, s)

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
plot_compromise(a)
plot_compromise(a,dim1=3,dim2=1)
plot_partial_fac(a,table=1)
plot_partial_fac(a,table=1,dim1=3,dim2=4)
plot_loading(a,table=1)
plot_loading(a,table=1,varnames=keys)
plot_loading(a,table=1,dim1=3,dim2=4,varnames=keys)

# test run app:
MFA::runExample()
