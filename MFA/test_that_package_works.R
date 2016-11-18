# stephanie's testing that the package actually works --

# first run each line in devtool_setup.R to install and load the package "MFA"

#now you should get documentation for things:
?mfa #should give html
?eigenvalueTable
?rv_table
?rv
?lg
?lg_table
?loadWineData

#load the data
d <- loadWineData()

s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
           seq(25,30), seq(31,35), seq(36,39), seq(40,45),
           seq(46,50), seq(51,54) )

a = mfa(d, s)
print(a)

plot(a)

eigenvalueTable(a)
