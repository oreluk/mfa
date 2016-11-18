# stephanie's testing that the package actually works --

# should just be able to use load_all() from devtools and the package will
# be loaded into your workspace!
devtools::load_all()
# run app:
MFA::runExample()

#if that fails, try doing each step individually using devtool_setup.R

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

X = a$partialFactorScores[[1]][,1]
Y = a$partialFactorScores[[1]][,2]

plot(X, Y,
     type = "p", pch=19, cex=sz,xlim=c(-1,1),ylim=c(-1,1))
text(X,Y,labels=1:length(X),col='red',xlim=c(-1,1),ylim=c(-1,1))
abline(v=0,h=0)
title('Partial Factor Scores for Table 1: first 2 components')
