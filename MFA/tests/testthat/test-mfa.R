# testing

## read csv
# get the raw data from the package itself:
filename = system.file("extdata", "wines.csv", package = "MFA")
#d = read.csv(filename, header=TRUE, check.names=FALSE)
#d = read.csv("/Users/stephanie/classes/STAT243/MFA/data/wines.csv",header=TRUE, check.names=FALSE)
d <- loadWineData()
class(d)

s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
           seq(25,30), seq(31,35), seq(36,39), seq(40,45),
           seq(46,50), seq(51,54) )

a = mfa(d, s)
a
eigenvalueTable(a)


b = mfa(d,s, ncomps = 2)
eigenvalueTable(b)
