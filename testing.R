# testing


## read csv
d = read.csv('C:/Users/Jim/Dropbox/classes/stat243a/ps/finalProject/wines.csv', header=TRUE, check.names=FALSE)
class(data)

s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24), 
           seq(25,30), seq(31,35), seq(36,39), seq(40,45), 
           seq(46,50), seq(51,54) )

a = mfa(d, s)
a
eigenvalueTable(a)




# Random Data - not working yet
A = runif(100)
A = matrix(A, nrow = 10, byrow = TRUE)
class(A)

s = c(1,2,3)
s = cbind(s, c(3,3,4))
s = list(s)
obj = mfa(A, s)
