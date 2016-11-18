# For setting up the package with devtools:

library(devtools)

# creating documentation (i.e. the Rd files in man/)
devtools::document()

# checking documentation
devtools::check_man()

# running tests
devtools::test()

# building tarball (e.g. oski_0.1.tar.gz)
devtools::build()

# checking install
devtools::install()

library("MFA")
