# Testing

# get the raw data from the package itself:
filename = system.file("extdata", "wines.csv", package = "MFA")
d <- loadWineData(checknames=TRUE)

s = list(  seq(2,7), seq(8,13), seq(14,19), seq(20,24),
            seq(25,30), seq(31,35), seq(36,39), seq(40,45),
            seq(46,50), seq(51,54) )

s2 = list(c('V1','V2','V3','V4','V5','V6'), c('V1.1','V2.1','V3.1','V4.1','V7','V8'),
          c('V1.2','V2.2','V3.2','V4.2','V9','V10'), c('V1.3','V2.3','V3.3','V4.3','V8.1'),
          c('V1.4','V2.4','V3.4','V4.4','V11','V12'), c('V1.5','V2.5','V3.5','V4.5','V13'),
          c('V1.6','V2.6','V3.6','V4.6'), c('V1.7','V2.7','V3.7','V4.7','V14','V5.1'),
          c('V1.8','V2.8','V3.8','V4.8','V15'), c('V1.9','V2.9','V3.9','V4.9'))


test_that("invalid inputs of mfa throw errors", {
  expect_error(mfa())
  expect_error(mfa(d))
  expect_error(mfa(s))
  expect_error(mfa(list(d), s))
  expect_error(mfa(d,s, ncomps =2.5))
  expect_error(mfa(d,s, ncomps = 100))
  expect_error(mfa(d,s, ncomps = 0))
  expect_error(mfa(d,s, scale = 0))
  expect_error(mfa(d,s, scale = 'FALSE'))
  expect_error(mfa(d,s, center = 0))
  expect_error(mfa(d,s, center = 12))
  expect_error(mfa(d,s, center = seq(1:52)))
})

test_that("valid inputs do not return warnings or errors", {
  expect_warning(mfa(d,s, center = seq(1:53)), NA)
  expect_error(mfa(d,s, center = seq(1:53)), NA)
  expect_warning(mfa(d,s, scale = seq(1:53)), NA)
  expect_error(mfa(d,s, scale = seq(1:53)), NA)
  expect_warning(mfa(d,s, center = seq(1:53), scale = rep(1,53) ), NA)
  expect_error(mfa(d,s, center = seq(1:53), scale = rep(1,53)), NA)
  expect_warning(mfa(d,s, center = seq(1:53), scale = rep(1,53), ncomps = 3), NA)
  expect_error(mfa(d,s, center = seq(1:53), scale = rep(1,53), ncomps = 3), NA)
  expect_warning(mfa(d,s2, center = seq(1:53), scale = rep(1,53), ncomps = 3), NA)
  expect_error(mfa(d,s2, center = seq(1:53), scale = rep(1,53), ncomps = 3), NA)
})

# using mfa objects for following tests
a = mfa(d,s)
b = mfa(d,s, ncomps=2)
a2 = mfa(d,s2)

test_that("the correct number of elements from eigenvalues and partial factor scores", {
  expect_equal(class(a), 'mfa')
  expect_equal(length(a$eigenvalues), 12)
  expect_equal(length(a$partialFactorScores), length(s))
  expect_equal(ncol(b$factorScores), 2)
  expect_equal(ncol(b$matrixLoadings[[1]]), 2)
})

test_that("results from list of characters or list of vectors is same", {
  expect_equal(a2$eigenvalues, a$eigenvalues)
  expect_equal(a2$factorScores, a$factorScores)
  expect_equal(a2$matrixLoadings, a$matrixLoadings)
  expect_equal(a2$alpha, a$alpha)
})

keys = loadWineInfo()
test_that("plot functions are working", {
  # Invalid Inputs
  expect_error(plot_loading(a,table=13))
  expect_error(plot_compromise(a,dim1=3,dim2=100))
  expect_error(plot_partial_fac(a,table=100))
  # Valid Inputs
  expect_warning(plot_compromise(a), NA)
  expect_error(plot_compromise(a), NA)
  expect_warning(plot_compromise(a,dim1=3,dim2=1), NA)
  expect_error(plot_compromise(a,dim1=3,dim2=1), NA)
  expect_warning(plot_partial_fac(a,table=1), NA)
  expect_error(plot_partial_fac(a,table=1), NA)
  expect_warning(plot_partial_fac(a,table=1,dim1=3,dim2=4), NA)
  expect_error(plot_partial_fac(a,table=1,dim1=3,dim2=4), NA)
  expect_warning(plot_loading(a,table=1), NA)
  expect_error(plot_loading(a,table=1), NA)
  expect_warning(plot_loading(a,table=1,varnames=keys), NA)
  expect_error(plot_loading(a,table=1,varnames=keys), NA)
  expect_warning(plot_loading(a,table=1,dim1=3,dim2=4,varnames=keys), NA)
  expect_error(plot_loading(a,table=1,dim1=3,dim2=4,varnames=keys), NA)
})



