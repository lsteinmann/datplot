source(file = "../create_testing_df.R")


testdf <- create.testing.df()

testdfsteps <- scaleweight(datsteps(testdf))
teststeps_wrong <- testdfsteps
attributes(teststeps_wrong)$stepsize <- NULL
#str(teststeps_wrong)



test_that("right errors are returned", {
  expect_error(get.histogramscale(teststeps_wrong, binwidth = "stepsize"), regexp = "dataframe as returned by datsteps")
  expect_error(get.histogramscale(teststeps_wrong), regexp = "dataframe as returned by datsteps")
  expect_error(get.histogramscale(testdfsteps, binwidth = "börek"), regexp = "or use")
  expect_error(get.histogramscale(20), regexp = "cannot be used with a number")
})


test_that("returns number", {
  expect_true(check.number(get.histogramscale(testdfsteps)))
  expect_true(check.number(get.histogramscale(20, binwidth = 5)))
})
