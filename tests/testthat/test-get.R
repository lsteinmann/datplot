source(file = "../create_testing_df.R")


testdf <- create.testing.df()

testdfsteps <- scaleweight(datsteps(testdf))
teststeps_wrong <- testdfsteps
attributes(teststeps_wrong)$stepsize <- NULL
#str(teststeps_wrong)


test_that("multiplication works", {
  #expect_type(get.histogramscale(teststeps), c("numeric", "double", "integer"))
  expect_error(get.histogramscale(teststeps_wrong))
  expect_true(check.number(get.histogramscale(testdfsteps)))
})
