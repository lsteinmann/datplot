source(file = "../create_testing_df.R")
testdf <- create.testing.df()

test_that("error for wrong value of stepsize", {
  expect_error(datsteps(testdf, stepsize = "test"))
})
