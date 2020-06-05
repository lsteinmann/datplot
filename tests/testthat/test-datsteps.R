source(file = "../create_testing_df.R")
testdf <- create.testing.df()

test_that("error for wrong value of stepsize", {
  expect_error(datsteps(testdf, stepsize = "test"), "stepsize has to be")
  expect_warning(datsteps(testdf, stepsize = 25))
})

data("DAT_df")

str(DAT_df)

test_that("error for wrong value of stepsize", {
  expect_warning(datsteps(DAT_df), regexp = "wrong order")

})
