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

testdf <- create.testing.df()
testdf[,3] <- sample(-200:0, nrow(testdf))
testdf[,4] <- sample(1:200, nrow(testdf))
testdf[1,3:4] <- c(4,4)

test_that("error for wrong value of stepsize", {
  expect_warning(datsteps(testdf), regexp = "the same value")
  expect_failure(expect_warning(datsteps(testdf, stepsize = 1),
                                regexp = "larger than the range of the closest"))
})
