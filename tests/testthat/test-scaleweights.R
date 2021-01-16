source(file = "../create_testing_df.R")


testdf <- create.testing.df()

testdf_steps <- datsteps(testdf)


test_that("scaleweight appends attribute", {
  expect_equal(2 * 2, 4)
  expect_match(as.character(attributes(scaleweight(testdf_steps, var = "all")$weight)), regexp = "all objects")
  expect_match(as.character(attributes(scaleweight(testdf_steps, var = 2)$weight)), regexp = "by variable")
})


test_that("val accepts only numbers", {
  expect_error(scaleweight(testdf_steps, val = "no"))
  expect_error(scaleweight(testdf_steps, val = 8))
})

