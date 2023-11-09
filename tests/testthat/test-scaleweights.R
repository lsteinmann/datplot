source(file = "../create_testing_df.R")


testdf <- create.testing.df()

testdf_steps <- suppressWarnings(datsteps(testdf, calc = "weight"))

test_that("scaleweight appends attribute", {
  expect_match(as.character(attributes(scaleweight(testdf_steps,
                                                   var = "all")$weight)),
               regexp = "all objects")
  expect_match(as.character(attributes(scaleweight(testdf_steps,
                                                   var = 2)$weight)),
               regexp = "grouped")
})


test_that("val does not accept non-existing column (chr)", {
  expect_error(scaleweight(testdf_steps, var = "all", val = "no"),
               regexp = "number")
})

test_that("val does not accept non-existing column (numeric)", {
  expect_error(scaleweight(testdf_steps, var = "all", val = 10),
               regexp = "index")
})

test_that("val manages to find correct column when using chr", {
  bla <- scaleweight(testdf_steps, var = "all", val = "weight")
  expect_match(as.character(attributes(bla$weight)),
               regexp = "scaled")
})

test_that("var accepts only numbers or 'all' ", {
  expect_error(scaleweight(testdf_steps, var = "blobb"),
               regexp = "all")
})

test_that("var manages to find correct column when using chr", {
  bla <- scaleweight(testdf_steps, var = "variable")
  expect_match(as.character(attributes(bla$weight)),
               regexp = "scaled")
})


