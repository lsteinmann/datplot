source(file = "../create_testing_df.R")
testdf <- create.testing.df()

test_that("error for wrong value of stepsize", {
  expect_error(suppressWarnings(datsteps(testdf, stepsize = "test")),
               "stepsize")
})

test_that("error for wrong value of stepsize", {
  expect_warning(datsteps(testdf, stepsize = 25), "recommended")
})


data("DAT_df")

test_that("error for wrong value of stepsize", {
  expect_warning(datsteps(DAT_df), regexp = "wrong order")
})

testdf <- create.testing.df()
testdf[, 3] <- sample(-200:0, nrow(testdf))
testdf[, 4] <- sample(1:200, nrow(testdf))
testdf[1, 3:4] <- c(4, 4)

test_that("error for wrong value of stepsize", {
  expect_warning(datsteps(testdf), regexp = "the same value")
  expect_warning(datsteps(testdf), regexp = "larger than the range of")
})


testdf <- create.testing.df()[1:2,]
testdf$variable <- as.factor(testdf$variable)
str(testdf)
testdf$min <- c(1, 10)
testdf$max <- c(2, 12)

test_steps <- datsteps(testdf, stepsize = 1)
test_cumul <- datsteps(testdf, stepsize = 1, cumulative = TRUE)

test_that("cumulative weight is added", {
  expect_equal(ncol(datsteps(testdf,
                             stepsize = 1,
                             cumulative = TRUE)),
               7)
  expect_equal(ncol(datsteps(testdf,
                             stepsize = 1,
                             cumulative = FALSE)),
               6)
})

test_that("cumulative weight is 1 for row 2 + 5", {
  expect_equal(datsteps(testdf, stepsize = 1,
                        cumulative = TRUE)[2,7],
               1)
  expect_equal(datsteps(testdf, stepsize = 1,
                        cumulative = TRUE)[5,7],
               1)
})

test_that("warning for cumulative weights with stepsize over 1", {
  expect_warning(datsteps(testdf, stepsize = 2,
                          cumulative = TRUE),
                 regexp = "cumulative")
})

test_that("warning for cumulative weights with stepsize over 1", {
testdf$variable <- as.factor(testdf$variable)
test_that("stepsize = 'auto' works", {
  expect_output(suppressWarnings(test <- datsteps(testdf, stepsize = "auto")),
                  "auto")
})
