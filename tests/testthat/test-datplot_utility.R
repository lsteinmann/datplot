source(file = "../create_testing_df.R")
library(datplot)
library(devtools)
library(testthat)

testmat <- matrix(c(1,2,3,4,0,0,50,-200,5,-200,50,5), byrow = FALSE, ncol = 3)
colnames(testmat) <- c("index", "datmin", "datmax")
DAT_err <- which(testmat[,2] > testmat[,3])
DAT_err


test_that("generate.stepsize returns one", {
  expect_equal(generate.stepsize(testmat), 1)
})

corrmat <- as.data.frame(matrix(c(1,2,3,4,6,6,8,8,0,-200,50,-200,5,0,50,5), byrow = FALSE, ncol = 4))
testmat <- as.data.frame(matrix(c(1,2,3,4,6,6,8,8,0,0,50,-200,5,-200,50,5), byrow = FALSE, ncol = 4))
test_that("switch.dating returns values correctly", {
  expect_equal(switch.dating(as.data.frame(testmat), DAT_err), corrmat)
})

testdf <- create.testing.df()

fristlast <- matrix(nrow = nrow(testdf), ncol = 2)
for (r in 1:nrow(testdf)) {
  seq <- get.step.sequence(datmin = testdf[r,3],
                    datmax = testdf[r,4],
                    stepsize = 25)
  fristlast[r,1] <- seq[1]
  fristlast[r,2] <- seq[length(seq)]
}

test_that("sequence returns first and last values", {
  expect_equal(fristlast[,1], testdf[,3])
  expect_equal(fristlast[,2], testdf[,4])
})


test_that("check.number returns true for numbers false for other", {
  expect_true(check.number(numeric(1)))
  expect_true(check.number(double(1)))
  expect_true(check.number(integer(1)))
  expect_false(check.number(character(1)))
  expect_false(check.number(factor(1)))
})




