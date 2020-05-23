library(datplot)
library(devtools)

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
