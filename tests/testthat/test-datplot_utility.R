source(file = "../create_testing_df.R")

testmat <- matrix(c(1, 2, 3, 4, 0, 0, 50, -200, 5, -200, 50, 5),
                  byrow = FALSE, ncol = 3)
colnames(testmat) <- c("index", "datmin", "datmax")
DAT_err <- which(testmat[, 2] > testmat[, 3])


test_that("generate.stepsize returns one", {
  expect_equal(generate.stepsize(testmat), 1)
})

corrmat <- as.data.frame(matrix(c(1, 2, 3, 4, 6, 6, 8, 8, 0, -200, 50,
                                  -200, 5, 0, 50, 5),
                                byrow = FALSE, ncol = 4))
testmat <- as.data.frame(matrix(c(1, 2, 3, 4, 6, 6, 8, 8, 0, 0, 50, -200,
                                  5, -200, 50, 5),
                                byrow = FALSE, ncol = 4))
test_that("switch.dating returns values correctly", {
  expect_equal(switch.dating(as.data.frame(testmat), DAT_err), corrmat)
})

testdf <- create.testing.df()

fristlast <- matrix(nrow = nrow(testdf), ncol = 2)
for (r in 1:nrow(testdf)) {
  seq <- get.step.sequence(datmin = testdf[r, 3],
                    datmax = testdf[r, 4],
                    stepsize = 25)
  fristlast[r, 1] <- seq[1]
  fristlast[r, 2] <- seq[length(seq)]
}

test_that("sequence returns first and last values", {
  expect_equal(fristlast[, 1], testdf[, 3])
  expect_equal(fristlast[, 2], testdf[, 4])
})






testdf_wrong <- testdf
testdf_wrong$min <- "TEST"
testdf_wrong_two <- testdf
testdf_wrong_two$max <- factor("börek")
testdf_wrong_three <- matrix(nrow = 4, ncol = 4)


test_that("check.number returns true for numbers false for other", {
  expect_true(check.number(numeric(1)))
  expect_true(check.number(double(1)))
  expect_true(check.number(integer(1)))
  expect_false(check.number(character(1)))
  expect_false(check.number(factor(1)))
  expect_false(check.number(testdf_wrong[3, 3]))
  expect_true(check.number(testdf_wrong[3, 4]))
  expect_false(check.number(testdf_wrong_two[3, 4]))
  expect_false(check.number(testdf))
})




test_that("check.structure works", {
  expect_true(suppressWarnings(check.structure(testdf)))
  expect_error(check.structure(testdf_wrong))
  expect_error(check.structure(testdf_wrong_two))
  expect_error(check.structure(testdf_wrong_three))
})

test_that("check.structure issues warning", {
  expect_warning(check.structure(testdf), regexp = "recommended")
})


testdf <- create.testing.df()
testdf[, 3] <- sample(-200:0, nrow(testdf))
testdf[, 4] <- sample(1:200, nrow(testdf))
testdf[1, 3:4] <- c(4, 4)

DAT_mat <- matrix(ncol = 5, nrow = nrow(testdf))
DAT_mat[, 1] <- 1:nrow(testdf)
DAT_mat[, 2] <- testdf[, 3]
DAT_mat[, 3] <- testdf[, 4]
colnames(DAT_mat) <- c("index", "datmin", "datmax", "weight", "step")
DAT_list <- as.data.frame(DAT_mat)
DAT_list <- unlist(apply(DAT_list, 1, list), recursive = FALSE)


test_that("warning or not for stepsize larger than steps in data in create.sub.objects", {
  expect_failure(expect_warning(create.sub.objects(DAT_list, stepsize = 1),
                                regexp = "stepsize is larger"))
  expect_warning(create.sub.objects(DAT_list, stepsize = 5),
                 regexp = "stepsize is larger")
})



test_that("create.sub.objects adds cumulative probability", {
  mat <- create.sub.objects(DAT_list, stepsize = 1,
                            cumulative = TRUE)
  expect_equal(ncol(mat),
               6)
  mat <- create.sub.objects(DAT_list, stepsize = 1,
                            cumulative = FALSE)
  expect_equal(ncol(mat),
               5)
})

