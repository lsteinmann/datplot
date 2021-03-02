

data(DAT_df)

test_that("works for single number", {
  expect_equal(get.histogramscale(2, binwidth = 2), 4)
})

test_that("works for df", {
  expect_equal(get.histogramscale(DAT_df, binwidth = 2), 10000)
})


test_that("works for vector", {
  expect_equal(get.histogramscale(c(2,3,4,5), binwidth = 2), 8)
})
