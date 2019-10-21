test_that("it returns an exact correlation", {
  expect_equal(cor(exact_correlation(100, .5))[1,2], .5)
})
