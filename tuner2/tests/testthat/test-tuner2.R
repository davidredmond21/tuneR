use_package("testthat")
context("simplest Test")
test_that("Mah function....  )", {
  x=matrix(data=runif(16,3,6), nrow=4, ncol=1 )
  expect_length(x, 4)
})


