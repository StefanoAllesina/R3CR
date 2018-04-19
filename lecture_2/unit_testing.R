context("These are all the tests we've written")

test_that("a meaningful message",{
  x <- 0.4
  expect_equal(sin(x), cos(pi / 2 - x))
  expect_equal(sin(x), sqrt(1 - cos(x)^2))
})

test_that("another meaningful message",{
   expect_equal(nchar("abcd"), 4)
   expect_equal(2^4, 16)
})

test_that("a test that will fail",{
  expect_equal(length("abcd"), 4)
})