test_that("returns correct matrix", {
  expected <- array(c(10, 10, 10, 10, 20, 10, 10, 10, 10), dim = c(3, 3))
  expect_equal(create_pyramid(3, initial_value = 10, step = 10), expected)
})

test_that("errors", {
  expect_error(create_pyramid(2), "dimension must be odd")
  expect_error(create_pyramid(-1), "dimension must be positive")
})
