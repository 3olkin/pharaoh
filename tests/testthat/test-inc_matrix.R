test_that("returns correct matrix", {
  initial_matrix <- array(0, dim = c(3, 3))
  expected <- array(c(0, 0, 0, 0, 10, 10, 0, 10, 20), dim = c(3, 3))
  expect_equal(inc_matrix(initial_matrix, step = 10), expected)
})
