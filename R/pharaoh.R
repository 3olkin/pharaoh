inc_matrix <- function(matrix, step, dimension = ncol(matrix)) {
  for (i in 2:dimension) {
    for (j in 1:dimension) {
      matrix[i, j] <- matrix[i - 1, j] + ifelse(j >= i, step, 0)
    }
  }
  matrix
}

prepare_quadrants <- function(center, dimension) {
  list(
    list(1:center, center:dimension),
    list(center:dimension, center:dimension),
    list(center:dimension, 1:center),
    list(1:center, 1:center)
  )
}

#' @title create_pyramid
#'
#' @description Creates pyramid height matrix
#'
#' @param dimension Matrix dimension
#' @param initial_value Matrix initial value
#' @param step Height delta between levels of pyramid
#'
#' @export
create_pyramid <- function(dimension, initial_value = 0, step = 0) {
  if (dimension <= 0) stop("dimension must be positive")
  if (dimension %% 2 == 0) stop("dimension must be odd")

  matrix <- array(initial_value, dim = c(dimension, dimension))
  center <- ceiling(dimension / 2)
  quadrants <- prepare_quadrants(center, dimension)
  value <- inc_matrix(matrix[1:center, 1:center], step, dimension = center)
  for (i in 1:4) {
    matrix[quadrants[[i]][[1]], quadrants[[i]][[2]]] <-
      omnibus::rotateMatrix(value, 90 * i)
  }
  matrix
}
