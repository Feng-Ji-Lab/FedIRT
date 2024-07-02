broadcast.fortmat <- function(mat1, mat2) {
  row1 <- nrow(mat1)
  col1 <- ncol(mat1)
  row2 <- nrow(mat2)
  col2 <- ncol(mat2)
  if(col1 != 1 && row2 != 1) {
    stop("illegal operation: not 1")
  }
  if(col1 == 1) {
    mat1_new <- mat1[, rep(1:col1, col2)]
  } else if(col1 != col2) {
    stop("illegal operation: col1")
  } else {
    mat1_new <- mat1
  }
  if(row2 == 1) {
    mat2_new <- mat2[rep(1:row2, each=row1), ]
  } else if(row2 != row1) {
    stop("illegal operation: row2")
  } else {
    mat2_new <- mat2
  }

  list(mat1_new, mat2_new)
}
