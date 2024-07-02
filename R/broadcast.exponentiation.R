broadcast.exponentiation <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new ^ mat2_new)
}
