#' @noRd
logL_internal = function(a, b, index) {
  sum(log(matrix(apply(broadcast.multiplication(.fedirtClusterEnv$Lik(a, b, index), t(.fedirtClusterEnv$A)), c(1), sum))))
}
