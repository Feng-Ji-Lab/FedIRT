#' @noRd
g_logL_internal = function(a, b, index) {
  result_a = .fedirtClusterEnv$da(a, b, index)
  result_b = .fedirtClusterEnv$db(a, b, index)
  list(result_a, result_b)
}
