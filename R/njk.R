
njk = function(a, b, index) {
  pxy = .fedirtClusterEnv$Pxy(a, b, index)
  matrix(apply(pxy, c(2), sum))
}
