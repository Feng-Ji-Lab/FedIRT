
rjk = function(a, b, index) {
  pxyr = .fedirtClusterEnv$Pxyr(a, b, index)
  apply(pxyr, c(2, 3), sum)
}
