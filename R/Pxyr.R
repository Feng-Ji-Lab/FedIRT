
Pxyr = function(a, b, index) {
  aperm(replicate(.fedirtClusterEnv$J, .fedirtClusterEnv$Pxy(a,b,index)), c(1, 3, 2)) * replicate(.fedirtClusterEnv$q, .fedirtClusterEnv$my_data[[index]])
}
