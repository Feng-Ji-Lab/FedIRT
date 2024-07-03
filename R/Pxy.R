
Pxy = function(a, b, index) {
  la = .fedirtClusterEnv$LA(a,b,index)
  sum_la = replicate(.fedirtClusterEnv$q, apply(la, c(1), sum))
  la / sum_la
}
