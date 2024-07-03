
da = function(a, b, index) {
  matrix(apply(-1 * broadcast.subtraction(b, t(.fedirtClusterEnv$X)) * (.fedirtClusterEnv$rjk(a, b, index) - broadcast.multiplication(.fedirtClusterEnv$Pj(a, b), t(.fedirtClusterEnv$njk(a, b, index)))), c(1), sum))
}
