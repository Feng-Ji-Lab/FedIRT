log_Lik = function(a, b, index) {
  .fedirtClusterEnv$my_data[[index]] %*% log(.fedirtClusterEnv$Pj(a, b))  + (1 - .fedirtClusterEnv$my_data[[index]]) %*% log(.fedirtClusterEnv$Qj(a, b))
}
