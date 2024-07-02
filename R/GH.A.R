GH.A = function(q = 21, lower_bound = -3, upper_bound = 3) {
  level_diff = (upper_bound - lower_bound) / (q - 1)

  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))
  return(A)
}

