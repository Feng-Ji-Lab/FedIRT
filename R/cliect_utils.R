broadcast.fortmat <- function(mat1, mat2) {
  row1 <- nrow(mat1)
  col1 <- ncol(mat1)
  row2 <- nrow(mat2)
  col2 <- ncol(mat2)
  if(col1 != 1 && row2 != 1) {
    stop("illegal operation: not 1")
  }
  if(col1 == 1) {
    mat1_new <- mat1[, rep(1:col1, col2)]
  } else if(col1 != col2) {
    stop("illegal operation: col1")
  } else {
    mat1_new <- mat1
  }
  if(row2 == 1) {
    mat2_new <- mat2[rep(1:row2, each=row1), ]
  } else if(row2 != row1) {
    stop("illegal operation: row2")
  } else {
    mat2_new <- mat2
  }

  list(mat1_new, mat2_new)
}

broadcast.multiplication <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new * mat2_new)
}
broadcast.divide <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new / mat2_new)
}
broadcast.subtraction <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new - mat2_new)
}
broadcast.exponentiation <- function(mat1, mat2) {
  format_result = broadcast.fortmat(mat1, mat2)
  mat1_new = format_result[[1]]
  mat2_new = format_result[[2]]
  return(mat1_new ^ mat2_new)
}

mem <- function(f) {
  memo <- new.env(parent = emptyenv())
  function(...) {
    key <- paste(list(...), collapse = " ,")
    if(!exists(as.character(key), envir = memo)) {
      memo[[as.character(key)]] <- f(...)
    }
    memo[[as.character(key)]]
  }
}
g = function(x) {
  return (exp(-0.5 * x * x) / sqrt(2 * pi))
}

logL = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Pj = mem(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = mem(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = mem(function(a, b) {
    exp(log_Lik(a, b))
  })

  sum(log(matrix(apply(broadcast.multiplication(Lik(a, b), t(A)), c(1), sum))))
}

g_logL = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Pj = mem(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = mem(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = mem(function(a, b) {
    exp(log_Lik(a, b))
  })

  LA = mem(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
  })
  Pxy = mem(function(a, b) {
    la = LA(a,b)
    sum_la = replicate(q, apply(la, c(1), sum))
    la / sum_la
  })
  Pxyr = mem(function(a, b) {
    aperm(replicate(J, Pxy(a,b)), c(1, 3, 2)) * replicate(q, data)
  })

  njk = mem(function(a, b) {
    pxy = Pxy(a, b)
    matrix(apply(pxy, c(2), sum))
  })
  rjk = mem(function(a, b) {
    pxyr = Pxyr(a, b)
    apply(pxyr, c(2, 3), sum)
  })
  da = mem(function(a, b) {
    matrix(apply(-1 * broadcast.subtraction(b, t(X)) * (rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })
  db = mem(function(a, b) {
    -1 * a * matrix(apply((rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })

  result_a = da(a, b)
  result_b = db(a, b)
  list(result_a, result_b)
}

my_personfit = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Pj = mem(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = mem(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = mem(function(a, b) {
    exp(log_Lik(a, b))
  })

  LA = mem(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
  })
  result = list()
  ta = matrix(a, J, 1)
  tb = matrix(b, J, 1)
  result[["ability"]] = matrix(apply(broadcast.multiplication(LA(ta,tb), t(X)), c(1), sum)) / matrix(apply(LA(ta,tb), c(1), sum))

  result[["site"]] = mean(result[["ability"]])

  result[["person"]] = result[["ability"]] - result[["site"]]
  return(result)
}
fedirt_gpcm = function(J, M,logL_entry, g_logL_entry) {
  get_new_ps = function(ps_old) {
    # "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"
    optim(par = ps_old, fn = logL_entry, method = "BFGS", control = list(fnscale=-1, trace = 0,  maxit = 10000))
  }
  ps_init = c(rep(1, J), rep(0, sum(M)))
  # print("fedirt_gpcm 2::")
  # print(M)
  # print(J)
  # print(sum(M))
  # print(ps_init)
  ps_next = get_new_ps(ps_init)
  ps_next$loglik = logL_entry(ps_next$par)

  ps_next$b = ps_next$par[(J+1):(J+sum(M))]
  ps_next$a = ps_next$par[1:J]

  ps_next
}

logL_gpcm = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  M <- apply(data, 2, function(df) {
    max(df)
  })
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Px = mem(function(a, b) {
    - rbind(rep(0, length(X)), a * broadcast.subtraction(t(b), t(X)))
  })
  Px_sum = mem(function(a, b) {
    exp(apply(Px(a,b),2,cumsum))
  })

  Pjx = mem(function(a, b, j) {
    px_sum = Px_sum(a,b)
    sum_px_sum = matrix(colSums(px_sum), nrow = 1)
    return(broadcast.divide(px_sum, sum_px_sum))
  })
  log_Lik_j = mem(function(a, b, j) {
    answerP = log(Pjx(a[j], b[[j]]))

    result_matrix <- matrix(0, nrow = N, ncol = M[j] + 1)
    result_matrix[cbind(seq_len(N), data[,j] + 1)] = 1
    selected = result_matrix %*% answerP
    return(selected)
  })

  Lik_j = mem(function(a, b, j) {
    exp(log_Lik_j(a,b,j))
  })

  finalLogL = 0
  for(j in 1:J) {
    temp = log_Lik_j(a, b, j)
    finalLogL = finalLogL + temp
  }
  sum(log(matrix(apply(broadcast.multiplication(exp(finalLogL), t(A)), c(1), sum))))
}

g_logL_gpcm = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  M <- apply(data, 2, function(df) {
    max(df)
  })
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Px = mem(function(a, b) {
    - rbind(rep(0, length(X)), a * broadcast.subtraction(t(b), t(X)))
  })
  Px_sum = mem(function(a, b) {
    exp(apply(Px(a,b),2,cumsum))
  })

  Pjx = mem(function(a, b, j) {
    # 提供所有答案的概率:  4:21
    px_sum = Px_sum(a,b)
    sum_px_sum = matrix(colSums(px_sum), nrow = 1)
    # if(j==1) {
    #   ans = broadcast.divide(px_sum, sum_px_sum)
    #   # print(ans)
    # }
    return(broadcast.divide(px_sum, sum_px_sum))
  })
  log_Lik_j = mem(function(a, b, j) {

    answerP = log(Pjx(a[j], b[[j]]))

    result_matrix <- matrix(0, nrow = N, ncol = M[j] + 1)
    result_matrix[cbind(seq_len(N), data[,j] + 1)] = 1
    selected = result_matrix %*% answerP
    return(selected)
  })

  Lik_j = mem(function(a, b, j) {
    exp(log_Lik_j(a,b,j))
  })
  LA = mem(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
    # 79 * 21
  })
  Pxy = mem(function(a, b) {
    la = LA(a,b) # 79 * 21
    sum_la = replicate(q, apply(la, c(1), sum)) # 79 * 21
    la / sum_la # 79 * 21
  })
  Pxyr = mem(function(a, b) {
    aperm(replicate(J, Pxy(a,b)), c(1, 3, 2)) * replicate(q, data) # 10 * 79 * 21
  })

  njk = mem(function(a, b) {
    pxy = Pxy(a, b)
    matrix(apply(pxy, c(2), sum)) # 21 * 1
  })
  rjk = mem(function(a, b) {
    pxyr = Pxyr(a, b)
    apply(pxyr, c(2, 3), sum) # 10 * 21
  })
  da = mem(function(a, b) {
    matrix(apply(-1 * broadcast.subtraction(b, t(X)) * (rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })
  db = mem(function(a, b) {
    -1 * a * matrix(apply((rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })

  result_a = da(a, b)
  result_b = db(a, b)
  list(result_a, result_b)
}

my_personfit_gpcm = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
  # init
  N = nrow(data)
  J = dim(data)[2]
  level_diff = (upper_bound - lower_bound) / (q - 1)
  X = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    return(index)
  })))
  A = as.matrix(as.numeric(map(1:q, function(k) {
    index = (lower_bound + (k - 1) * level_diff)
    quadrature = quadl(g, index - level_diff * 0.5, index + level_diff * 0.5)
    return(quadrature)
  })))

  Pj = mem(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = mem(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = mem(function(a, b) {
    exp(log_Lik(a, b))
  })

  LA = mem(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
  })
  result = list()
  ta = matrix(a, J, 1)
  tb = matrix(b, J, 1)
  result[["ability"]] = matrix(apply(broadcast.multiplication(LA(ta,tb), t(X)), c(1), sum)) / matrix(apply(LA(ta,tb), c(1), sum))

  result[["site"]] = mean(result[["ability"]])

  result[["person"]] = result[["ability"]] - result[["site"]]
  return(result)
}