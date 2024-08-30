#' @noRd
#' @title Federated 2PL model with school effects
#' @description This function is used to test the accuracy and processing time of this algorithm. It inputs a list of responding matrices and return the federated 2PL parameters.
#' Note: To use federated 2PL in distributed datasets, please use fedirt_2PL().
#' @details Input is a List of responding matrices from each school, every responding matrix is one site's data.
#' @param inputdata A List of all responding matrices.
#' @return A list with the estimated global discrimination a, global difficulty b, person's abilities ability, sites' abilities site, log-likelihood value loglik, and the standard error SE. It also displays the school ability sc, which is considered as a fixed effect.
#'
#' @examples
#' inputdata = list(as.matrix(example_data_2PL))
#' fedresult = fedirt_2PL_schooleffects(inputdata)
#'
#' inputdata = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
#' fedresult = fedirt_2PL_schooleffects(inputdata)
#'

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim
#' @importFrom stats optimHess

fedirt_2PL_schooleffects = function(inputdata) {
  my_data <- inputdata
  N <- lapply(my_data, function(x) nrow(x))
  J <- dim(my_data[[1]])[2]
  K <- length(my_data)
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
  q = 21
  lower_bound = -3
  upper_bound = 3
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
  Pj = mem(function(a, b,sc, index) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X + sc[index]))))
    return (t / (1 + t))
  })
  Qj = mem(function(a, b,sc, index) {
    return (1 - Pj(a, b, sc, index))
  })

  log_Lik = mem(function(a, b, sc, index) {
    my_data[[index]] %*% log(Pj(a, b,sc, index))  + (1 - my_data[[index]]) %*% log(Qj(a, b,sc, index))
  })

  Lik = mem(function(a, b, sc, index) {
    exp(log_Lik(a, b, sc, index))
  })

  LA = mem(function(a, b, sc, index) {
    broadcast.multiplication(Lik(a,b,sc,index), t(A))
  })

  Pxy = mem(function(a, b, index) {
    la = LA(a,b,index)
    sum_la = replicate(q, apply(la, c(1), sum))
    la / sum_la
  })
  Pxyr = mem(function(a, b, index) {
    aperm(replicate(J, Pxy(a,b,index)), c(1, 3, 2)) * replicate(q, my_data[[index]])
  })

  njk = mem(function(a, b, index) {
    pxy = Pxy(a, b, index)
    matrix(apply(pxy, c(2), sum))
  })
  rjk = mem(function(a, b, index) {
    pxyr = Pxyr(a, b, index)
    apply(pxyr, c(2, 3), sum)
  })
  da = mem(function(a, b, index) {
    matrix(apply(-1 * broadcast.subtraction(b, t(X)) * (rjk(a, b, index) - broadcast.multiplication(Pj(a, b), t(njk(a, b, index)))), c(1), sum))
  })
  db = mem(function(a, b, index) {
    -1 * a * matrix(apply((rjk(a, b, index) - broadcast.multiplication(Pj(a, b), t(njk(a, b, index)))), c(1), sum))
  })
  g_logL = function(a, b, index) {
    result_a = da(a, b, index)
    result_b = db(a, b, index)
    list(result_a, result_b)
  }
  logL = function(a, b, sc, index) {
    sum(log(matrix(apply(broadcast.multiplication(Lik(a, b, sc, index), t(A)), c(1), sum))))
  }
  logL_entry = function(ps) {
    a = matrix(ps[1:J])
    b = matrix(ps[(J+1):(2*J)])
    sc = matrix(ps[(J+J+1):(J+J+K)])
    result = sum(unlist(map(1:K, function(index) logL(a, b, sc, index))))
    result
  }

  g_logL_entry = function(ps) {
    a = matrix(ps[1:J])
    b = matrix(ps[(J+1):(2*J)])
    ga = matrix(0, nrow = J)
    gb = matrix(0, nrow = J)
    for(index in 1:K) {
      result = g_logL(a, b, index)
      ga = ga + result[[1]]
      gb = gb + result[[2]]
    }
    rbind(ga, gb)
  }

  my_personfit = function(a, b, sc) {
    result = list()
    result[["a"]] = a
    result[["b"]] = b
    for(index in 1:K) {
      result[["ability"]][[index]] = matrix(apply(broadcast.multiplication(LA(a,b,sc, index), t(X + sc[index])), c(1), sum)) / matrix(apply(LA(a,b,sc, index), c(1), sum)) - sc[index]
    }
    return(result)
  }

  fed_irt_entry = function(data) {
    get_new_ps = function(ps_old) {
      optim_result = optim(par = ps_old, fn = logL_entry, method = "BFGS",
                           control = list(fnscale=-1, trace = 0, maxit = 10000),hessian = TRUE)
      hessian_adjusted <- -optim_result$hessian
      hessian_inv = solve(hessian_adjusted)

      SE = sqrt(diag(hessian_inv))
      list(result = optim_result, SE = SE)
    }

    ps_init = c(rep(1, J), rep(0, J), rep(0, K))
    optim_result = get_new_ps(ps_init)
    ps_next = optim_result$result
    ps_next$loglik = logL_entry(ps_next$par)

    ps_next$b = ps_next$par[(J+1):(J+J)]
    ps_next$a = ps_next$par[1:J]
    ps_next$sc = ps_next$par[(J+J+1):(J+J+K)]

    ps_next$SE$a = optim_result$SE[1:J]
    ps_next$SE$b = optim_result$SE[(J+1):(J+J)]
    ps_next$SE$sc = optim_result$SE[(J+J+1):(J+J+K)]

    ps_next$person = my_personfit(ps_next[["a"]], ps_next[["b"]], ps_next$sc)
    ps_next
  }



  fed_irt_entry(inputdata)
}
