#' @noRd
#' @title Federated gpcm model
#' @description This function is used to test the accuracy and processing time of this algorithm. It inputs a list of responding matrices and return the federated gpcm parameters.
#' Note: This function can only calculate one combined dataset. To use federated gpcm in distributed datasets, please use fedirt_gpcm().
#' @details Input is a List of responding matrices from each school, every responding matrix is one site's data.
#' @param inputdata A List of all responding matrix.
#' @return A list with the estimated global discrimination a, global difficulty b, person's abilities ability, sites' abilities site, log-likelihood value loglik, and standard error SE.
#'
#' @examples
#' \donttest{
#' inputdata = list(as.matrix(example_data_graded))
#' fedresult = fedirt_gpcm_data(inputdata)
#'
#' inputdata = list(as.matrix(example_data_graded_and_binary))
#' fedresult = fedirt_gpcm_data(inputdata)
#'}

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim
#' @importFrom stats optimHess

fedirt_gpcm_data = function(inputdata) {

  my_data <- inputdata
  N <- lapply(my_data, function(x) nrow(x))
  J <- dim(my_data[[1]])[2]
  K <- length(my_data)
  M <- apply(my_data[[1]], 2, function(df) {
    max(df)
  })

  g = function(x) {
    return (exp(-0.5 * x * x) / sqrt(2 * pi))
  }
  logL_gpcm = function(a, b, index, q = 21, lower_bound = -3, upper_bound = 3) {
    # init
    data = my_data[[index]]
    level_diff = (upper_bound - lower_bound) / (q - 1)
    X = as.matrix(as.numeric(map(1:q, function(k) {
      temp = (lower_bound + (k - 1) * level_diff)
      return(temp)
    })))
    A = as.matrix(as.numeric(map(1:q, function(k) {
      temp = (lower_bound + (k - 1) * level_diff)
      quadrature = quadl(g, temp - level_diff * 0.5, temp + level_diff * 0.5)
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
      # 根据答案 data 选对应的概率
      # 原来： N : 21 = N:10 * 10:21
      # 现在： N : 21 = 10 * (N:1 select 3:21)
      answerP = log(Pjx(a[j], b[[j]]))
      # 初始化一个全0的矩阵，矩阵尺寸为N行和M[j]列
      result_matrix <- matrix(0, nrow = N[[index]], ncol = M[j] + 1)
      result_matrix[cbind(seq_len(N[[index]]), data[,j] + 1)] = 1
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
#
#   g_logL_gpcm = function(a, b, index, q = 21, lower_bound = -3, upper_bound = 3) {
#     # init
#     data = my_data[[index]]
#     level_diff = (upper_bound - lower_bound) / (q - 1)
#     X = as.matrix(as.numeric(map(1:q, function(k) {
#       temp = (lower_bound + (k - 1) * level_diff)
#       return(temp)
#     })))
#     A = as.matrix(as.numeric(map(1:q, function(k) {
#       temp = (lower_bound + (k - 1) * level_diff)
#       quadrature = quadl(g, temp - level_diff * 0.5, temp + level_diff * 0.5)
#       return(quadrature)
#     })))
#
#     Px = mem(function(a, b) {
#       - rbind(rep(0, length(X)), a * broadcast.subtraction(t(b), t(X)))
#     })
#     Px_sum = mem(function(a, b) {
#       exp(apply(Px(a,b),2,cumsum))
#     })
#
#     Pjx = mem(function(a, b, j) {
#       # 提供所有答案的概率:  4:21
#       px_sum = Px_sum(a,b)
#       sum_px_sum = matrix(colSums(px_sum), nrow = 1)
#       # if(j==1) {
#       #   ans = broadcast.divide(px_sum, sum_px_sum)
#       #   # print(ans)
#       # }
#       return(broadcast.divide(px_sum, sum_px_sum))
#     })
#     log_Lik_j = mem(function(a, b, j) {
#       # 根据答案 data 选对应的概率
#       # 原来： N : 21 = N:10 * 10:21
#       # 现在： N : 21 = 10 * (N:1 select 3:21)
#       answerP = log(Pjx(a[j], b[[j]]))
#       # 初始化一个全0的矩阵，矩阵尺寸为N行和M[j]列
#       result_matrix <- matrix(0, nrow = N, ncol = M[j] + 1)
#       result_matrix[cbind(seq_len(N), data[,j] + 1)] = 1
#       selected = result_matrix %*% answerP
#       return(selected)
#     })
#
#     Lik_j = mem(function(a, b, j) {
#       exp(log_Lik_j(a,b,j))
#     })
#     LA = mem(function(a, b) {
#       broadcast.multiplication(Lik(a,b), t(A))
#       # 79 * 21
#     })
#     Pxy = mem(function(a, b) {
#       la = LA(a,b) # 79 * 21
#       sum_la = replicate(q, apply(la, c(1), sum)) # 79 * 21
#       la / sum_la # 79 * 21
#     })
#     Pxyr = mem(function(a, b) {
#       aperm(replicate(J, Pxy(a,b)), c(1, 3, 2)) * replicate(q, data) # 10 * 79 * 21
#     })
#
#     njk = mem(function(a, b) {
#       pxy = Pxy(a, b)
#       matrix(apply(pxy, c(2), sum)) # 21 * 1
#     })
#     rjk = mem(function(a, b) {
#       pxyr = Pxyr(a, b)
#       apply(pxyr, c(2, 3), sum) # 10 * 21
#     })
#     da = mem(function(a, b) {
#       matrix(apply(-1 * broadcast.subtraction(b, t(X)) * (rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
#     })
#     db = mem(function(a, b) {
#       -1 * a * matrix(apply((rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
#     })
#
#     result_a = da(a, b)
#     result_b = db(a, b)
#     list(result_a, result_b)
#   }
  logL_entry = function(ps) {
    a = matrix(ps[1:J])
    totalb = as.matrix(ps[(J+1):(J+sum(M))])
    listb = split(totalb, findInterval(seq_along(totalb), c(0, cumsum(M)), left.open = TRUE))
    # b = matrix(ps[(J+1):(2*J)])
    b = lapply(listb, function(vec) {
      matrix(vec, nrow = 1, byrow = TRUE) # byrow = TRUE 使得向量按行填充矩阵
    })
    # print(paste0("logL_entry::", J))
    if(K==1){
      result = logL_gpcm(a,b,1)
    } else{
      result = 0
      for(index in 1:K) {
        result = result + as.numeric(logL_gpcm(a,b,index))
      }
    }

    # print(result)
    result
  }
#   g_logL_entry = function(ps) {
#     a = matrix(ps[1:J])
#     totalb = as.matrix(ps[(J+1):(J+sum(M))])
#     listb = split(totalb, findInterval(seq_along(totalb), c(0, cumsum(M)), left.open = TRUE))
#     # b = matrix(ps[(J+1):(2*J)])
#     b = lapply(listb, function(vec) {
#       matrix(vec, nrow = 1, byrow = TRUE) # byrow = TRUE 使得向量按行填充矩阵
#     })
#     ga = matrix(0, nrow = J)
#     gb = matrix(0, nrow = J)
#     # print(ga)
#     # print(gb)
#     for(index in 1:K) {
#       result = g_logL_gpcm(a,b, index)
#       ga[, 1] = ga[, 1] + result[1:J]
#       gb[, 1] = gb[, 1] + result[(J+1):(2*J)]
#     }
#     # print(rbind(ga,gb))
#     rbind(ga, gb)
#   }
#   my_personfit_gpcm = function(a, b, data, q = 21, lower_bound = -3, upper_bound = 3) {
#     # init
#     N = nrow(data)
#     J = dim(data)[2]
#     level_diff = (upper_bound - lower_bound) / (q - 1)
#     X = as.matrix(as.numeric(map(1:q, function(k) {
#       temp = (lower_bound + (k - 1) * level_diff)
#       return(temp)
#     })))
#     A = as.matrix(as.numeric(map(1:q, function(k) {
#       temp = (lower_bound + (k - 1) * level_diff)
#       quadrature = quadl(g, temp - level_diff * 0.5, temp + level_diff * 0.5)
#       return(quadrature)
#     })))
#
#     Pj = mem(function(a, b) {
#       t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
#       return (t / (1 + t))
#     })
#     Qj = mem(function(a, b) {
#       return (1 - Pj(a, b))
#     })
#
#     log_Lik = mem(function(a, b) {
#       data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
#     })
#
#     Lik = mem(function(a, b) {
#       exp(log_Lik(a, b))
#     })
#
#     LA = mem(function(a, b) {
#       broadcast.multiplication(Lik(a,b), t(A))
#     })
#     result = list()
#     ta = matrix(a, J, 1)
#     tb = matrix(b, J, 1)
#     result[["ability"]] = matrix(apply(broadcast.multiplication(LA(ta,tb), t(X)), c(1), sum)) / matrix(apply(LA(ta,tb), c(1), sum))
#
#     result[["site"]] = mean(result[["ability"]])
#
#     result[["person"]] = result[["ability"]] - result[["site"]]
#     return(result)
#   }


  fed_irt_entry = function(data) {
      get_new_ps = function(ps_old) {
        optim_result = optim(par = ps_old, fn = logL_entry, method = "BFGS",
                             control = list(fnscale=-1, trace = 0, maxit = 10000), hessian = TRUE)
        hessian_adjusted <- -optim_result$hessian
        hessian_inv = solve(hessian_adjusted)

        SE = sqrt(diag(hessian_inv))
        list(result = optim_result, SE = SE)
    }

    ps_init = c(rep(1, J), rep(0, sum(M)))

    optim_result = get_new_ps(ps_init)
    ps_next = optim_result$result
    ps_next$loglik = logL_entry(ps_next$par)

    ps_next$b = ps_next$par[(J+1):(J+sum(M))]
    ps_next$a = ps_next$par[1:J]

    ps_next$SE$b = optim_result$SE[(J+1):(J+sum(M))]
    ps_next$SE$a = optim_result$SE[1:J]

    ps_next
  }


  fed_irt_entry(inputdata)

}

