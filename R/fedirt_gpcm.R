#' Federated Graded Response Model Estimation Function
#' @description Implements a federated learning approach for the estimation of the graded response model parameters, enabling collaborative parameter estimation across distributed datasets while ensuring individual data source privacy.
#' @details The function adopts a federated learning framework to perform estimation of item step difficulties and individual ability levels in an IRT graded response model without needing to pool the data into one centralized dataset. The estimator follows an iterative optimization procedure consisisting of local computations, information sharing with a central aggregator, and updating of the global parameters.

#' @param J An integer indicating the number of items in the IRT model across all sites.
#' This number should be consistent for all response matrices provided.
#' @param M An integer vector indicating the maximum level (number of categories minus one) for each item across all sites, which determines the total number of step difficulties to estimate for the graded response model.
#' @param logL_entry A function that calculates the sum of log-likelihoods for the response matrices across all sites.
#' This function is crucial for evaluating the fit of the model at each iteration.
#' @param g_logL_entry A function that computes the aggregated gradient of the log-likelihood across all participating entities.

#' @return A list containing the following components from the federated graded model estimation:
#' \itemize{
#' \item \code{par}: Numeric vector of model's fitted parameters including item discrimination (a) and item difficulty (b) parameters.
#' \item \code{value}: The optimization objective function's value at the found solution, typically the log-likelihood.
#' \item \code{counts}: Named integer vector with counts of function evaluations and gradient evaluations during optimization.
#' \item \code{convergence}: Integer code indicating the optimization's convergence status (0 indicates successful convergence).
#' \item \code{message}: Message from optimizer about optimization process, NULL if no message is available.
#' \item \code{loglik}: The calculated log-likelihood of the fitted model, identical to the 'value' element when the objective function is log-likelihood.
#' \item \code{a}: Numeric vector of estimated item discrimination parameters.
#' \item \code{b}: Numeric vector of estimated item difficulty parameters.
#' }
#'
#' @references
#' Muraki, E. (1992). A generalized partial credit model: Application of an EM algorithm. \emph{Applied Psychological Measurement}, 16(2), 159--176. \doi{10.1177/014662169201600206}
#'
#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim

#' @export
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

#' @title Log-Likelihood of the federated graded Model
#'
#' @description Computes the log-likelihood of the graded IRT model given item parameters and response data. The computation utilizes numerical integration and is optimized through memoization for repeated evaluations.
#'
#' @details The function performs numerical integration over a set of quadrature points to calculate the probabilities of the observed responses under the graded model, considering the item discrimination (a) and difficulty (b) parameters. Memoization is used to cache computed values of the probabilities, logits, and log-likelihoods to avoid redundant calculations and speed up the process.

#' @param a The vector of item discrimination parameters in the graded model.
#' @param b The vector of item difficulty parameters in the graded model.
#' @param data The matrix of observed responses, with individuals in rows and items in columns.
#' @param q The number of Gaussian quadrature points to use for numerical integration (default is 21). Gaussian quadrature is a numerical integration technique to approximate the integral of a function, and is particularly useful for accurate and efficient computation.
#' @param lower_bound The lower limit for the Gaussian quadrature integration (default is -3).
#' @param upper_bound The upper limit for the Gaussian quadrature integration (default is 3).
#'
#' @return The computed log-likelihood of the graded model as a single numeric value.
#'
#' @importFrom purrr map
#' @importFrom pracma quadl
#' @export
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

#' @title Gradient of Log-Likelihood for the federated graded Model
#'
#' @description Calculates the gradients of the log-likelihood function with respect to the item discrimination (a) and difficulty (b) parameters for the graded IRT model. This computation is vital for optimizing the item parameters via gradient-based optimization algorithms.
#'
#' @details The function approximates the partial derivatives by utilizing Gaussian quadrature for numerical integration. Memoization techniques are used to cache intermediate results, which is crucial for efficient computation because it avoids redundant calculations. This can significantly speed up iterative algorithms, particularly in the context of large datasets.

#' @param a Numeric vector of item discrimination parameters in the graded model.
#' @param b Numeric vector of item difficulty parameters in the graded model.
#' @param data The matrix of observed item responses, with individuals in rows and items in columns.
#' @param q The number of Gaussian quadrature points for numerical integration (default is 21).
#' @param lower_bound The lower bound for Gaussian quadrature integration (default is -3).
#' @param upper_bound The upper bound for Gaussian quadrature integration (default is 3).
#'
#' @return A list containing two elements: the gradient vector with respect to item discrimination parameters ('a') and the gradient vector with respect to item difficulty parameters ('b').
#'
#' @importFrom purrr map
#' @importFrom pracma quadl
#' @export
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
    # 根据答案 data 选对应的概率
    # 原来： N : 21 = N:10 * 10:21
    # 现在： N : 21 = 10 * (N:1 select 3:21)
    answerP = log(Pjx(a[j], b[[j]]))
    # 初始化一个全0的矩阵，矩阵尺寸为N行和M[j]列
    result_matrix <- matrix(0, nrow = N, ncol = M[j] + 1)
    result_matrix[cbind(seq_len(N), data[,j] + 1)] = 1
    selected = result_matrix %*% answerP
    return(selected)
  })

  Lik_j = mem(function(a, b, j) {
    exp(log_Lik_j(a,b,j))
  })
  # zby 标注尺寸
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
  class(result) <- "fedirt"
  return(result)
}

