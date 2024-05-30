#' @title Federated 2PL model
#' @description This function implements a federated learning approach to estimate the parameters of the 2PL IRT model. It allows for collaborative estimation across multiple datasets, while maintaining the privacy of each individual data source. The federated 2PL model is particularly useful in contexts where data sharing might be limited due to privacy concerns or logistical constraints.
#' @details The algorithm leverages federated learning techniques to estimate shared item parameters and individual ability levels without requiring the raw data to be combined into a single dataset.
#' The estimation procedure is composed of several steps, including initialization, local computations at each data source, communication of summary statistics to a central server, and global parameter updates.
#' This cycle is repeated until convergence criteria are met and the global parameters stabilize.
#'

#' Regarding the input parameters, 'J' is the number of items across all sites, which should be consistent and known in advance.
#' The 'logL_entry' parameter should be a function that computes the log-likelihood of the observed responses given the current model parameters.
#' Likewise, 'g_logL_entry' is expected to be a function that computes the gradient of the log-likelihood with respect to the model parameters to inform the optimization process during parameter estimation.
#' @param J An integer indicating the number of items in the IRT model across all sites.
#' This number should be consistent for all response matrices provided.
#' @param logL_entry A function that calculates the sum of log-likelihoods for the response matrices across all sites.
#' This function is crucial for evaluating the fit of the model at each iteration.
#' @param g_logL_entry A function that computes the aggregated gradient of the log-likelihood across all participating entities.
#'
#' @return A list containing the following components from the federated 2PL model estimation:
#' \itemize{
#' \item \code{par}: Numeric vector of model's fitted parameters including item discrimination (a) and item difficulty (b) parameters.
#' \item \code{value}: The optimization objective function's value at the found solution, typically the log-likelihood.
#' \item \code{counts}: Named integer vector with counts of function evaluations and gradient evaluations during optimization.
#' \item \code{convergence}: Integer code indicating the optimization's convergence status (0 indicates successful convergence).
#' \item \code{message}: Message from optimizer about optimization process, NULL if no message is available.
#' \item \code{loglik}: The calculated log-likelihood of the fitted model, identical to the 'value' element when the objective function is log-likelihood.
#' \item \code{a}: Numeric vector of estimated item discrimination parameters.
#' \item \code{b}: Numeric vector of estimated item difficulty parameters.
#' \item \code{person}: List containing person-related estimates with elements:
#' \itemize{
#'   \item \code{a}: Vector of discrimination parameters (same as top-level 'a').
#'   \item \code{b}: Vector of difficulty parameters (same as top-level 'b').
#'   \item \code{ability}: List of numeric vectors with person abilities per site.
#'   \item \code{site}: Numeric vector of abilities or locations specific to each site.
#'   \item \code{person}: List of numeric vectors of person abilities minus site ability.
#'   }
#' }
#'


#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim

#' @export
fedirt_2PL = function(J,logL_entry, g_logL_entry) {
  get_new_ps = function(ps_old) {
    # "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"
    optim(par = ps_old, fn = logL_entry, gr = g_logL_entry, method = "BFGS", control = list(fnscale=-1, trace = 0,  maxit = 10000))
  }
  ps_init = c(rep(1, J), rep(0, J))
  ps_next = get_new_ps(ps_init)
  ps_next$loglik = logL_entry(ps_next$par)

  ps_next$b = ps_next$par[(J+1):(J+J)]
  ps_next$a = ps_next$par[1:J]

  ps_next
}

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
#' Memoization Function for Speed Optimization
#'
#' @description A simple memoization function that stores the results of expensive function calls and reuses those results when the same inputs occur again. This technique greatly speeds up the computation of `fedirt` function by caching previously computed values.
#'
#' @param f Function to be memoized.
#'
#' @return Returns a memoized version of function `f` that will cache its previously computed results for faster subsequent evaluations, especially beneficial when applied to `fedirt`.
#'
#' @examples
#' # To memoize a function, simply wrap it with `memoize`:
#' memoize(function(a,b){return(a+b)})
#' @export
memoize <- function(f) {
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

#' Log-Likelihood of the federated 2PL Model
#'
#' @description Computes the log-likelihood of the Two-Parameter Logistic (2PL) IRT model given item parameters and response data. The computation utilizes numerical integration and is optimized through memoization for repeated evaluations.
#'
#' @details The function performs numerical integration over a set of quadrature points to calculate the probabilities of the observed responses under the 2PL model, considering the item discrimination (a) and difficulty (b) parameters. Memoization is used to cache computed values of the probabilities, logits, and log-likelihoods to avoid redundant calculations and speed up the process.
#'
#' @param a The vector of item discrimination parameters in the 2PL model.
#' @param b The vector of item difficulty parameters in the 2PL model.
#' @param data The matrix of observed responses, with individuals in rows and items in columns.
#' @param q The number of Gaussian quadrature points to use for numerical integration (default is 21). Gaussian quadrature is a numerical integration technique to approximate the integral of a function, and is particularly useful for accurate and efficient computation.
#' @param lower_bound The lower limit for the Gaussian quadrature integration (default is -3).
#' @param upper_bound The upper limit for the Gaussian quadrature integration (default is 3).
#'
#' @return The computed log-likelihood of the 2PL model as a single numeric value.
#'
#' @importFrom purrr map
#' @importFrom pracma quadl
#' @export
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

  Pj = memoize(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = memoize(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = memoize(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = memoize(function(a, b) {
    exp(log_Lik(a, b))
  })

  sum(log(matrix(apply(broadcast.multiplication(Lik(a, b), t(A)), c(1), sum))))
}
#' Gradient of Log-Likelihood for the federated 2PL Model
#'
#' @description Calculates the gradients of the log-likelihood function with respect to the item discrimination (a) and difficulty (b) parameters for the Two-Parameter Logistic (2PL) Item Response Theory (IRT) model. This computation is vital for optimizing the item parameters via gradient-based optimization algorithms.
#'
#' @details The function approximates the partial derivatives by utilizing Gaussian quadrature for numerical integration. Memoization techniques are used to cache intermediate results, which is crucial for efficient computation because it avoids redundant calculations. This can significantly speed up iterative algorithms, particularly in the context of large datasets.
#'
#' The partial gradient for each parameter is:
#' \deqn{ \frac{ \partial l_k}  { \partial \alpha_j } = \sum\limits_{n=1}^{q} \sum\limits_{i=1}^{N_k} (V_{ik}(n) - \beta_j) [ r_{ijnk} - m_{ink} P_j(V_{ik}(n))] }
#' \deqn{ \frac{ \partial l_k}  { \partial \beta_j } = (-\alpha_j)\sum\limits_{n=1}^{q} \sum\limits_{i=1}^{N_k} [ r_{ijnk} - m_{ink} P_j(V_{ik}(n))] }
#'
#' @param a Numeric vector of item discrimination parameters in the 2PL model.
#' @param b Numeric vector of item difficulty parameters in the 2PL model.
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

  Pj = memoize(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = memoize(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = memoize(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = memoize(function(a, b) {
    exp(log_Lik(a, b))
  })

  LA = memoize(function(a, b) {
    broadcast.multiplication(Lik(a,b), t(A))
  })
  Pxy = memoize(function(a, b) {
    la = LA(a,b)
    sum_la = replicate(q, apply(la, c(1), sum))
    la / sum_la
  })
  Pxyr = memoize(function(a, b) {
    aperm(replicate(J, Pxy(a,b)), c(1, 3, 2)) * replicate(q, data)
  })

  njk = memoize(function(a, b) {
    pxy = Pxy(a, b)
    matrix(apply(pxy, c(2), sum))
  })
  rjk = memoize(function(a, b) {
    pxyr = Pxyr(a, b)
    apply(pxyr, c(2, 3), sum)
  })
  da = memoize(function(a, b) {
    matrix(apply(-1 * broadcast.subtraction(b, t(X)) * (rjk(a, b) - broadcast.multiplication(Pj(a, b), t(njk(a, b)))), c(1), sum))
  })
  db = memoize(function(a, b) {
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

  Pj = memoize(function(a, b) {
    t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, t(X))))
    return (t / (1 + t))
  })
  Qj = memoize(function(a, b) {
    return (1 - Pj(a, b))
  })

  log_Lik = memoize(function(a, b) {
    data %*% log(Pj(a, b))  + (1 - data) %*% log(Qj(a, b))
  })

  Lik = memoize(function(a, b) {
    exp(log_Lik(a, b))
  })

  LA = memoize(function(a, b) {
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
