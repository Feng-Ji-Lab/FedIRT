#' @noRd
#' @title Federated 2PL model
#' @description This function is used to test the accuracy and processing time of this algorithm. It inputs a list of responding matrices and return the federated 2PL parameters.
#' Note: To use federated 2PL in distributed datasets, please use fedirt_2PL().
#' @details Input is a List of responding matrices from each school, every responding matrix is one site's data.
#' @param inputdata A List of all responding matrix.
#' @return A list with the estimated global discrimination a, global difficulty b, person's abilities ability, sites' abilities site, log-likelihood value loglik, and standard error array SE.
#'
#' @examples
#' inputdata = list(as.matrix(example_data_2PL))
#' fedresult = fedirt_2PL_data(inputdata)
#'
#' inputdata = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
#' fedresult = fedirt_2PL_data(inputdata)
#'

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim
#' @importFrom stats sd
#' @importFrom stats optimHess

fedirt_2PL_data = function(inputdata) {
  .fedirtClusterEnv$my_data <- inputdata
  .fedirtClusterEnv$N <- lapply(.fedirtClusterEnv$my_data, function(x) nrow(x))
  .fedirtClusterEnv$J <- dim(.fedirtClusterEnv$my_data[[1]])[2]
  .fedirtClusterEnv$K <- length(.fedirtClusterEnv$my_data)

  .fedirtClusterEnv$q = 21
  lower_bound = -3
  upper_bound = 3
  .fedirtClusterEnv$X = GH.X(.fedirtClusterEnv$q,lower_bound,upper_bound)
  .fedirtClusterEnv$A = GH.A(.fedirtClusterEnv$q,lower_bound,upper_bound)
  .fedirtClusterEnv$Pj = mem(Pj)
  .fedirtClusterEnv$Qj = mem(Qj)

  .fedirtClusterEnv$log_Lik = mem(log_Lik)

  .fedirtClusterEnv$Lik = mem(Lik)

  .fedirtClusterEnv$LA = mem(LA)

  .fedirtClusterEnv$Pxy = mem(Pxy)
  .fedirtClusterEnv$Pxyr = mem(Pxyr)

  .fedirtClusterEnv$njk = mem(njk)
  .fedirtClusterEnv$rjk = mem(rjk)
  .fedirtClusterEnv$da = mem(da)
  .fedirtClusterEnv$db = mem(db)
  logL_entry = function(ps) {
    a = matrix(ps[1:.fedirtClusterEnv$J])
    b = matrix(ps[(.fedirtClusterEnv$J+1):(2*.fedirtClusterEnv$J)])
    result = sum(unlist(map(1:.fedirtClusterEnv$K, function(index) logL_internal(a, b, index))))
    result
  }

  g_logL_entry = function(ps) {
    a = matrix(ps[1:.fedirtClusterEnv$J])
    b = matrix(ps[(.fedirtClusterEnv$J+1):(2*.fedirtClusterEnv$J)])
    ga = matrix(0, nrow = .fedirtClusterEnv$J)
    gb = matrix(0, nrow = .fedirtClusterEnv$J)
    for(index in 1:.fedirtClusterEnv$K) {
      result = g_logL_internal(a, b, index)
      ga = ga + result[[1]]
      gb = gb + result[[2]]
    }
    rbind(ga, gb)
  }

  my_person = function(a, b) {
    result = list()
    result[["a"]] = a
    result[["b"]] = b
    for(index in 1:.fedirtClusterEnv$K) {
      result[["ability"]][[index]] = matrix(apply(broadcast.multiplication(.fedirtClusterEnv$LA(a,b,index), t(.fedirtClusterEnv$X)), c(1), sum)) / matrix(apply(.fedirtClusterEnv$LA(a,b,index), c(1), sum))
    }

    for(index in 1:.fedirtClusterEnv$K) {
      result[["site"]][[index]] = mean(result[["ability"]][[index]])
    }

    for(index in 1:.fedirtClusterEnv$K) {
      result[["person"]][[index]] = result[["ability"]][[index]] - result[["site"]][[index]]
    }

    P = function(a, b, ability) {
      t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, ability)))
      return (t / (1 + t))
    }
    for(index in 1:.fedirtClusterEnv$K) {
      Xi = apply(.fedirtClusterEnv$my_data[[index]], c(1), sum)
      EXi = apply(t(P(matrix(a),matrix(b), t(result[["ability"]][[index]]))), c(1), sum)
      chaXi = Xi-EXi
      Lz = chaXi / sd(Xi)
      Zh = (Lz-mean(Lz)) / sd(Lz)
      Pij = t(P(matrix(a),matrix(b), t(result[["ability"]][[index]])))
      Xij = .fedirtClusterEnv$my_data[[index]]
      Wij = 1 / Pij / (1-Pij)
      Infit_fz = Wij * (Xij - Pij) * (Xij - Pij)
      Infit = apply(Infit_fz, c(1), sum) / apply(Wij, c(1), sum)
      Outfit = apply((Xij - Pij) * (Xij - Pij), c(1), sum) / .fedirtClusterEnv$J

      fit_result = list()
      fit_result[["Lz"]] = Lz
      fit_result[["Zh"]] = Zh
      fit_result[["Infit"]] = Infit
      fit_result[["Outfit"]] = Outfit
      result[["fit"]][[index]] = fit_result
    }
    return(result)
  }

  fed_irt_entry = function(data) {
    get_new_ps = function(ps_old) {
      optim_result = optim(par = ps_old, fn = logL_entry, gr = g_logL_entry, method = "BFGS",
                           control = list(fnscale=-1, trace = 0, maxit = 10000), hessian = TRUE)
      hessian_adjusted <- -optim_result$hessian
      hessian_inv = solve(hessian_adjusted)

      SE = sqrt(diag(hessian_inv))
      list(result = optim_result, SE = SE)
    }

    ps_init = c(rep(1, .fedirtClusterEnv$J), rep(0, .fedirtClusterEnv$J))
    optim_result = get_new_ps(ps_init)
    ps_next = optim_result$result
    ps_next$loglik = logL_entry(ps_next$par)

    ps_next$b = ps_next$par[(.fedirtClusterEnv$J+1):(.fedirtClusterEnv$J+.fedirtClusterEnv$J)]
    ps_next$a = ps_next$par[1:.fedirtClusterEnv$J]

    ps_next$person = my_person(ps_next[["a"]], ps_next[["b"]])

    ps_next$SE$a = optim_result$SE[1:.fedirtClusterEnv$J]
    ps_next$SE$b = optim_result$SE[(.fedirtClusterEnv$J+1):(.fedirtClusterEnv$J+.fedirtClusterEnv$J)]

    ps_next
  }

  fed_irt_entry(inputdata)
}

