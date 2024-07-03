#' @noRd
#' @title Federated 2PL model
#' @description This function is used to test the accuracy and processing time of this algorithm. It inputs a list of responding matrices and return the federated 2PL parameters.
#' Note: To use federated 2PL in distributed datasets, please use fedirt_2PL().
#' @details Input is a List of responding matrices from each school, every responding matrix is one site's data.
#' @param inputdata A List of all responding matrix.
#' @return A list with the estimated global discrimination a, global difficulty b, person's abilities ability, sites' abilities site, and log-likelihood value loglik.
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

fedirt_2PL_data = function(inputdata) {
  .fedirtClusterEnv$my_data <- inputdata
  N <- lapply(.fedirtClusterEnv$my_data, function(x) nrow(x))
  J <- dim(.fedirtClusterEnv$my_data[[1]])[2]
  K <- length(.fedirtClusterEnv$my_data)

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
  g_logL = function(a, b, index) {
    result_a = .fedirtClusterEnv$da(a, b, index)
    result_b = .fedirtClusterEnv$db(a, b, index)
    list(result_a, result_b)
  }
  logL_entry = function(ps) {
    a = matrix(ps[1:J])
    b = matrix(ps[(J+1):(2*J)])
    result = sum(unlist(map(1:K, function(index) logL(a, b, index))))
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

  my_person = function(a, b) {
    result = list()
    result[["a"]] = a
    result[["b"]] = b
    for(index in 1:K) {
      result[["ability"]][[index]] = matrix(apply(broadcast.multiplication(.fedirtClusterEnv$LA(a,b,index), t(.fedirtClusterEnv$X)), c(1), sum)) / matrix(apply(.fedirtClusterEnv$LA(a,b,index), c(1), sum))
    }

    for(index in 1:K) {
      result[["site"]][[index]] = mean(result[["ability"]][[index]])
    }

    for(index in 1:K) {
      result[["person"]][[index]] = result[["ability"]][[index]] - result[["site"]][[index]]
    }

    P = function(a, b, ability) {
      t = exp(-1 * broadcast.multiplication(a, broadcast.subtraction(b, ability)))
      return (t / (1 + t))
    }
    for(index in 1:K) {
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
      Outfit = apply((Xij - Pij) * (Xij - Pij), c(1), sum) / J

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
      # "Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"
      optim(par = ps_old, fn = logL_entry, gr = g_logL_entry, method = "BFGS", control = list(fnscale=-1, trace = 0,  maxit = 10000))
    }
    ps_init = c(rep(1, J), rep(0, J))
    ps_next = get_new_ps(ps_init)
    ps_next$loglik = logL_entry(ps_next$par)

    ps_next$b = ps_next$par[(J+1):(J+J)]
    ps_next$a = ps_next$par[1:J]

    ps_next$person = my_person(ps_next[["a"]], ps_next[["b"]])
    ps_next
  }


  fed_irt_entry(inputdata)
}

