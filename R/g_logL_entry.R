
#' @title Aggregated Gradient of Log-Likelihood for Federated Learning
#'
#' @description Calculates the sum of the gradients of the log-likelihood with respect to item discrimination (a) and difficulty (b) parameters across all schools participating in a federated learning process. The function `g_logL_entry` is a critical component in the gradient-based optimization process within `fedirt`.
#'
#' @details The function aggregates the gradients computed locally at each school. The cumulative gradient is then used in the optimization algorithm to update the model parameters. Each school should implement the function `get_g_logL_from_index` which computes the gradients of log-likelihood locally. This function needs to be aligned with the federated learning framework, typically involving network communication to retrieve the gradient information.
#'
#' In simplified scenarios, or during initial testing and development, users can substitute the network communication with a direct call to a local `g_logL` function that computes the gradient of log-likelihood.
#'
#' @param ps A numeric vector including the model's current estimates for the item parameters, organized consecutively with discrimination parameters followed by difficulty parameters.
#'
#' @return A matrix where the first half of rows corresponds to the aggregated gradient with respect to item discrimination parameters and the second half corresponds to the aggregated gradient with respect to item difficulty parameters.
#'
#' @export

g_logL_entry = function(ps) {
  a = matrix(ps[1:J])
  b = matrix(ps[(J+1):(2*J)])
  ga = matrix(0, nrow = J)
  gb = matrix(0, nrow = J)
  # print(ga)
  # print(gb)
  for(index in 1:K) {
    result = get_g_logL_from_index(ps, index)
    ga[, 1] = ga[, 1] + result[1:J]
    gb[, 1] = gb[, 1] + result[(J+1):(2*J)]
  }
  # print(rbind(ga,gb))
  rbind(ga, gb)
}
