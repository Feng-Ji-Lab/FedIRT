#' @title Federated IRT SE
#' @description Calculates Standard Error(SE) for FedIRT models.
#' @details Input is the object of fedirt class.
#' @param fedresult fedirt result object
#' @return An array of standard errors for all parameters.
#'
#' @examples
#' # turn input data to a list
#' inputdata = list(as.matrix(example_data_2PL))
#' # Call fedirt() function, and use 2PL model
#' fedresult = fedirt(inputdata, model_name = "2PL")
#' # get SE result
#' SEresult = SE(fedresult)

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim

#' @export
#'
#'
SE = function(fedresult) {
  result = fedresult$SE
  return(result)
}
