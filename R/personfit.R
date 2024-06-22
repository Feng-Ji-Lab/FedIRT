#' @title Federated IRT person fit
#' @description personfit calculates the Zh values, infit and outfit statistics.
#' The returned object is a list.
#' @details Input is the object of fedirt class.
#' @param fedresult fedirt result object
#' @return a list of person fit in each school.
#'
#' @examples
#' # turn input data to a list
#' inputdata = list(as.matrix(example_data_2PL))
#' # Call fedirt() function, and use 2PL model with school effect as a fixed effect
#' fedresult = fedirt(inputdata, model_name = "2PL",school_effect = TRUE)
#' personfitResult = personfit(fedresult)

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim

#' @export
#'

personfit = function(fedresult) {
  return(fedresult$person$fit)
}
