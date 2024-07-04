#' @title Federated IRT person score
#' @description This function calculates persons' ability.
#' @details Input is the object of fedirt class.
#' @param fedresult fedirt result object
#' @return a list of person score in each school.
#'
#' @examples
#' # turn input data to a list
#' inputdata = list(as.matrix(example_data_2PL))
#' # Call fedirt() function, and use 2PL model
#' fedresult = fedirt(inputdata, model_name = "2PL")
#' personscoreResult = personscore(fedresult)

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim

#' @export
#'

personscore = function(fedresult) {
  return(fedresult$person$ability)
}
