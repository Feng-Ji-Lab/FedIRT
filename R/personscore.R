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
  result = fedresult$person$ability
  class(result) <- "personscore"
  return(result)
}

#' @noRd
#' @title Summary Method for personscore Objects
#' @description Provides a summary for objects of class \code{personscore}.
#' @param object An object of class \code{personscore}.
#' @param ... Additional arguments passed to the summary method.
#' @method summary personscore
#' @export
summary.personscore <- function(object, ...) {
  cat("Summary of FedIRT Person Score Results:\n")

  cat("\nAbility Estimates:\n")
  for (i in seq_along(object)) {
    cat(paste0("School ", i, ":\n"))
    ability_matrix <- t(object[[i]])
    print(as.vector(ability_matrix))
  }

  cat("\nEnd of Summary\n")
}
summary <- function(object, ...) UseMethod("summary")
