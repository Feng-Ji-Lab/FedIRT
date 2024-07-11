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
#' # Call fedirt() function, and use 2PL model
#' fedresult = fedirt(inputdata, model_name = "2PL")
#' personfitResult = personfit(fedresult)

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim

#' @export
#'

personfit = function(fedresult) {
  result = fedresult$person$fit
  class(result) <- "personfit"
  return(result)
}

#' @noRd
#' @title Summary Method for personfit Objects
#' @description Provides a summary for objects of class \code{personfit}.
#' @param object An object of class \code{personfit}.
#' @param ... Additional arguments passed to the summary method.
#' @method summary personfit
#' @export
summary.personfit <- function(object, ...) {
  cat("Summary of FedIRT Person Fit Results:\n")

  cat("\nFit Estimates:\n")
  for (i in seq_along(object)) {
    cat(paste0("School ", i, ":\n"))
    personfitResult <- object[[i]]
    formatResult = cbind(personfitResult$Lz, personfitResult$Zh, personfitResult$Infit, personfitResult$Outfit)
    colnames(formatResult) <- c("Lz", "Zh", "Infit", "Outfit")
    print(formatResult)
  }

  cat("\nEnd of Summary\n")
}
summary <- function(object, ...) UseMethod("summary")
