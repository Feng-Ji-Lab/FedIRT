#' @title Federated IRT model
#' @description This function combines all types of algorithm of federated IRT models. It inputs a dataset and return the estimated IRT parameters.
#' @details Input is a list of responding matrices from each school, every responding matrix is one site's data.
#' @param inputdata A list of all responding matrices.
#' @param model_name The name of the model you want to use. Can be "1PL" "2PL" or "graded". "1PL" refers to Rasch Model, "2PL" refers to two-parameter logistic model, "graded" refers to graded model.
#' @param school_effect A bool parameter, TRUE refers to considering the school effect as a fixed effect. Default is FALSE.
#' @param federated The federated learning method. Default is "Avg", meaning using Federated Average. Can also be "Med", meaning Federated Median.
#' @return Corresponding model result as a list.
#'
#' @examples
#' # turn input data to a list
#' inputdata = list(as.matrix(example_data_2PL))
#' # Call fedirt() function, and use 2PL model with school effect as a fixed effect
#' fedresult = fedirt(inputdata, model_name = "2PL",school_effect = TRUE)
#'
#' # turn input data to a list
#' inputdata = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
#' # Call fedirt() function, and use graded model
#' fedresult = fedirt(inputdata, model_name = "graded")
#'

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim

#' @export
#'
fedirt = function(inputdata, model_name = "2PL", school_effect = FALSE, federated = "Avg") {
  valid_models = c("1PL","2PL","graded")
  if (!model_name %in% valid_models) {
    stop("Invalid model_name. Please use one of the following: ", paste(valid_models, collapse = ", "), ".")
  }

  result = list()

  if(model_name == "1PL"){
    result = fedirt_1PL_data(inputdata)
  }
  else if (model_name == "graded"){
    result = fedirt_gpcm_data(inputdata)
  }
  else if(school_effect == TRUE){
    result = fedirt_2PL_schooleffects(inputdata)
  }
  else if(federated == "Med"){
    result = fedirt_2PL_median_data(inputdata)
  }
  else {
    result = fedirt_2PL_data(inputdata)
  }
  class(result) <- "fedirt"
  return(result)
}


#' @title Summary Method for FedIRT Objects
#' @description Provides a summary for objects of class \code{fedirt}.
#' @param object An object of class \code{fedirt}.
#' @param ... Additional arguments passed to the summary method.
#' @method summary fedirt
#' @export
summary.fedirt <- function(object, ...) {
  cat("Summary of FedIRT Results:\n\n")

  cat("\nCounts:\n")
  print(object$counts)

  cat("\nConvergence Status (convergence):\n")
  if (object$convergence == 0) {
    cat("Converged\n")
  } else {
    cat("Not converged\n")
  }


  cat("\nLog Likelihood (loglik):\n")
  print(object$loglik)

  cat("\nDifficulty Parameters (b):\n")
  print(object$b)

  cat("\nDiscrimination Parameters (a):\n")
  print(object$a)

  if (!is.null(object$sc)) {
    cat("\nSchool effect:\n")
    print(object$sc)
  }

  cat("\nAbility Estimates:\n")
  for (i in seq_along(object$person$ability)) {
    cat(paste0("School ", i, ":\n"))
    ability_matrix <- t(object$person$ability[[i]])
    print(as.vector(ability_matrix))
  }

  cat("\nEnd of Summary\n")
}
summary <- function(object, ...) UseMethod("summary")


