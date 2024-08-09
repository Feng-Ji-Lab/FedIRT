#' @title Federated IRT model
#' @description This function combines all types of algorithm of federated IRT models. It inputs a dataframe and return the estimated IRT parameters.
#' @details Input is a dataframe from each school with a column indicating the school name.
#' @param inputdata A dataframe.
#' @param model_name The name of the model you want to use. Can be "1PL" "2PL" or "graded". "1PL" refers to Rasch Model, "2PL" refers to two-parameter logistic model, "graded" refers to graded model.
#' @param school_effect A bool parameter, TRUE refers to considering the school effect as a fixed effect. Default is FALSE.
#' @param federated The federated learning method. Default is "Avg", meaning using Federated Average. Can also be "Med", meaning Federated Median.
#' @param colname Column name indicating the school.
#' @return Corresponding model result as a list.
#'
#' @examples
#' \dontrun{
#' data <- read.csv("dataset.csv", header = TRUE)
#' fedresult <- fedirt_file(data, model_name = "2PL")
#' }

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim
#' @importFrom stats optimHess

#' @export
#'
fedirt_file = function(inputdata, model_name = "2PL", school_effect = FALSE, federated = "Avg", colname = "site") {

  data_list <- split(inputdata[, -1], inputdata[[colname]])
  inputdata <- lapply(data_list, as.matrix)
  fedirt(inputdata,model_name,school_effect,federated)
}
