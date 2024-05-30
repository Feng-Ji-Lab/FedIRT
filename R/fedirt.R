#' @title Federated IRT model
#' @description This function combines all types of algorithm of federated IRT models. It inputs a dataset and return the federated IRT parameters.
#' Note: This function is used for one combined dataset.
#' @details Input is a List of responding matrices from each school, every responding matrix is one site's data.
#' @param inputdata A List of all responding matrices.
#' @param model_name The name of the model you want to use. Can be "1PL" "2PL" or "graded". "1PL" refers to Rasch Model, "2PL" refers to two-parameter logistic model, "graded" refers to graded model. 
#' @param school_effect A bool parameter, TRUE refers to considering the school effect as a fixed effect. Default is FALSE.
#' @param federated The federated learning method. Default is "Avg", meaning using Federated Average. Can also be "Med", meaning Federated Median.
#' @return Corresponding model result as a list.
#'
#' @examples
#' inputdata = list(as.matrix(example_data_2PL))
#' fedresult = fedirt(inputdata, model_name = "2PL",school_effect = TRUE)
#'
#' inputdata = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
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

  if(model_name == "1PL"){
    return(fedirt_1PL_data(inputdata))
  }
  else if (model_name == "graded"){
    return(fedirt_gpcm_data(inputdata))
  }
  else if(school_effect == TRUE){
    return(fedirt_2PL_schooleffects(inputdata))
  }
  else if(federated == "Med"){
    return(fedirt_2PL_median_data(inputdata))
  }
  else {
    return(fedirt_2PL_data(inputdata))
  }
}
