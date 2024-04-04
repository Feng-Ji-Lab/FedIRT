#' @title Server for Federated IRT Model Estimation
#' @note This shiny app should be used together with client version.
#'
#' @description Launches a server that handles federated learning across multiple schools or institutions for the estimation of Item Response Theory (IRT) model parameters. This server facilitates communication between the central aggregator and distributed data sources, coordinating the data sharing process while maintaining privacy.
#'
#' @details The server establishes a federated learning environment where each participating entity (school) computes parts of the model locally. The server then collects summary statistics from each entity and uses them to update the global model parameters. It features a user interface for initiating the estimation process and for displaying the results of the federated learning procedure. The user interface provides real-time information about the connected schools, data consistency checks, and the mode of the IRT model being estimated (binary or graded).
#'
#' Function 'updateM' checks for consistency in the number of maximum item levels across all schools, setting a flag to indicate whether a binary or graded model should be used. Function 'check_J' ensures that all schools have a consistent number of items in their datasets. The 'ui' function serves as the user interface for the server, while 'getLocalIP' retrieves the server's IP address for connections. Finally, the 'server' function contains the logic for receiving data from schools, triggering the estimation process, and sending the results back to participating schools.
#'
#' Overall, the 'runserver' function orchestrates the federated IRT model estimation process by combining local computations from schools, managing data traffic, executing the appropriate estimation function, and providing users with an interactive web interface.
#'
#' The web interface is built using Shiny, allowing users to check connection statuses, start the estimation process, and view results. It supports both GET and POST HTTP methods for handling data exchange with clients. The server is designed to be flexible and can be adapted for various federated learning scenarios in the education sector.
#'
#' @return No return value, called for side effects (initiates interactive Shiny server session) and display estimates on the interface.
#'

#' @importFrom purrr map
#' @importFrom pracma quadl
#' @importFrom stats optim
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel mainPanel textInput actionButton verbatimTextOutput fileInput dataTableOutput plotOutput reactiveValues renderPrint
#' @importFrom httr POST content
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal labs
#' @importFrom shinyjs useShinyjs
#' @importFrom callr r_bg r
#' @importFrom shiny runApp
#' @importFrom DT datatable
#'
#'
#' @export




runserver = function(){

  appDir <- system.file("shiny", package = "FedIRT")
  serverAppFile <- file.path(appDir, "server.R")

  if (file.exists(serverAppFile)) {
    shiny::runApp(serverAppFile)
  } else {
    stop("Unable to launch the server app - file server.R not found in the FedIRT package")
  }
}
