#' A wrapper function to run Shiny Apps from \code{CameraTrapDetectoR}.
#' 
#' Running this function will launch a shiny application to deploy object detection models
#'  on camera trap images
#' @param app The name of the app you want to run. The options are currently 
#' `deploy`.
#' @return interactive interface
#' 
#' @import shiny
#' @import shinyFiles
#' @import shinyBS
#' @rawNamespace import(shinyjs, except = runExample)
#' 
#' @export

runShiny <- function(){
  
  # find and launch the app
  appDir <- fs::path_package("shiny", package = "CameraTrapDetectoR")
  
  shiny::runApp(appDir, display.mode = "normal")
}