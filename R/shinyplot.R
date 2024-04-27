#' @title shinyplot
#'
#' @description Calls Project 3 shiny app with descriptive plotting and widgets for MV Classification using RFR.
#'
#' @return  Shiny app
#'
#' @export
#'
#' @examples
#' \dontrun{ shinyplots()}
shinyplots<-function(){
  shiny::runApp(system.file("shiny", package="Project3Caba0009"),launch.browser = TRUE)
}
