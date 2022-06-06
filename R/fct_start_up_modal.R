#' start_up_modal 
#'
#' @description A function to generate the modal dialog box shown on app start or when the home button is pressed
#'
#' @return Start-up modal dialog box
#'
#' @noRd

start_up_modal <- function(){
  shiny::showModal(shiny::div(id = "start_modal", shiny::modalDialog(
    title = "Welcome to the GPAD Cards!",
    shiny::h4("Description of this resource"),
    "The Global Physical Activity Dataset (GPAD) cards have been created to allow researchers to identify 
            datasets that have measured physical activity using an accelerometric device and collected a range of health outcomes. 
            The cards were initially informed by a scoping review. ",
    shiny::h4("Contributing"),
    "If you have any questions, have found a bug, believe some of the data needs correcting or have any suggestions
            for features or enhancements you would like to see; please visit our Github page", shiny::a(href = "https://github.com/lboro-climb/gpad.cards/issues", "https://github.com/lboro-climb/gpad.cards/issues"),
    shiny::h4("Version control"),
    "Version control information can be found on our Github page", shiny::a(href = "https://github.com/lboro-climb/gpad.cards", "https://github.com/lboro-climb/gpad.cards"), "The current version of the cards is 0.0.9",
    shiny::h6("To return to this page, click the", icon("home"),"in the top left corner of the screen."),
    easyClose = TRUE,
    footer = "Click anywhere outside this message to close it"
  )))
}