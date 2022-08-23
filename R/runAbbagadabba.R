#' Run RShiny GUI for Abbagadabba package
#'
#' @name runAbbagadabba
#'
#' @examples
#' \dontrun{
#' abbagadabba::runAbbagadabba()
#' }
#' @export

library('shiny')

runAbbagadabba <- function() {
    shiny::runApp("R/app/")
}


