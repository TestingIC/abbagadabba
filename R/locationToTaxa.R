#' Return taxa data in the region based on the geographical parameters
#'
#' @param country 2 letter country according to ISO-3166-1
#' @param limit Number of occurrences returned, limit is 500
#' @param stateProvice Next smallest administrative region below country
#' @param scientificName The kingdom of the occurrence
#' @param geometry Uses a WKT object, example shows using Polygon object
#' @param ... Arguments to be passed to rgbif::occ_data
#'
#' @return Return vector of species names
#'
#' @examples
#' \dontrun{
#' locationToTaxa('US',1 , 'Maine','','POLYGON((-428.68012 44.00357,-428.58777 44.00357,-428.58777 44.0898,-428.68012 44.0898,-428.68012 44.00357))')
#' locationToTaxa('US', 1, '', 'Animalia', '')
#' }

locationToTaxa <- function(passedCountry, passedLimit, passedStateProvice, passedScientificname, passedGeometry, ...) {
    full_data <- rgbif::occ_data(country = passedCountry, limit = passedLimit, stateProvince = passedStateProvice, scientificName = passedScientifName, geometry = passedGeometry, ...)
    names <- full_data$data$scientificName

    return(names)
}
