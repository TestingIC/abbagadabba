#' Return taxa data in the region based on the geographical parameters
#'

#' @param taxonKey The index according to rank
#' @param geometry Uses a WKT object, example shows using Polygon object
#' @param limit Number of occurrences returned, limit is 10000
#'
#' @param ... Arguments to be passed to rgbif::occ_data
#'
#' @return Return data frame of species names, basis of record, and the dataset key
#'
#' @examples
#' \dontrun{
#' locationToTaxa(passedGeometry = 'POLYGON((-428.68012 44.00357,-428.58777 44.00357,-428.58777 44.0898,-428.68012 44.0898,-428.68012 44.00357))')
#' locationToTaxa(passedLimit = 5)
#' locationToTaxa(passedTaxonKey = 1, passedLimit=10)
#' }
#' @export

locationToTaxa <- function(passedTaxonKey = NULL, passedGeometry = NULL, passedLimit = 10000, ...) {
    full_data <- rgbif::occ_data(taxonKey = passedTaxonKey, geometry = passedGeometry, limit = passedLimit, ...)

    names <- data.frame(scientific_name = full_data$data$scientificName, basis_of_record = full_data$data$basisOfRecord, dataset_key = full_data$data$datasetKey)

    return(names)
}
