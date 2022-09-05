#' Return taxa metadata based on ids
#'
#' @param ids ID or IDs of species to search for
#'
#' @return Return data frame of species ids, organism, create_date, lat_lon, product, organelle, and sequence.
#'
#' @examples
#' \dontrun{
#' getMetadata(2029858529)
#' getMetadata(c(2029858529, 257154225, 257154223, 257154221))
#' }
#' @export

getMetadata <- function(ids) {
    unformated_xml_metadata_list  <- entrez_fetch(db="nucleotide", id = ids, rettype = "gbc", retmode = "xml", parsed=TRUE, retmax = 50)
    xml_metadata_list <- XML::xmlToList(unformated_xml_metadata_list)

    definitions <- c()
    organisms <- c()
    sequences <- c()

    for (element in xml_metadata_list) {
        if (exists("INSDSeq_definition", where=element)) {
            definitions <- c(definitions, element$INSDSeq_definition)
        }
        else {
            definitions <- c(definitions, "No definition")
        }

        if (exists("INSDSeq_organism", where=element)) {
            organisms <- c(organisms, element$INSDSeq_organism)
        }
        else {
            organisms <- c(organisms, "No definition")
        }
    }

    for (loop_id in ids) {
        unformated_xml_fasta_list  <- entrez_fetch(db="nucleotide", id = loop_id, rettype = "fasta", retmode = "xml", parsed = TRUE, retmax = 50)
        xml_fasta_list <- XML::xmlToList(unformated_xml_fasta_list)

        if (xml_fasta_list == "\n\n") {
            sequences <- c(sequences, "Master Record")
        }
        else {
            sequences <- c(sequences, xml_fasta_list$TSeq$TSeq_sequence)
        }
    }

    metadata <- data.frame(ids = ids, definitions = definitions, organisms = organisms, sequences = sequences)
    return(metadata)
}
