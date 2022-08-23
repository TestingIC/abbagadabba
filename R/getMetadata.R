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
    if (any(!is.na(ids))) {
        unformated_metadata <- entrez_fetch(db="nucleotide", id=ids, rettype = "gbc", retmode = "xml", parsed=TRUE)
        xml_metadata <- XML::xmlToList(unformated_metadata)

        accession_version <- c()
        organism <- c()
        create_date <- c()
        pubmed <- c()
        sequence <- c()

        gene <- c()
        product <- c()
        organelle <- c()
        note <- c()
        lat_lon <- c()
        country <- c()
        collection_date <- c()
        collected_by <- c()
        specimen_voucher <- c()

        for (filler in 1:length(ids)) {
            gene <- append(gene, NA)
            product <- append(product, NA)
            organelle <- append(organelle, NA)
            note <- append(note, NA)
            lat_lon <- append(lat_lon, NA)
            country <- append(country, NA)
            collection_date <- append(collection_date, NA)
            collected_by <- append(collected_by, NA)
            specimen_voucher <- append(specimen_voucher, NA)
        }

        index <- 1
        for (metadata_table in xml_metadata) {
            accession_version <- append(accession_version, metadata_table$`INSDSeq_accession-version`)
            organism <- append(organism, metadata_table$INSDSeq_organism)
            create_date <- append(create_date, metadata_table$`INSDSeq_create-date`)
            if (length(metadata_table$INSDSeq_references$INSDReference$INSDReference_pubmed) != 0) {
                pubmed <- append(pubmed, metadata_table$INSDSeq_references$INSDReference$INSDReference_pubmed)
            }
            else {
                pubmed <- append(pubmed, NA)
            }
            if (length(metadata_table$INSDSeq_sequence) != 0) {
                sequence <- append(sequence, metadata_table$INSDSeq_sequence)
            }
            else {
                sequence <- append(sequence, NA)
            }
            #sequence <- append(sequence, metadata_table$INSDSeq_sequence)

            for (feature in metadata_table$`INSDSeq_feature-table`) {
                if (feature$INSDFeature_key == "source") {
                    for (element in feature$INSDFeature_quals) {
                        if (element$INSDQualifier_name == "organelle") {
                            organelle[index] <- element$INSDQualifier_value
                        }
                        if (element$INSDQualifier_name == "lat_lon") {
                            lat_lon[index] <- element$INSDQualifier_value
                        }
                        if (element$INSDQualifier_name == "country") {
                            country[index] <- element$INSDQualifier_value
                        }
                        if (element$INSDQualifier_name == "collection_date") {
                            collection_date[index] <- element$INSDQualifier_value
                        }
                        if (element$INSDQualifier_name == "collected_by") {
                            collected_by[index] <- element$INSDQualifier_value
                        }
                        if (element$INSDQualifier_name == "specimen_voucher") {
                            specimen_voucher[index] <- element$INSDQualifier_value
                        }
                    }
                }
                if (feature$INSDFeature_key == "gene") {
                    for (element in feature$INSDFeature_quals) {
                        if (element$INSDQualifier_name == "gene") {
                            if (!is.na(gene[index])) {
                                gene[index] <- "More than one gene is present!"
                            }
                            else {
                                gene[index] <- element$INSDQualifier_value
                            }
                        }
                    }
                }
                if (feature$INSDFeature_key == "CDS") {
                    for (element in feature$INSDFeature_quals) {
                        if (element$INSDQualifier_name == "product") {
                            if (!is.na(product[index])) {
                                product[index] <- "More than one product is present!"
                            }
                            else {
                                product[index] <- element$INSDQualifier_value
                            }
                        }
                    }
                }
                if (feature$INSDFeature_key == "misc_feature") {
                    for (element in feature$INSDFeature_quals) {
                        if (element$INSDQualifier_name == "note") {
                            if (!is.na(note[index])) {
                                note[index] <- "More than one note is present!"
                            }
                            else {
                                note[index] <- element$INSDQualifier_value
                            }
                        }
                    }
                }
            }
            index <- index + 1
        }

        metadata_dataframe <- data.frame(id = ids, accession_version = accession_version, organism = organism, create_date = create_date, pubmed = pubmed, gene = gene, product = product, organelle = organelle, note = note, lat_lon = lat_lon, country = country, collection_date = collection_date, collected_by = collected_by, specimen_voucher = specimen_voucher, sequence = sequence)
        return(metadata_dataframe)
    }
}
