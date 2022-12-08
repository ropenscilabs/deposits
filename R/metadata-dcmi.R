# Functions to convert metadata inputs into standard DCMI terms

#' Get DCMI elements  from definition file
#'
#' @return An \pkg{xml2} `xml_document` with the DCMI terms schema.
#' @family meta
#' @noRd
dcmi_schema <- function () {

    path <- "extdata/dc/"
    schema <- "dcterms.xsd"
    s <- system.file (file.path (path, schema), package = "deposits")
    if (!file.exists (s)) {
        stop ("Schema file [", s, "] not found")
    }
    xml2::read_xml (s)
}

#' Get names of DCMI terms
#'
#' The Dublin Core Metadata Initiative defines a set of terms at
#' \url{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}.
#'
#' @param term If specified, match term to official DCMI nomenclature, and
#' return single match.
#' @return A character vector of DCMI terms.
#' @family meta
#' @export
dcmi_terms <- function (term = NULL) {

    schema <- dcmi_schema ()
    elements <- xml2::xml_find_all (schema, "xs:element")
    element_names <- xml2::xml_attr (elements, "name")

    if (!is.null (term)) {
        element_names <- grep (term, element_names, value = TRUE, ignore.case = TRUE)
    }

    return (element_names)
}

#' Load metadata term translation table from local inst/extdata
#' @noRd
load_meta_terms <- function () {

    terms <- system.file (file.path ("extdata", "DCTerms.csv"),
        package = "deposits"
    )
    terms <- utils::read.csv (terms)
    for (i in seq_len (ncol (terms))) {
        terms [, i] <- gsub ("^\\s+|\\s+$", "", terms [, i])
    }
    index <- which (nzchar (terms$Zenodo) | nzchar (terms$Figshare))
    return (terms [index, ])
}

#' Constrct map between DCMI terms and those of nominated deposits service.
#' @noRd
get_dcmi_term_map <- function (service = "zenodo") {

    terms <- load_meta_terms ()
    this_col <- grep (service, names (terms), ignore.case = TRUE)
    terms <- terms [which (nzchar (terms [, this_col])), ]
    terms <- cbind (terms$DC, terms [, this_col])
    terms <- apply (terms, 1, function (i) {
        val <- strsplit (i [2], "\\|") [[1]]
        cbind (rep (i [1], length (val)), val)
    })
    terms <- do.call (rbind, terms)
    # zenodo metadata has "(m)" at end of terms
    terms <- data.frame (
        "dcmi" = terms [, 1],
        "service" = terms [, 2],
        "meta" = grepl ("\\(m\\)$", terms [, 2])
    )
    terms [, 2] <- gsub ("\\(m\\)$", "", terms [, 2])

    return (terms)
}
