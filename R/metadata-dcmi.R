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
#' This function returns the names of those terms currently recognised by the
#' \pkg{atom4R} package.
#'
#' @return A character vector of DCMI terms.
#' @family meta
#' @export
dcmi_terms <- function () {

    schema <- dcmi_schema ()
    elements <- xml2::xml_find_all (schema, "xs:element")
    element_names <- xml2::xml_attr (elements, "name")

    return (element_names)
}

#' Process metadata parameters in one of the three possible forms, returning a
#' 'DCEntry' object.
#' @param metadata Metadata as list, filename, or DCEntry object
#' @return A 'DCEntry' object
#' @noRd
metadata_to_dcmi <- function (metadata) {

    if (is.character (metadata)) {

        checkmate::assert_string (metadata)
        checkmate::assert_file_exists (metadata)
        metadata <- deposits_meta_to_dcmi (metadata)

    } else if (is.list (metadata)) {

        filename <- tempfile (pattern = "meta_", fileext = ".json")
        deposits_metadata_template (filename, metadata)
        metadata <- deposits_meta_to_dcmi (filename)

    } else {

        checkmate::assert_class (
            metadata,
            c ("DCEntry", "AtomEntry", "R6")
        )
    }

    return (metadata)
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

#' Convert metadata of atom4R::DCEntry object into a list of terms
#'
#' @param metadata The 'metadata' object of a 'deposits' client, as an
#' `atom4R::DCEntry` object.
#' @param term_map The 'term_map' object of a 'deposits' client.
#' @return A named list of metadata terms extracted from metadata, and matching
#' the terms identified and permitted in the `term_map`.
#' @noRd
metadata_dcmi_to_list <- function (metadata, term_map) {

    # term_map is constructed so that first DCMI translation is the preferred
    # one, with subsequent ones offering alternative translations
    term_map <- term_map [which (!duplicated (term_map$dcmi)), ]

    values <- lapply (term_map$dcmi, function (i) {
        lapply (metadata [[i]], function (j) {
            j$value
        })
    })
    names (values) <- term_map$service
    values <- values [which (vapply (values, length, integer (1)) > 0L)]
    arrays <- c ("keywords", "contributors")
    index <- which (!names (values) %in% arrays)
    values [index] <- lapply (values [index], function (i) {
        paste0 (i, collapse = ",")
    })

    is_zenodo <- any (term_map$meta)
    if (is_zenodo) {

        values <- construct_md_list_zenodo (values, term_map)

    } else {

        values <- construct_md_list_figshare (values, term_map)
    }

    return (values)
}

#' 'atom4R' has no way of directly listing which 'DCEntry' items have content.
#' This function does that.
#' @noRd
get_dcentry_items <- function (dcmi) {

    dcmi_get_fns <- grep ("^getDC", ls (dcmi), value = TRUE)
    not_these <- c ("getDCElementByValue", "getDCElements")
    dcmi_get_fns <- dcmi_get_fns [which (!dcmi_get_fns %in% not_these)]

    dcmi_has_values <- vapply (
        dcmi_get_fns, function (f) {
            length (dcmi [[f]] ()) > 0L
        },
        logical (1L)
    )

    dcmi_items <- names (dcmi_has_values) [which (dcmi_has_values)]
    dcmi_items <- gsub ("^getDC|s$", "", dcmi_items)

    return (dcmi_items)
}
