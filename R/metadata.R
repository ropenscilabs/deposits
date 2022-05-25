
#' Process metadata parameters in one of the three possible forms, returning a
#' 'DCEntry' object.
#' @param metadata Metadata as list, filename, or DCEntry object
#' @return A 'DCEntry' object
#' @noRd
process_metadata_param <- function (metadata) {

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

#' Get names of DCMI terms
#'
#' The Dublin Core Metadata Initiative defines a set of terms at
#' \url{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}.
#' This function returns the names of those terms currently recognised by the
#' \pkg{atom4R} package.
#'
#' @return A character vector of DCMI terms.
#' @export
dcmi_terms <- function () {

    terms <- ls (envir = asNamespace ("atom4R"), pattern = "^DC")
    not_meta <- c ("DCElement", "DCEntry", "DCMIVocabulary")
    terms <- terms [which (!terms %in% not_meta)]

    return (gsub ("^DC", "", terms))
}


#' Load metadata term translation table from local inst/extdata
#' @noRd
load_meta_terms <- function () {

    terms <- system.file (file.path ("extdata", "DCTerms.csv"),
        package = "deposits"
    )
    terms <- utils::read.csv (terms)
    for (i in seq (ncol (terms))) {
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
#' @param metadata The 'metadata' object of a 'deposits' client.
#' @param term_map The 'term_map' object of a 'deposits' client.
#' @noRd
construct_data_list <- function (metadata, term_map) {

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

        index <- which (names (values) %in%
            term_map$service [which (term_map$meta)])
        meta_values <- values [index]
        values <- values [-index]

        req <- list (
            "upload_type" = "other",
            "title" = "Title",
            "creators" = "A. Person",
            "description" = "Description"
        )

        index <- which (!names (req) %in% names (meta_values))
        meta_values <- c (meta_values, req [index])

        if (!is.list (meta_values$creators)) {
            meta_values$creators <- list (list (name = meta_values$creators))
        } else if (names (meta_values$creators) == "name") {
            meta_values$creators <- list (meta_values$creators)
        }
        if ("upload_type" %in% names (meta_values)) {
            meta_values$upload_type <- tolower (meta_values$upload_type)
        }

        values$metadata <- meta_values

        if (!"created" %in% names (values)) {
            values <- c ("created" = paste0 (Sys.Date ()), values)
        }

    } else {

        if ("authors" %in% names (values) & !is.list (values$authors)) {
            values$authors <- list (list (name = values$authors))
        }
        if ("categories" %in% names (values) &
            !is.integer (values$categories)) {
            message (
                "Figshare categories must be integer values; ",
                "the provided values will be removed."
            )
            values$categories <- NULL
        }
        if ("timeline" %in% names (values)) {
            # figshare timeline allows only:
            # [firstOnline, publisherPublication, publisherAcceptance]
            # For demonstration purposes, only use firstOneline for now
            values$timeline <- list (firstOnline = values$timeline [1])
        }
        if ("license" %in% names (values)) {
            if (is.na (suppressWarnings (as.integer (values$license)))) {
                warning (
                    "Figshare licenses must be integer-valued; ",
                    "the value will be reset to '1' = 'CC-BY'"
                )
                values$license <- 1L
            }
        }
    }

    return (values)
}
