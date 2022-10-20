validate_zenodo_terms <- function (metaterms) {

    meta <- metaterms$metadata
    metaterms$metadata <- NULL

    f <- system.file (file.path ("extdata", "zenodoTerms.csv"),
        package = "deposits"
    )
    these_terms <- utils::read.csv (f)
    for (i in seq_len (ncol (these_terms))) {
        these_terms [, i] <- gsub ("^\\s+|\\s+$", "", these_terms [, i])
    }
    these_terms$metadata <- as.logical (these_terms$metadata)

    these_meta_terms <- these_terms [which (these_terms$metadata), ]
    these_terms <- these_terms [which (!these_terms$metadata), ]

    index <- which (these_meta_terms$term %in% names (meta))
    these_meta_terms <- these_meta_terms [index, ]
    these_terms <-
        these_terms [which (these_terms$term %in% names (metaterms)), ]

    out <- c (
        check_zenodo_terms (these_terms, metaterms),
        check_zenodo_meta_terms (these_meta_terms, meta)
    )

    return (out)
}

#' Check standard zenodo terms - not their "metadata"
#' @noRd
check_zenodo_terms <- function (these_terms, metaterms) {

    out <- NULL

    for (i in seq_len (nrow (these_terms))) {

        term_i <- metaterms [[these_terms$term [i]]]

        if (nzchar (these_terms$vocabulary [i])) {

            out <- c (
                out,
                check_zen_from_vocab (these_terms, i, term_i)
            )

        } else if (these_terms$format [i] == "integer") {

            out <- c (out, meta_validate_term_integer (these_terms, i, term_i))
        }
    }

    return (out)
}

#' Check one zenodo metadata term against vocabulary entry
#' @noRd
check_zen_from_vocab <- function (these_terms, i, term_i) {

    out <- NULL

    voc <- strsplit (these_terms$vocabulary [i], "\\|") [[1]]

    if (!all (term_i %in% voc)) {
        out <- paste0 (
            "Data [",
            these_terms$term [i],
            " = '",
            term_i,
            "'] must follow fixed vocabulary of [",
            paste0 (voc, collapse = ", "),
            "]"
        )
    }

    return (out)
}

#' Check standard zenodo metadata terms
#' @noRd
check_zenodo_meta_terms <- function (these_meta_terms, metaterms) {

    out <- NULL

    for (i in seq_len (nrow (these_meta_terms))) {

        if (grepl ("\\.csv$", these_meta_terms$vocabulary [i])) {

            out <- c (
                out,
                check_zen_meta_from_file (these_meta_terms, metaterms, i)
            )

        } else if (nzchar (these_meta_terms$vocabulary [i])) {

            out <- c (
                out,
                check_zen_meta_from_vocab (these_meta_terms, metaterms, i)
            )

        } else if (these_meta_terms$format [i] == "array") {

            out <- c (
                out,
                check_zen_meta_array (these_meta_terms, metaterms, i)
            )

        } else if (these_meta_terms$term [i] == "language") {

            # internal language vocabulary
            out <- c (
                out,
                check_zen_meta_language (these_meta_terms, metaterms, i)
            )
        }
    }

    return (out)
}

#' Check one zenodo metadata term against vocabulary file
#' @noRd
check_zen_meta_from_file <- function (these_meta_terms, metaterms, i) {

    out <- NULL

    f <- system.file (file.path (
        "extdata",
        these_meta_terms$vocabulary [i]
    ),
    package = "deposits"
    )
    voc <- utils::read.csv (f)
    if (these_meta_terms$term [i] == "license") {
        voc <- c ("cc-zero", "cc-by", voc$id)
    }
    term_i <- metaterms [[these_meta_terms$term [i]]]

    if (these_meta_terms$format [i] == "array") {

        if (!is.list (term_i)) {
            out <- paste0 (
                "Metadata [",
                these_meta_terms$term [i],
                "] must be an array"
            )
        }

    } else if (!term_i %in% voc) {

        out <- paste0 (
            "Metadata [",
            these_meta_terms$term [i],
            " = '",
            term_i,
            "'] not in required vocabulary."
        )
    }

    return (out)
}

#' Check one zenodo metadata term against vocabulary entry
#' @noRd
check_zen_meta_from_vocab <- function (these_meta_terms, metaterms, i) {

    out <- NULL

    values <- strsplit (these_meta_terms$vocabulary [i], "\\|") [[1]]
    term_i <- metaterms [[these_meta_terms$term [i]]]

    if (these_meta_terms$format [i] == "array") {

        term_names <- unique (unlist (lapply (term_i, names)))
        if (!all (term_names %in% values)) {
            out <- paste0 (
                "Metadata [",
                these_meta_terms$term [i],
                "] must be an array/list ",
                "with names in [",
                paste0 (values, collapse = ", "),
                "]"
            )
        }

    } else if (!term_i %in% values) {

        out <- paste0 (
            "Metadata [",
            these_meta_terms$term [i],
            " = '",
            term_i,
            "'] not in required vocabulary of [",
            these_meta_terms$vocabulary [i],
            "]"
        )
    }

    return (out)
}

#' Check one zenodo metadata array term
#' @noRd
check_zen_meta_array <- function (these_meta_terms, metaterms, i) {

    out <- NULL

    term_i <- metaterms [[these_meta_terms$term [i]]]

    if (!is.list (term_i)) {
        out <- paste0 (
            "Metadata [",
            these_meta_terms$term [i],
            "] must be an array/list object"
        )
    }

    return (out)
}

check_zen_meta_language <- function (these_meta_terms, metaterms, i) {

    out <- NULL

    term_i <- metaterms [[these_meta_terms$term [i]]]

    if (!term_i %in% iso_639_2_language_codes () [, 1]) {
        out <- paste0 (
            "Metadata [",
            these_meta_terms$term [i],
            " = '",
            metaterms [[these_meta_terms$term [i]]],
            "'] must be a three-letter ISO-639-2 or ISO-639-3 language identifier."
        )
    }

    return (out)
}
