validate_zenodo_terms <- function (metaterms) {

    meta <- metaterms$metadata
    metaterms$metadata <- NULL

    f <- system.file (file.path ("extdata", "zenodoTerms.csv"),
        package = "deposits"
    )
    term_def <- utils::read.csv (f) # metadata term definitions
    for (i in seq_len (ncol (term_def))) {
        term_def [, i] <- gsub ("^\\s+|\\s+$", "", term_def [, i])
    }
    term_def$metadata <- as.logical (term_def$metadata)

    meta_term_def <- term_def [which (term_def$metadata), ]
    term_def <- term_def [which (!term_def$metadata), ]

    index <- which (meta_term_def$term %in% names (meta))
    meta_term_def <- meta_term_def [index, ]
    term_def <-
        term_def [which (term_def$term %in% names (metaterms)), ]

    out <- c (
        check_zenodo_terms (term_def, metaterms),
        check_zenodo_meta_terms (meta_term_def, meta)
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
                meta_validate_term_from_vocab (these_terms, i, term_i)
            )

        } else if (these_terms$format [i] == "integer") {

            out <- c (
                out,
                meta_validate_term_integer (these_terms, i, term_i)
            )
        }
    }

    return (out)
}

#' Check standard zenodo metadata terms
#' @noRd
check_zenodo_meta_terms <- function (meta_term_def, metaterms) {

    out <- NULL

    for (i in seq_len (nrow (meta_term_def))) {

        if (grepl ("\\.csv$", meta_term_def$vocabulary [i])) {

            out <- c (
                out,
                check_zen_meta_from_file (meta_term_def, metaterms, i)
            )

        } else if (nzchar (meta_term_def$vocabulary [i])) {

            out <- c (
                out,
                check_zen_meta_from_vocab (meta_term_def, metaterms, i)
            )

        } else if (meta_term_def$format [i] == "array") {

            out <- c (
                out,
                meta_validate_term_array (meta_term_def, metaterms, i)
            )

        } else if (meta_term_def$term [i] == "language") {

            # internal language vocabulary
            out <- c (
                out,
                meta_validate_language_iso639 (meta_term_def, metaterms, i)
            )
        }
    }

    return (out)
}

#' Check one zenodo metadata term against vocabulary file
#' @noRd
check_zen_meta_from_file <- function (meta_term_def, metaterms, i) {

    out <- NULL

    f <- system.file (
        file.path (
            "extdata",
            meta_term_def$vocabulary [i]
        ),
        package = "deposits"
    )
    voc <- utils::read.csv (f)
    if (meta_term_def$term [i] == "license") {
        meta_term_def$vocabulary <-
            paste0 (c ("cc-zero", "cc-by", voc$id), collapse = "|")
    }
    term_i <- metaterms [[meta_term_def$term [i]]]

    if (meta_term_def$format [i] == "array") {

        out <- meta_validate_term_array (meta_term_def, metaterms, i)

    } else {

        out <- meta_validate_term_from_vocab (meta_term_def, i, term_i)
    }

    return (out)
}

#' Check one zenodo metadata term against vocabulary entry
#'
#' This includes one zenodo-specific validation check, because zenodo has array
#' terms with specific vocabularies. This can be translated to a generic check
#' if any other services also have this requirement.
#' @noRd
check_zen_meta_from_vocab <- function (meta_term_def, metaterms, i) {

    out <- NULL

    term_i <- metaterms [[meta_term_def$term [i]]]

    if (meta_term_def$format [i] == "array") {

        voc <- strsplit (meta_term_def$vocabulary [i], "\\|") [[1]]
        term_names <- unique (unlist (lapply (term_i, names)))

        if (!all (term_names %in% voc)) {
            out <- paste0 (
                "Metadata [",
                meta_term_def$term [i],
                "] must be an array/list ",
                "with names in [",
                paste0 (voc, collapse = ", "),
                "]"
            )
        }

    } else {

        out <- meta_validate_term_from_vocab (meta_term_def, i, term_i)
    }

    return (out)
}
