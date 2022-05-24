#'
validate_zenodo_terms <- function (metaterms) {

    meta <- metaterms$metadata
    metaterms$metadata <- NULL

    f <- system.file (file.path ("extdata", "zenodoTerms.csv"),
        package = "deposits"
    )
    zen_terms <- utils::read.csv (f)
    for (i in seq (ncol (zen_terms))) {
        zen_terms [, i] <- gsub ("^\\s+|\\s+$", "", zen_terms [, i])
    }
    zen_terms$metadata <- as.logical (zen_terms$metadata)

    zen_meta_terms <- zen_terms [which (zen_terms$metadata), ]
    zen_terms <- zen_terms [which (!zen_terms$metadata), ]

    index <- which (zen_meta_terms$term %in% names (meta))
    zen_meta_terms <- zen_meta_terms [index, ]
    zen_terms <- zen_terms [which (zen_terms$term %in% names (metaterms)), ]

    out <- c (
        check_zenodo_terms (zen_terms),
        check_zenodo_meta_terms (zen_meta_terms, meta)
    )

    return (out)
}

#' Check standard zenodo terms - not their "metadata"
#' @noRd
check_zenodo_terms <- function (zen_terms) {

    out <- NULL

    for (i in seq (nrow (zen_terms))) {

        term_i <- zen_terms$term [i]

        if (nzchar (zen_terms$vocabulary [i])) {

            values <- strsplit (zen_terms$vocabulary [i], "\\|") [[1]]
            if (!term_i %in% values) {
                out <- c (
                    out,
                    paste0 (
                        "Data [",
                        term_i,
                        " = '",
                        term_i,
                        "'] not in required vocabulary of [",
                        zen_terms$vocabulary [i],
                        "]"
                    )
                )
            }
        } else if (zen_terms$format [i] == "integer") {

            if (suppressWarnings (is.na (as.integer (term_i)))) {
                out <- c (
                    out,
                    paste0 (
                        "Data [",
                        term_i,
                        "] must be an integer."
                    )
                )
            }
        }
    }

    return (out)
}

#' Check standard zenodo metadata terms
#' @noRd
check_zenodo_meta_terms <- function (zen_meta_terms, meta) {

    out <- NULL

    for (i in seq (nrow (zen_meta_terms))) {

        if (grepl ("\\.csv$", zen_meta_terms$vocabulary [i])) {

            out <- c (out, check_zen_meta_from_file (zen_meta_terms, meta, i))

        } else if (nzchar (zen_meta_terms$vocabulary [i])) {

            out <- c (out, check_zen_meta_from_vocab (zen_meta_terms, meta, i))

        } else if (zen_meta_terms$format [i] == "array") {

            out <- c (out, check_zen_meta_array (zen_meta_terms, meta, i))
        }
    }

    return (out)
}

#' Check one zenodo metadata term against vocabulary file
#' @noRd
check_zen_meta_from_file <- function (zen_meta_terms, meta, i) {

    out <- NULL

    f <- system.file (file.path (
        "extdata",
        zen_meta_terms$vocabulary [i]
    ),
    package = "deposits"
    )
    voc <- utils::read.csv (f)
    if (zen_meta_terms$term [i] == "license") {
        voc <- c ("cc-zero", "cc-by", voc$id)
    }
    term_i <- meta [[zen_meta_terms$term [i]]]

    if (zen_meta_terms$format [i] == "array") {

        if (!is.list (term_i)) {
            out <- paste0 (
                "Metadata [",
                zen_meta_terms$term [i],
                "] must be an array"
            )
        }

    } else if (!term_i %in% voc) {

        out <- paste0 (
            "Metadata [",
            zen_meta_terms$term [i],
            " = '",
            term_i,
            "'] not in required vocabulary."
        )
    }

    return (out)
}

#' Check one zenodo metadata term against vocabulary entry
#' @noRd
check_zen_meta_from_vocab <- function (zen_meta_terms, meta, i) {

    out <- NULL

    values <- strsplit (zen_meta_terms$vocabulary [i], "\\|") [[1]]
    term_i <- meta [[zen_meta_terms$term [i]]]

    if (zen_meta_terms$format [i] == "array") {

        term_names <- unique (unlist (lapply (term_i, names)))
        if (!all (term_names %in% values)) {
            out <- paste0 (
                "Metadata [",
                zen_meta_terms$term [i],
                "] must be an array/list ",
                "with names in [",
                paste0 (values, collapse = ", "),
                "]"
            )
        }

    } else if (!term_i %in% values) {

        out <- paste0 (
            "Metadata [",
            zen_meta_terms$term [i],
            " = '",
            term_i,
            "'] not in required vocabulary of [",
            zen_meta_terms$vocabulary [i],
            "]"
        )
    }

    return (out)
}

#' Check one zenodo metadata array term
#' @noRd
check_zen_meta_array <- function (zen_meta_terms, meta, i) {

    out <- NULL

    term_i <- meta [[zen_meta_terms$term [i]]]

    if (!is.list (term_i)) {
        out <- paste0 (
            "Metadata [",
            zen_meta_terms$term [i],
            "] must be an array/list object"
        )
    }

    return (out)
}
