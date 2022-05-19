#' validate metadata terms
#'
#' @param terms A list of metadata terms returned from `construct_data_list()`.
#' @param deposit Name of deposits service.
#' @return `NULL` if all terms are valid, otherwise a vector of any invalid
#' terms.
#' @noRd
validate_terms <- function (terms, deposit = "zenodo") {

    deposit <- match.arg (deposit, c ("figshare", "zenodo"))

    if (deposit == "zenodo") {
        res <- validate_zenodo_terms (terms)
    } else if (deposit == "figshare") {
        res <- validate_figshare_terms (terms)
    }

    return (res)
}

validate_zenodo_terms <- function (terms) {

    meta <- terms$metadata
    terms$metadata <- NULL

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
    zen_terms <- zen_terms [which (zen_terms$term %in% names (terms)), ]

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

        if (nzchar (zen_terms$vocabulary [i])) {
            values <- strsplit (zen_terms$vocabulary [i], "\\|") [[1]]
            term_i <- terms [[zen_terms$term [i]]]
            if (!term_i %in% values) {
                out <- c (
                    out,
                    paste0 (
                        "Data [",
                        zen_terms$term [i],
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
                        zen_terms$term [i],
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

            out <- c (out, check_zen_meta_array (zen_meta_terms, i))
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
check_zen_meta_array <- function (zen_meta_terms, i) {

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

validate_figshare_terms <- function (terms) {

    f <- system.file (file.path ("extdata", "figshareTerms.csv"),
        package = "deposits"
    )
    fs_terms <- utils::read.csv (f)

    for (i in seq (ncol (fs_terms))) {
        fs_terms [, i] <- gsub ("^\\s+|\\s+$", "", fs_terms [, i])
    }
    fs_terms$metadata <- NULL # zenodo only
    fs_terms <- fs_terms [which (fs_terms$term %in% names (terms)), ]

    out <- NULL

    for (i in seq (nrow (fs_terms))) {

        term_i <- terms [[fs_terms$term [i]]]
        if (fs_terms$format [i] == "integer") {
            if (is.na (suppressWarnings (as.integer (term_i)))) {
                out <- c (
                    out,
                    paste0 (
                        "Data [",
                        fs_terms$term [i],
                        "] is not coercible to integer."
                    )
                )
            }
        } else if (grepl ("^(array|list)", fs_terms$format [i])) {
            if (!is.list (term_i)) {
                out <- c (
                    out,
                    paste0 (
                        "Data [",
                        fs_terms$term [i],
                        "] must have format [",
                        fs_terms$format [i],
                        "]"
                    )
                )
            } else if (nzchar (fs_terms$vocabulary [i])) {
                voc <- strsplit (fs_terms$vocabulary [i], "\\|") [[1]]
                term_names <- c (
                    names (term_i),
                    unlist (lapply (term_i, names))
                )
                if (!all (term_names %in% voc)) {
                    out <- c (
                        out,
                        paste0 (
                            "Data [",
                            fs_terms$term [i],
                            " = '",
                            term_i,
                            "' must follow fixed vocabulary of [",
                            paste0 (voc, collapse = ", "),
                            "]"
                        )
                    )
                }
            }
        } else if (nzchar (fs_terms$vocabulary [i])) {
            voc <- strsplit (fs_terms$vocabulary [i], "\\|") [[1]]
            term_names <- c (
                names (term_i),
                unlist (lapply (term_i, names))
            )
            if (!all (term_names %in% voc)) {
                out <- c (
                    out,
                    paste0 (
                        "Data [",
                        fs_terms$term [i],
                        " = '",
                        term_i,
                        "' must follow fixed vocabulary of [",
                        paste0 (voc, collapse = ", "),
                        "]"
                    )
                )
            }
        }
    }

    return (out)
}
