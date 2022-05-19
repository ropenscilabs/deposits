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

            out <- c (out, check_fs_meta_integer (fs_terms, i, term_i))

        } else if (grepl ("^(array|list)", fs_terms$format [i])) {

            out <- c (out, check_fs_meta_array (fs_terms, i, term_i))

        } else if (nzchar (fs_terms$vocabulary [i])) {

            out <- c (out, check_fs_meta_from_vocab (fs_terms, i, term_i))
        }
    }

    return (out)
}

#' Check one integer-valued figshare metadata term
#' @noRd
check_fs_meta_integer <- function (fs_terms, i, term_i) {

    out <- NULL

    if (is.na (suppressWarnings (as.integer (term_i)))) {

        out <- paste0 (
            "Data [",
            fs_terms$term [i],
            "] is not coercible to integer."
        )
    }

    return (out)
}

#' Check one figshare metadata term against vocabulary entry
#' @noRd
check_fs_meta_from_vocab <- function (fs_terms, i, term_i) {

    out <- NULL

    voc <- strsplit (fs_terms$vocabulary [i], "\\|") [[1]]
    term_names <- c (
        names (term_i),
        unlist (lapply (term_i, names))
    )

    if (!all (term_names %in% voc)) {
        out <- paste0 (
            "Data [",
            fs_terms$term [i],
            " = '",
            term_i,
            "' must follow fixed vocabulary of [",
            paste0 (voc, collapse = ", "),
            "]"
        )
    }

    return (out)
}

#' Check one figshare metadata array term
#' @noRd
check_fs_meta_array <- function (fs_terms, i, term_i) {

    out <- NULL

    if (!is.list (term_i)) {

        out <- paste0 (
            "Data [",
            fs_terms$term [i],
            "] must have format [",
            fs_terms$format [i],
            "]"
        )

    } else if (nzchar (fs_terms$vocabulary [i])) {

        voc <- strsplit (fs_terms$vocabulary [i], "\\|") [[1]]
        term_names <- c (
            names (term_i),
            unlist (lapply (term_i, names))
        )

        if (!all (term_names %in% voc)) {

            out <- paste0 (
                "Data [",
                fs_terms$term [i],
                " = '",
                term_i,
                "' must follow fixed vocabulary of [",
                paste0 (voc, collapse = ", "),
                "]"
            )
        }
    }

    return (out)
}
