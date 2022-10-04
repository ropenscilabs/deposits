validate_figshare_terms <- function (metaterms) {

    f <- system.file (file.path ("extdata", "figshareTerms.csv"),
        package = "deposits"
    )
    these_meta_terms <- utils::read.csv (f)

    for (i in seq_len (ncol (these_meta_terms))) {
        these_meta_terms [, i] <- gsub ("^\\s+|\\s+$", "", these_meta_terms [, i])
    }
    these_meta_terms$metadata <- NULL # zenodo only
    these_meta_terms <-
        these_meta_terms [which (these_meta_terms$term %in% names (metaterms)), ]

    out <- c (
        # no terms here just metaterms:
        # check_fs_terms (these_terms, metaterms),
        check_fs_meta_terms (these_meta_terms, metaterms)
    )

    return (out)
}

#' Check standard fileshare metadata terms
#' @param meta Not used here, but kept for consistency with
#' 'check_zenodo_meta_terms()'.
#' @noRd
check_fs_meta_terms <- function (these_meta_terms, metaterms) {

    out <- NULL

    for (i in seq_len (nrow (these_meta_terms))) {

        term_i <- metaterms [[these_meta_terms$term [i]]]

        if (these_meta_terms$format [i] == "integer") {

            out <- c (out, check_fs_meta_integer (these_meta_terms, i, term_i))

        } else if (grepl ("^(array|list)", these_meta_terms$format [i])) {

            out <- c (out, check_fs_meta_array (these_meta_terms, i, term_i))

        } else if (nzchar (these_meta_terms$vocabulary [i])) {

            out <- c (out, check_fs_meta_from_vocab (these_meta_terms, i, term_i))
        }
    }

    return (out)
}

#' Check one integer-valued figshare metadata term
#' @noRd
check_fs_meta_integer <- function (these_meta_terms, i, term_i) {

    out <- NULL

    if (is.na (suppressWarnings (as.integer (term_i)))) {

        out <- paste0 (
            "Data [",
            these_meta_terms$term [i],
            "] is not coercible to integer."
        )
    }

    return (out)
}

#' Check one figshare metadata term against vocabulary entry
#' @noRd
check_fs_meta_from_vocab <- function (these_meta_terms, i, term_i) {

    out <- NULL

    voc <- strsplit (these_meta_terms$vocabulary [i], "\\|") [[1]]

    if (!all (term_i %in% voc)) {
        out <- paste0 (
            "Data [",
            these_meta_terms$term [i],
            " = '",
            term_i,
            "'] must follow fixed vocabulary of [",
            paste0 (voc, collapse = ", "),
            "]"
        )
    }

    return (out)
}

#' Check one figshare metadata array term
#' @noRd
check_fs_meta_array <- function (these_meta_terms, i, term_i) {

    out <- NULL

    if (!is.list (term_i)) {

        out <- paste0 (
            "Data [",
            these_meta_terms$term [i],
            "] must have format [",
            these_meta_terms$format [i],
            "]"
        )

    } else if (nzchar (these_meta_terms$vocabulary [i])) {

        voc <- strsplit (these_meta_terms$vocabulary [i], "\\|") [[1]]
        term_names <- c (
            names (term_i),
            unlist (lapply (term_i, names))
        )

        if (!all (term_names %in% voc)) {

            out <- paste0 (
                "Data [",
                these_meta_terms$term [i],
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
