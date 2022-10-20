validate_figshare_terms <- function (metaterms) {

    f <- system.file (file.path ("extdata", "figshareTerms.csv"),
        package = "deposits"
    )
    meta_term_def <- utils::read.csv (f)

    for (i in seq_len (ncol (meta_term_def))) {
        meta_term_def [, i] <-
            gsub ("^\\s+|\\s+$", "", meta_term_def [, i])
    }
    meta_term_def$metadata <- NULL # zenodo only
    meta_term_def <- meta_term_def [
        which (meta_term_def$term %in% names (metaterms)),
    ]

    out <- c (
        # no terms here just metaterms:
        # check_fs_terms (these_terms, metaterms),
        check_fs_meta_terms (meta_term_def, metaterms)
    )

    return (out)
}

#' Check standard fileshare metadata terms
#' @param meta Not used here, but kept for consistency with
#' 'check_zenodo_meta_terms()'.
#' @noRd
check_fs_meta_terms <- function (meta_term_def, metaterms) {

    out <- NULL

    for (i in seq_len (nrow (meta_term_def))) {

        term_i <- metaterms [[meta_term_def$term [i]]]

        if (meta_term_def$format [i] == "integer") {

            out <- c (
                out,
                meta_validate_term_integer (meta_term_def, i, term_i)
            )

        } else if (grepl ("^(array|list)", meta_term_def$format [i])) {

            out <- c (
                out,
                check_fs_meta_array (meta_term_def, i, term_i)
            )

        } else if (nzchar (meta_term_def$vocabulary [i])) {

            out <- c (
                out,
                meta_validate_term_from_vocab (meta_term_def, i, term_i)
            )
        }
    }

    return (out)
}

#' Check one figshare metadata array term. This is a service-specific function,
#' because figshare has various kinds of arrays with different formats.
#' @noRd
check_fs_meta_array <- function (meta_term_def, i, term_i) {

    out <- NULL

    if (!is.list (term_i)) {

        out <- paste0 (
            "Data [",
            meta_term_def$term [i],
            "] must have format [",
            meta_term_def$format [i],
            "]"
        )

    } else if (nzchar (meta_term_def$vocabulary [i])) {

        term_names <- c (
            names (term_i),
            unlist (lapply (term_i, names))
        )
        out <- meta_validate_term_from_vocab (meta_term_def, i, term_names)
    }

    return (out)
}
