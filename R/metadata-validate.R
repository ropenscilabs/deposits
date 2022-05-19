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
        res <- validate_zenodo_terms (terms) # in metadata-validate-zenodo.R
    } else if (deposit == "figshare") {
        res <- validate_figshare_terms (terms)
    }

    return (res)
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
