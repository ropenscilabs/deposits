
#' Check one integer-valued metadata term
#' @noRd

meta_validate_term_integer <- function (these_terms, i, term_i) {

    out <- NULL

    if (is.na (suppressWarnings (as.integer (term_i)))) {

        out <- paste0 (
            "Data [",
            these_terms$term [i],
            "] is not coercible to integer."
        )
    }

    return (out)
}

#' Check one zenodo metadata term against vocabulary entry
#' @noRd
meta_validate_term_from_vocab <- function (these_terms, i, term_i) {

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
