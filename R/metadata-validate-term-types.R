
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
