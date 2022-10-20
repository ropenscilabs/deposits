# These are generic checks for different types of terms intended to be
# applicable to all services. Current term types include:
#
# - integer
# - terms which should match a defined vocabulary
# - ISO 639 language codes

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

#' Check one metadata langauge entry against ISO 639-2/3 values
#' @noRd

meta_validate_language_iso639 <- function (these_terms, metaterms, i) {

    out <- NULL

    term_i <- metaterms [[these_terms$term [i]]]

    if (!term_i %in% iso_639_2_language_codes () [, 1]) {
        out <- paste0 (
            "Metadata [",
            these_terms$term [i],
            " = '",
            metaterms [[these_terms$term [i]]],
            "'] must be a three-letter ISO-639-2 or ISO-639-3 language identifier."
        )
    }

    return (out)
}

#' Check one metadata array term
#' @noRd
meta_validate_term_array <- function (these_terms, metaterms, i) {

    out <- NULL

    term_i <- metaterms [[these_terms$term [i]]]

    if (!is.list (term_i)) {
        out <- paste0 (
            "Metadata [",
            these_terms$term [i],
            "] must be an array/list object"
        )
    }

    return (out)
}
