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
