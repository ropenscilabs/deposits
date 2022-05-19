#' validate metadata terms
#'
#' @param metaterms A list of metadata terms returned from `construct_data_list()`.
#' @param deposit Name of deposits service.
#' @return `NULL` if all metaterms are valid, otherwise a vector of any invalid
#' metaterms.
#' @noRd
validate_terms <- function (metaterms, deposit = "zenodo") {

    deposit <- match.arg (deposit, c ("figshare", "zenodo"))

    if (deposit == "zenodo") {
        res <- validate_zenodo_terms (metaterms) # in metadata-validate-zenodo.R
    } else if (deposit == "figshare") {
        res <- validate_figshare_terms (metaterms)
    }

    return (res)
}
