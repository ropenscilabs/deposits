#' validate metadata terms
#'
#' @param metaterms A list of metadata terms returned from `construct_data_list()`.
#' @param service Name of deposits service.
#' @return `NULL` if all metaterms are valid, otherwise a vector of any invalid
#' metaterms.
#' @noRd
validate_terms <- function (metaterms, service = "zenodo") {

    service <- match.arg (service, c ("figshare", "zenodo"))

    if (service == "zenodo") {
        res <- validate_zenodo_terms (metaterms) # in metadata-validate-zenodo.R
    } else if (service == "figshare") {
        res <- validate_figshare_terms (metaterms)
    }

    return (res)
}
