#' validate all metadata
#'
#' @noRd
validate_metadata <- function (metadata, service = "zenodo") {

    if (service == "zenodo-sandbox") {
        service <- "zenodo"
    }

    metadata <- process_metadata_param (metadata)

    # Check sanity of XML schema via 'atom4R' routines:
    out <- utils::capture.output (
        chk <- metadata$validate ()
    )
    if (!chk) {
        stop (
            "metadata is not valid - ",
            "see details via metadata$validate()"
        )
    }

    # Then check internal metadata standards
    term_map <- get_dcmi_term_map (service = service)
    metaterms <- construct_metadata_list (metadata, term_map)
    check <- validate_terms (metaterms, service = service)
    if (length (check) > 0L) {
        warning (
            "The following metadata terms do not conform:\n",
            paste0 (check, collapse = "\n")
        )
    }

    return (metadata)
}

#' validate metadata terms
#'
#' @param metaterms A list of metadata terms returned from
#' `construct_metadata_list()`.
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
