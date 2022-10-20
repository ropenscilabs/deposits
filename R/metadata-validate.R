# These are the main entry points to metadata validation routines called
# directly from the main client functions. The `validate_metadata()` function is
# called on the parameter used in client construction, while the
# `validate_terms` function is applied to .

#' validate metadata input to client either as "metadata" parameter, or though
#' `deposit_fill_metadata()` method.
#'
#' @param metadata Metadata as list, filename, or DCEntry object
#'
#' @noRd
validate_metadata <- function (metadata, service = "zenodo") {

    if (service == "zenodo-sandbox") {
        service <- "zenodo"
    }

    metadata <- metadata_to_dcmi (metadata)

    metadata <- httptest2_dcmi_timestamps (metadata)

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
    metaterms <- metadata_dcmi_to_list (metadata, term_map)

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
#' `metadata_dcmi_to_list()`.
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
