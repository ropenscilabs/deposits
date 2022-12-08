# These are the main entry points to metadata validation routines called
# directly from the main client functions. The `validate_metadata()` function is
# called on the parameter used in client construction, while the
# `validate_terms` function is applied to .

#' validate metadata input to client either as "metadata" parameter, or though
#' `deposit_fill_metadata()` method.
#'
#' @param metadata Metadata as list, filename, or DCEntry object
#' @return An `arom4R::DCEntry` metadata object.
#'
#' @noRd
validate_metadata <- function (metadata, service = "zenodo", term_map) {

    if (methods::is (metadata, "character")) {
        metadata <- deposits_meta_from_file (metadata)
    }

    if (service == "zenodo-sandbox") {
        service <- "zenodo"
    }

    if (!any (grepl ("[Cc]reated", names (metadata)))) {
        metadata [dcmi_terms ("created")] <-
            paste0 (strftime (Sys.time (), "%Y-%m-%d"))
    }
    metadata <- httptest2_dcmi_created (metadata)

    # Align all metadata term names with DCMI names:
    nms <- vapply (
        names (metadata),
        function (n) dcmi_terms (n),
        character (1L)
    )
    index <- which (names (metadata) != unname (nms))
    if (length (index) > 0L) {
        msg <- vapply (
            index,
            function (i) paste0 ("   ", names (metadata) [i], " -> ", nms [i], "\n"),
            character (1L)
        )
        message (
            "Names of the following metadata terms have been changed:\n",
            msg
        )
        names (metadata) <- unname (nms)
    }

    if (service == "zenodo") {
        metadata <- construct_md_list_zenodo (metadata, term_map)
    }

    check <- validate_terms (metadata, service = service)

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
        res <- validate_figshare_terms (metaterms) # in metadata-validate-figshare.R
    }

    return (res)
}
