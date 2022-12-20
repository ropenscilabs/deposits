
#' validate metadata input to client either as "metadata" parameter, or though
#' `deposit_fill_metadata()` method.
#'
#' This only validates compliance with DCMI terminology, and standardises names
#' of metadata items. DCMI dictates no structural properties of any metadata
#' items, and thus neither does this function.
#'
#' @param metadata Metadata as a list or filename.
#' @return A list of metadata terms, standardised to expected DCMI nomenclature.
#'
#' @noRd
validate_dcmi_metadata <- function (metadata) {

    if (methods::is (metadata, "character")) {
        metadata <- deposits_meta_from_file (metadata)
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

    index <- which (!nzchar (nms)) # invalid term names
    if (length (index) > 0L) {
        warning (
            "The following metadata terms do not conform and will be removed:\n",
            paste0 (names (nms) [index], collapse = "\n")
        )
        metadata <- metadata [-index]
        nms <- nms [-index]
    }

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

    return (metadata)
}

#' Transform and validate DCMI metadata into service-specific form.
#'
#' @param metadata DCMI metadata returned from the preceding
#' `validate_dcmi_metadata()` function.
#' @return A list of metadata terms, standardised to nomenclature expected for
#' the specified service.
#'
#' @noRd
validate_service_metadata <- function (metadata, service) {

    service <- gsub ("\\-sandbox$", "", service)

    term_map <- get_dcmi_term_map (service)

    if (service == "zenodo") {
        metadata_service <- convert_dcmi_to_zenodo (metadata, term_map)
        check <- validate_zenodo_terms (metadata_service)
    } else if (service == "figshare") {
        metadata_service <- convert_dcmi_to_figshare (metadata, term_map)
        check <- validate_figshare_terms (metadata_service)
    }

    return (metadata_service)
}
