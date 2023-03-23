#' Validate metadata and convert to service-specific form
#'
#' This function is a wrapper used to call the following two functions in
#' sequence.
#'
#' @param metadata Metadata as list or filename.
#' @param service Name of deposits service
#' @return A list of two elements: 'dcmi' holding validated DCMI metadata, and
#' 'service' holding the validated, service-specific translation of 'dcmi'
#' @noRd
validate_metadata <- function (metadata, service) {

    metadata_dcmi <- validate_dcmi_metadata (metadata)
    metadata_service <-
        translate_dc_to_service (metadata_dcmi, service = service)

    return (list (
        dcmi = metadata_dcmi,
        service = metadata_service
    ))
}

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

    # Align all metadata term names with DCMI names:
    nms <- vapply (
        names (metadata),
        function (n) dcmi_terms (term = n),
        character (1L)
    )

    index <- which (!nzchar (nms)) # invalid term names
    if (length (index) > 0L) {
        warning (
            "The following metadata terms do not conform ",
            "and will be removed:\n",
            paste0 (names (nms) [index], collapse = "\n"),
            call. = FALSE
        )
        metadata <- metadata [-index]
        nms <- nms [-index]
    }

    index <- which (names (metadata) != unname (nms))
    if (length (index) > 0L) {
        msg <- vapply (
            index,
            function (i) {
                paste0 ("   ", names (metadata) [i], " -> ", nms [i], "\n")
            },
            character (1L)
        )
        message (
            "Names of the following metadata terms have been changed:\n",
            msg
        )
        names (metadata) <- unname (nms)
    }

    metadata <- metadata [order (names (metadata))]

    schema <- system.file (fs::path ("extdata", "dc", "schema.json"),
        package = "deposits"
    )

    f <- fs::file_temp (ext = ".json")
    jsonlite::write_json (metadata, f, auto_unbox = TRUE)
    v <- jsonvalidate::json_validate (f, schema, engine = "ajv", verbose = TRUE)

    if (!v) {
        errs <- attr (v, "error")
        nms <- c ("instancePath", "schemaPath", "keyword", "params", "message")
        required <- errs$parentSchema$items$required
        required <- vapply (
            required,
            function (i) ifelse (is.null (i), NA_character_, i [1]),
            character (1L)
        )
        errs <- cbind (errs [, nms], required)
        print (errs)
        stop (
            "Stopping because the DCMI metadata terms listed above ",
            "do not confirm with the expected schema."
        )
    }

    return (metadata)
}

#' Validate service-specific metadata
#'
#' The validation is performed via JSON schemas included in the 'inst/extdata'
#' directory of this package, one for each deposits service. These schemas
#' specify names and details of all expected metadata terms for each service.
#'
#' @param metadata Service-specific metadata
#' @return Results of `jsonvalidate::json_validate`.
#'
#' @noRd
validate_service_metadata <- function (metadata, service) {

    schema <- system.file (fs::path ("extdata", service, "schema.json"),
        package = "deposits"
    )

    f <- fs::file_temp (ext = ".json")
    jsonlite::write_json (metadata, f, auto_unbox = TRUE)
    res <-
        jsonvalidate::json_validate (f, schema, engine = "ajv", verbose = TRUE)

    return (res)
}
