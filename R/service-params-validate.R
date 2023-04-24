#' Validate service_parameters
#'
#' @param service_parameters Named list of service_parameters
#' @return Validated version of input parameter.
#' @noRd
validate_service_params <- function (service_parameters) {

    schema <- system.file (fs::path ("extdata", "service-params", "schema.json"),
        package = "deposits"
    )

    f <- fs::file_temp (ext = ".json")
    jsonlite::write_json (service_parameters, f, auto_unbox = TRUE)
    v <- jsonvalidate::json_validate (f, schema, engine = "ajv", verbose = TRUE)
    fs::file_delete (f)

    if (!v) {
        errs <- attr (v, "error")
        nms <- c ("instancePath", "schemaPath", "keyword", "params", "message")
        required <- errs$parentSchema$items$required
        errs <- errs [, nms]
        if (!is.null (required)) {
            required <- vapply (
                required,
                function (i) ifelse (is.null (i), NA_character_, i [1]),
                character (1L)
            )
            errs <- cbind (errs, required)
        }
        print (errs)
        stop (
            "Stopping because the 'service_parameters' terms listed above ",
            "do not conform with the expected schema."
        )
    }

    return (service_parameters)
}
