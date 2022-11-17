#' Add additional metadata list-item values required by zenodo.
#'
#' Zenodo has it's own "metadata" list item. The main thing this function does
#' is to move appropriate terms within the initially flat 'metadata' list into
#' the sub-component of "metadata" within the main metadata.
#'
#' @param values Initial metadata list
#' @param term_map The term map for the 'zenodo' deposits service.
#' @return A potentially modified version of `values`, with structures of
#' individual items rectified to expected forms, and any otherwise missing yet
#' required fields inserted with default values.
#' @noRd

construct_md_list_zenodo <- function (values, term_map) {

    index <- which (names (values) %in%
        term_map$service [which (term_map$meta)])
    meta_values <- values [index]
    values <- values [-index]

    req <- list (
        "upload_type" = "other",
        "title" = "Title",
        "creators" = "A. Person",
        "description" = "Description"
    )

    index <- which (!names (req) %in% names (meta_values))
    meta_values <- c (meta_values, req [index])

    if (!is.list (meta_values$creators)) {
        meta_values$creators <- list (list (name = meta_values$creators))
    } else if (names (meta_values$creators) == "name") {
        meta_values$creators <- list (meta_values$creators)
    }
    if ("upload_type" %in% names (meta_values)) {
        meta_values$upload_type <- tolower (meta_values$upload_type)
    }

    values$metadata <- meta_values

    if (!"created" %in% names (values)) {
        values <- c ("created" = paste0 (Sys.Date ()), values)
    }

    values <- httptest2_dcmi_created (values)

    return (values)
}
