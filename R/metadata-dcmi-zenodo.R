
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
