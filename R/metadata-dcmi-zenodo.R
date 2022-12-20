#' Convert standard DCMI metadata list to form and content required for upload
#' to zenodo API.
#'
#' Zenodo has it's own "metadata" list item. The main thing this function does
#' is to move appropriate terms within the initially flat 'dcmi' list into
#' the sub-component of "metadata" within the main metadata.
#'
#' @param dcmi DCMI-compliant metadata returned from `validate_dcmi_metadata()`
#' function.
#' @param term_map The term map for the 'zenodo' deposits service.
#' @return A potentially modified version of `dcmi`, with structures of
#' individual items rectified to expected forms, and any otherwise missing yet
#' required fields inserted with default values.
#' @noRd

convert_dcmi_to_zenodo <- function (dcmi, term_map) {

    index <- which (names (dcmi) %in%
        term_map$service [which (term_map$meta)])
    meta_values <- dcmi [index]
    values <- dcmi [-index]

    req <- list (
        "upload_type" = "other",
        "title" = "Title",
        "creators" = "A. Person",
        "description" = "Description"
    )
    if ("abstract" %in% names (values) && !"description" %in% names (values)) {
        req$description <- values$abstract
        values$abstract <- NULL
    }

    index <- which (!names (req) %in% names (meta_values))
    meta_values <- c (meta_values, req [index])

    # grab any corresponding entries from 'values':
    move_one <- function (ptn, val_list) {
        i <- grep (ptn, names (val_list$values))
        if (length (i) == 1L) {
            val_list$meta_values [[grep (ptn, names (val_list$meta_values))]] <-
                val_list$values [[i]]
            val_list$values <- val_list$values [-i]
        }
        return (val_list)
    }
    val_list <- move_one ("^[Cc]reator",
        list (meta_values = meta_values, values = values))
    val_list <- move_one ("^[Tt]itle", val_list)
    val_list <- move_one ("^[Dd]escr", val_list)
    val_list <- move_one ("^[Uu]pload", val_list)

    meta_values <- val_list$meta_values
    values <- val_list$values

    if (!is.list (meta_values$creators)) {
        meta_values$creators <- list (list (name = meta_values$creators))
    } else if (is.null (names (meta_values$creators))) {
        meta_values$creators <- lapply (meta_values$creators, function (i) {
            if (!is.list (i)) {
                i <- list (name = i)
            }
            return (i)
        })
    } else if (names (meta_values$creators) == "name") {
        meta_values$creators <- list (meta_values$creators)
    }

    if ("upload_type" %in% names (meta_values)) {
        meta_values$upload_type <- tolower (meta_values$upload_type)
    }

    values$metadata <- meta_values

    if (!"created" %in% names (values)) {
        values <- c (
            "created" = paste0 (strftime (Sys.time (), "%Y-%m-%d")),
            values
        )
    }

    # Finally, move any 'meta' terms in term_map from 'values' to 'meta_values':
    # index <- which (names (values) %in% term_map$dcmi [term_map$meta])
    # if (length (index) > 0) {
    #     service_name <-
    #         term_map$service [match (names (values) [index], term_map$dcmi)]
    #     for (i in seq_along (index)) {
    #         values$metadata [[service_name [i]]] <- values [[index [i]]]
    #         values <- values [-index [i]]
    #     }
    # }

    values <- httptest2_dcmi_created (values)

    return (values)
}
