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
        term_map$dcmi [which (term_map$meta)])
    meta_values <- dcmi [index]
    values <- dcmi [-index]

    # Zenodo only allows 'description', not 'abstract':
    if ("abstract" %in% names (meta_values) &&
        "description" %in% names (meta_values)) {

        meta_values$description <- paste0 (
            "Abstract: \n",
            meta_values$abstract,
            "\nDescription: \n",
            meta_values$description
        )
        meta_values$abstract <- NULL

    } else if ("abstract" %in% names (meta_values)) {

        meta_values$description <- meta_values$abstract
        meta_values$abstract <- NULL
    }

    val_list <- mv_zen_values_to_metavalues (values, meta_values)
    meta_values <- val_list$meta_values
    values <- val_list$values

    meta_values <- match_meta_names_to_zen_api (meta_values, term_map)
    meta_values <- convert_zen_meta_vals_to_lists (meta_values)
    if ("upload_type" %in% names (meta_values)) {
        meta_values$upload_type <- tolower (meta_values$upload_type)
    }
    meta_values <- insert_zen_required_meta_values (meta_values)

    meta_values <- meta_values [order (names (meta_values))]
    values$metadata <- meta_values

    if (!"created" %in% names (values)) {
        values <- c (
            "created" = paste0 (strftime (Sys.time (), "%Y-%m-%d")),
            values
        )
    }

    values <- values [order (names (values))]

    values <- httptest2_dcmi_created (values)

    return (values)
}

#' Move any terms defined in 'values' into zenodo 'metavalues' list
#'
#' @noRd
mv_zen_values_to_metavalues <- function (values, meta_values) {

    move_one <- function (ptn, val_list) {
        i <- grep (ptn, names (val_list$values))
        if (length (i) == 1L) {
            val_list$meta_values [[grep (ptn, names (val_list$meta_values))]] <-
                val_list$values [[i]]
            val_list$values <- val_list$values [-i]
        }
        return (val_list)
    }

    val_list <- move_one (
        "^[Cc]reator",
        list (meta_values = meta_values, values = values)
    )
    val_list <- move_one ("^[Tt]itle", val_list)
    val_list <- move_one ("^[Dd]escr", val_list)
    val_list <- move_one ("^[Uu]pload", val_list)

    return (val_list)
}

match_meta_names_to_zen_api <- function (meta_values, term_map) {

    index <- which (!names (meta_values) %in% term_map$service)
    index_not_dcmi <-
        which (!all (names (meta_values) [index] %in% term_map$dcmi))

    if (all (names (meta_values) [index] %in% term_map$dcmi)) {
        names (meta_values) [index] <- term_map$service [
            match (names (meta_values [index]), term_map$dcmi)
        ]
    }

    if (length (index_not_dcmi) > 0L) {
        stop (
            "metadata field names [",
            paste0 (names (meta_values) [index_not_dcmi], collapse = ", "),
            "] are not valid; see 'dcmi_terms()' for valid names",
            call. = FALSE
        )

        names (meta_values) [index] <- vapply (index, function (i) {

            index_service <-
                which (term_map$dcmi == names (meta_values) [i])

            if (length (index_service) > 1L) {

                index_service_fields <-
                    unlist (lapply (meta_values [[i]], function (j) {

                        ret <- NA_integer_
                        g <- regexpr ("^\\[.*[^\\]\\]", j)
                        if (g > 0L) {
                            which_field <-
                                gsub ("\\[|\\]", "", regmatches (j, g))
                            which_field <-
                                tolower (gsub ("\\s+", "_", which_field))
                            if (!which_field %in% term_map$service) {
                                stop (
                                    "field name [",
                                    which_field,
                                    "] not recognised.",
                                    call. = FALSE
                                )
                            }
                            ret <- which (term_map$service == which_field)
                        }
                        return (ret)
                    }))

                index_service_fields <- unique (index_service_fields [
                    which (!is.na (index_service_fields))
                ])
                if (length (index_service_fields) == 1L) {
                    index_service <- index_service_fields
                } else {
                    index_service <- index_service [1]
                }
            }

            term_map$service [index_service]

        }, character (1L))

        meta_values <- lapply (meta_values, function (i) {
            lapply (i, function (j) {
                gsub ("^\\[.*[^\\]\\](\\s+?)", "", j)
            })
        })
    }

    return (meta_values)
}

convert_zen_meta_vals_to_lists <- function (meta_values) {

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

    return (meta_values)
}

insert_zen_required_meta_values <- function (meta_values) {

    req <- list (
        "upload_type" = "other",
        "title" = "Title",
        "creators" = "A. Person",
        "description" = "Description"
    )
    index <- which (!names (req) %in% names (meta_values))
    meta_values <- c (meta_values, req [index])

    return (meta_values)
}
