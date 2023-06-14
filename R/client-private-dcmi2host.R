# Service-specific functions to insert deposits "metadata" field in
# DCMI-compliant form into host metadata fields, including private client
# methods. See #65.

#' Method to convert client "metadata" (DCMI) field into one metadata parameter
#' on host service.
#'
#' This is called on outgoing methods of deposit initiation and update.
#' @noRd
depositsClient$set ("private", "dcmi2host", function () {

    meta_json <- jsonlite::toJSON (self$metadata, auto_unbox = TRUE)
    meta_json <- paste0 (
        "\\n\\n---start-deposits-meta---\\n",
        paste0 (meta_json),
        "\\n---end-deposits-meta---\\n"
    )

    if (self$service == "zenodo") {

        private$metadata_service$metadata$notes <- paste0 (
            private$metadata_service$metadata$notes,
            meta_json
        )

    } else if (self$service == "figshare") {

        # Figshare should ideally go into "custom_fields", or
        # "custom_fields_list", but those are currently only used to auto-fill
        # from institutional-level settings, and can't be used for indivudual
        # deposits.

        private$metadata_service$description <- paste0 (
            private$metadata_service$description,
            meta_json
        )
    }

    invisible (self)
})

#' Method to convert "metadata" (DCMI) field embedded into one host metadata
#' parameter back into internal client "metadata" structure.
#'
#' This is called on incoming method of deposit retrieval, and only actually
#' executed if the client has no local metadata.
#' @noRd
depositsClient$set ("private", "host2dcmi_internal", function () {

    if (!is.null (self$metadata)) {
        return (invisible (self))
    }

    if (self$service == "zenodo") {
        field <- self$hostdata$metadata$notes
    } else if (self$service == "figshare") {
        field <- self$hostdata$description
    }

    if (length (field) == 0L) {
        return (invisible (self))
    }

    ptn <- "^\\-{3}start\\-deposits\\-meta\\-{3}$"
    if (!grepl (ptn, field)) {
        return (invisible (self))
    }

    # Figshare does not render "\n", only "\\n", and some of these double
    # backslashes end up repeated and need to be reduced here for JSON parsing.
    field <- condense_linebreaks (field)
    field <- strsplit (field, "\n") [[1]]
    i <- grep (ptn, field)
    j <- grep (gsub ("start", "end", ptn), field)

    if (length (i) == 1L && length (j) == 1L) {
        index <- seq (i + 1, j - 1)
        metadata <- paste0 (field [index], collapse = "")
        self$metadata <- jsonlite::fromJSON (metadata, simplifyVector = FALSE)
    }

    invisible (self)
})

#' Remove the metadata produced in 'dcmi2host'.
#'
#' This is called as soon as a 'datapackage.json' file is uploaded. From that
#' point on, metadata are stored and read from there, so no longer need to be
#' stored in the host metadata field.
#' @noRd
depositsClient$set ("private", "remove_dcmi2host", function () {

    metadata <- validate_metadata (
        self$metadata,
        gsub ("\\-sandbox$", "", self$service)
    )
    metadata <- httptest2_created_timestamp (metadata)
    self$metadata <- metadata$dcmi
    private$metadata_service <- metadata$service

    # That service data will then *not* have the 'dcmi2host' field inserted, so
    # can be used to update directly. The "local_path" variable has to be
    # temporarily removed here to prevent update from erroring because
    # additional local files do not exist on remote deposit.
    local_path <- self$local_path
    self$local_path <- NULL
    self$deposit_update ()
    self$local_path <- local_path

    invisible (self)
})
