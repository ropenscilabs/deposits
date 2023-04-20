# Functions to insert deposits "metadata" field in DCMI-compliant form into host
# metadata fields, including private client methods.

#' Method to convert client "metadata" (DCMI) field into one metadata parameter
#' on host service.
#'
#' This is called on outgoing methods of deposit initiation and update.
#' @noRd
depositsClient$set ("private", "dcmi2host", function () {

    meta_json <- jsonlite::toJSON (self$metadata, auto_unbox = TRUE)

    if (self$service == "zenodo") {

        notes <- paste0 (
            private$metadata_service$metadata$notes,
            "\n---start-deposits-meta---\n",
            paste0 (meta_json),
            "\n---end-deposits-meta---\n"
        )
        private$metadata_service$metadata$notes <- notes
    }

    invisible (self)
})

#' Method to convert "metadata" (DCMI) field embedded into one host metadata
#' parameter back into internal client "metadata" structure.
#'
#' This is called on incoming method of deposit retrieval, and only actually
#' executed if the client has no local metadata.
#' @noRd
depositsClient$set ("private", "host2dcmi", function () {

    if (!is.null (self$metadata)) {
        return (invisible (self))
    }

    if (self$service == "zenodo") {

        notes <- strsplit (self$hostdata$metadata$notes, "\\n") [[1]]
        ptn <- "^\\-{3}start\\-deposits\\-meta\\-{3}$"
        i <- grep (ptn, notes)
        j <- grep (gsub ("start", "end", ptn), notes)
        if (length (i) == 1L && length (j) == 1L) {
            index <- seq (i + 1, j - 1)
            self$metadata <-
                jsonlite::fromJSON (notes [index], simplifyVector = FALSE)
        }
    }

    invisible (self)
})
