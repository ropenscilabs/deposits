# ---------------------------------------------------
# Private methods for embargo and publish API methods
# ---------------------------------------------------


#' @description Embargo method for Zenodo service
#'
#' Zenodo has no API method for embargos. The process is entirely specified by
#' metadata fields. This method will also update an embargo date if one has been
#' previously specified.
#'
#' @param embargo_date Date of expiry for embargo.
#' @return Updated client with embargo metadata
#' @noRd

depositsClient$set ("private", "embargo_zenodo", function (embargo_date) {

    s_meta <- private$metadata_service$metadata

    if (!"access_right" %in% names (private$metadata_service)) {

        s_meta$access_right <- "embargoed"
        s_meta$embargo_date <- embargo_date

    } else {

        if (!s_meta$access_right %in% c ("closed", "embargoed")) {
            stop (
                "deposit already has 'access_right' = [",
                s_meta$access_right, "]",
                .call = FALSE
            )
        }
        s_meta$access_right <- "embargoed"
        s_meta$embargo_date <- embargo_date
    }

    private$metadata_service$metadata <- s_meta

    # http methods to update deposit:
    url <- paste0 (get_service_url (self), "/", self$id)

    req <- create_httr2_helper (url, self$headers$Authorization, "PUT")
    req <- httr2::req_body_json (req, data = private$metadata_service)

    resp <- httr2::req_perform (req)

    self$hostdata <- httr2::resp_body_json (resp)

    invisible (self)
})

#' @description Publish method for Zenodo service
#'
#' @return Updated client
#' @noRd

depositsClient$set ("private", "publish_zenodo", function () {

    url <- paste0 (get_service_url (self), "/", self$id, "/publish")
    req <- create_httr2_helper (url, self$headers$Authorization, "POST")
    resp <- httr2::req_perform (req)

    self$hostdata <- httr2::resp_body_json (resp)

    invisible (self)
})

#' @description Embargo method for Figshare service
#'
#' Figshare embargo methods are provided by a distinct "embargo" API endpoint.
#' That has the 3 methods DELETE, GET, and PUT. This method will also update an
#' embargo date if one has been previously specified.
#'
#' @param embargo_date Date of expiry for embargo.
#' @return Updated client with embargo metadata
#' @noRd

depositsClient$set (
    "private", "embargo_figshare",
    function (embargo_date, embargo_type, embargo_reason) {

        url <- paste0 (get_service_url (self), "/", self$id, "/embargo")

        embargo_data <- list (
            is_embargoed = TRUE,
            embargo_date = embargo_date,
            embargo_type = embargo_type,
            embargo_title = paste0 (embargo_type, " under embargo")
        )
        if (!is.null (embargo_reason)) {
            embargo_data$embargo_reason <- embargo_reason
        }

        req <- create_httr2_helper (url, self$headers$Authorization, "PUT")
        req <- httr2::req_body_json (req, data = embargo_data)

        resp <- httr2::req_perform (req)

        self$deposit_retrieve (self$id)

        invisible (self)
    }
)

#' @description Publish method for Figshare service
#'
#' @return Updated client
#' @noRd

depositsClient$set ("private", "publish_figshare", function () {

    url <- paste0 (get_service_url (self), "/", self$id, "/publish")
    req <- create_httr2_helper (url, self$headers$Authorization, "POST")
    resp <- httr2::req_perform (req)

    self$deposit_retrieve (self$id)

    invisible (self)
})
