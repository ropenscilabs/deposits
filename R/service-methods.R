# service-specific methods called directly from public or private methods.

# -------------------------------------------------------
# ---------------   FROM PUBLIC METHODS   ---------------
# ---------------    in client-main.R     ---------------
# -------------------------------------------------------

#' Set the 'prereserve_doi' flag in metadata_service for zenodo.
#'
#' This is only called from `deposit_new()`.
#' @param prereserve_doi Passed directly from `deposit_new()` method.
#' @noRd
depositsClient$set (
    "private", "set_doi_prereserve_meta_flag",
    function (prereserve_doi) {

        if (gsub ("\\-sandbox$", "", self$service) == "zenodo") {
            private$metadata_service$metadata$prereserve_doi <-
                prereserve_doi
        }

        invisible (self)
    }
)

#' Fill hostdata for a new deposit.
#'
#' This is called from `deposit_new()`, with exact procedures depending on the
#' service.
#' @param hostdata Retrurned from API on construction of new deposit. This may
#' or not hold actual deposit data, depending on service.
#' @param prereserve_doi Passed directly from `deposit_new()` method.
#' @noRd
depositsClient$set (
    "private", "fill_deposit_new_hostdata",
    function (hostdata, prereserve_doi) {

        if (self$service == "figshare") {
            if (prereserve_doi) {
                doi <- private$prereserve_doi (hostdata$entity_id)
            }
            self$deposit_retrieve (hostdata$entity_id)
        } else if (self$service == "zenodo") {
            self$hostdata <- hostdata
        }

        invisible (self)
    }
)

#' Unlock a deposit for editing - Zenodo-only
#'
#' Only called in public deposit_update() method.
#' @noRd
depositsClient$set (
    "private", "unlock_deposit_for_editing",
    function (deposit_id) {

        if (gsub ("\\-sandbox$", "", self$service) == "zenodo" &&
            !is.null (self$hostdata)) {
            if (self$hostdata$state == "done") {

                url <- get_service_url (self, deposit_id = deposit_id)
                url <- paste0 (url, "/actions/edit")
                req <- create_httr2_helper (
                    url,
                    self$headers$Authorization,
                    "POST"
                )
                resp <- httr2::req_perform (req)
                httr2::resp_check_status (resp)
                message (
                    "Previously published deposit [",
                    deposit_id,
                    "] unlocked for editing."
                )
            }
        }

        invisible (self)
    }
)

#' @description Remove 'hostdata' and 'metadata' items after call to
#' `deposit_delete()` method (if they correspond to `self$id`).
#' @noRd

depositsClient$set ("private", "rm_host_meta_data", function () {

    if (self$service == "figshare") {

        if (!(is.null (self$id) & is.null (self$hostdata))) {
            if (!is.null (self$hostdata$entity_id)) {
                if (self$hostdata$entity_id == self$id |
                    self$hostdata$id == self$id) {
                    self$hostdata <- self$metadata <- NULL
                }
            }
        }

    } else if (self$service == "zenodo") {

        if (!(is.null (self$id) & is.null (self$hostdata))) {
            if (!is.null (self$hostdata$id)) {
                if (self$hostdata$id == self$id) {
                    self$hostdata <- self$metadata <- NULL
                }
            }
        }
    }

    invisible (self)
})

#' Fill the 'url_service' field of client from 'hostdata'
#'
#' This is called from `deposit_new()` and `deposit_retrieve()` methods.
#' @noRd

depositsClient$set ("private", "fill_service_id_url", function () {

    if (self$service == "figshare") {
        # entity_id is filled on creation, but retrieval returns 'id'
        self$id <- self$hostdata$entity_id
        if (is.null (self$id)) {
            self$id <- self$hostdata$id
        }
        self$url_service <-
            paste0 (
                "https://figshare.com/account/articles/",
                self$id
            )
    } else if (self$service == "zenodo") {
        self$id <- self$hostdata$id
        links <- self$hostdata$links
        self$url_service <- ifelse (
            "latest_html" %in% names (links),
            links$latest_html, links$html
        )

        # If id is a new version, update url_service to edit-mode interface for
        # new version:
        latest_id <- as.integer (fs::path_file (self$url_service))
        if (latest_id != self$id) {
            self$url_service <-
                gsub ("\\/record\\/", "/deposit/", self$url_service)
            self$url_service <- gsub (latest_id, self$id, self$url_service)
        }
    }

    invisible (self)
})

# -------------------------------------------------------
# --------------   FROM PRIVATE METHODS   ---------------
# ---------    in client-private-methods.R     ----------
# -------------------------------------------------------

#' @description Extract integer IDs of all current deposits.
#'
#' This is called from private 'servicedata_from_dp' method.
#'
#' Returned values are currently identical for all services, but this function
#' allows any differences in new systems to be immediately implemented.
#' @return Vector of integer IDs (if any; otherwise NULL).
#' @noRd

depositsClient$set ("private", "get_deposits_ids", function () {

    deps <- self$deposits
    if (length (deps) == 0L) {
        return (NULL)
    }

    ids <- NULL
    if (self$service == "figshare") {
        ids <- deps$id
    } else if (self$service == "zenodo") {
        ids <- deps$id
    }

    return (ids)
})
