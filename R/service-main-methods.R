# service-specific methods called directly from 'client-main.R'

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
