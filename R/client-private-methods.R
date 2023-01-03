
#' @description Define service for deposits client
#' @param service (character) of a deposits service (see
#' \link{deposits_services}).
#' @param sandbox If `TRUE`, connect client to sandbox, rather than
#' actual API endpoint (for "zenodo" only).
#' @noRd

depositsClient$set (
    "private", "define_service",
    function (service, sandbox = FALSE) {

        service <- match.arg (tolower (service), c ("zenodo", "figshare"))
        checkmate::assert_logical (sandbox, len = 1L)

        if (sandbox && service == "zenodo") {
            service <- "zenodo-sandbox"
        }
        self$sandbox <- sandbox

        s <- deposits_services ()
        self$service <- service
        self$url_base <- s$api_base_url [s$name == service]

        if (self$service == "zenodo-sandbox") {
            self$service <- "zenodo"
        }
        self$term_map <- get_dcmi_term_map (self$service)

        invisible (self)
    }
)

#' @description Fill client 'id' and 'url_service' values from
#' 'hostdata'
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
        self$url_service <- self$hostdata$links$html
    }

    invisible (self)
})

#' @description Extract list of all current deposits and store as
#' 'deposits' member element.
#' @noRd

depositsClient$set ("private", "deposits_list_extract", function () {

    url <- paste0 (
        self$url_base,
        ifelse (self$service == "figshare",
            "account/articles",
            "deposit/depositions?size=1000"
        )
    )

    req <- create_httr2_helper (url, self$headers$Authorization, "GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    self$deposits <- httr2::resp_body_json (resp, simplifyVector = TRUE)

    invisible (self)
})

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

#' @description Perform actual upload of local file.
#' @noRd

depositsClient$set ("private", "upload_local_file", function (deposit_id, path) {

    url <- get_service_url (self)

    if (self$service == "figshare") {

        # in R/upload-figshare.R, which returns updated hostdata
        self$hostdata <- upload_figshare_file (
            deposit_id,
            url,
            self$headers,
            path
        )

    } else if (self$service == "zenodo") {

        # in R/upload-zenodo.R, which returns data on file upload only
        res <- upload_zenodo_file (
            deposit_id,
            url,
            self$headers,
            path
        )

        self <- self$deposit_retrieve (deposit_id)
    }

    invisible (self)
})
