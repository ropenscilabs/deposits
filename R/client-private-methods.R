
#' @description Fill client 'id' and 'url_deposit' values from
#' 'hostdata'
#' @noRd

depositsClient$set ("private", "fill_deposit_id_url", function () {

    if (self$service == "figshare") {
        # entity_id is filled on creation, but retrieval returns 'id'
        self$id <- self$hostdata$entity_id
        if (is.null (self$id)) {
            self$id <- self$hostdata$id
        }
        self$url_deposit <-
            paste0 (
                "https://figshare.com/account/articles/",
                self$id
            )
    } else if (self$service == "zenodo") {
        self$id <- self$hostdata$id
        self$url_deposit <- self$hostdata$links$html
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
