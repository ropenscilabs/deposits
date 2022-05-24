
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

#' @description Convert DCMI metadata to XML and update with current deposit
#' @noRd

depositsClient$set ("private", "upload_dcmi_xml", function () {

    xml <- as (self$metadata$encode (), "character")

    # httptest2 file name is hashed from md5, so need to have entirely
    # consistent contents for test instances:
    if (Sys.getenv ("DEPOSITS_TEST_ENV") == "true") {

        # gsub timestamps:
        ptn <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}"
        xml <- gsub (ptn, "2022-01-01T00:00:00", xml)
        # and gsub integer dataset id values:
        xml <- gsub ("dataset\\/\\_\\/[0-9]*<", "dataset/_/identifier<", xml)
    }

    f <- file.path (tempdir (), paste0 ("DCEntry-", self$id, ".xml"))
    if (file.exists (f)) {
        file.remove (f)
    }
    writeLines (xml, f)
    self$deposit_upload_file (self$id, f)

    chk <- file.remove (f)

    invisible (self)
})
