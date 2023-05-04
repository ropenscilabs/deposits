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

        service <- add_service_sandbox (service, sandbox)
        self$sandbox <- sandbox

        s <- deposits_services ()
        self$service <- service
        self$url_base <- s$api_base_url [s$name == service]
        self$service <- rm_service_sandbox (service)

        private$term_map <- get_dcmi_term_map (self$service)

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

#' @description Extract integer IDs of all current deposits.
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

depositsClient$set (
    "private", "upload_local_file",
    function (path, overwrite, compress) {

        name_field <- private$get_file_name_field ()
        if (md5sums_are_same (path, self$hostdata, name_field, quiet = FALSE)) {
            return (invisible (self))
        }

        url <- get_service_url (self)

        if (compress != "no") {
            path_old <- path
            path <- compress_local_file (path, compress)
        }

        if (self$service == "figshare") {

            current_files <- self$hostdata$files$name
            if (fs::path_file (path) %in% current_files) {
                if (!overwrite) {
                    stop (
                        "File [", fs::path_file (path), "] already ",
                        "exists on deposit [", self$id,
                        "] and overwrite is set to 'FALSE'",
                        call. = FALSE
                    )
                } else {
                    # Figshare simply duplicates files by default, so have to
                    # remove previous one:
                    figshare_delete_file (
                        self$id,
                        get_service_url (self),
                        self$hostdata$files,
                        self$headers,
                        path
                    )
                }
            }

            # in R/upload-figshare.R, which returns updated hostdata
            self$hostdata <- upload_figshare_file (
                self$id,
                url,
                self$headers,
                path
            )

        } else if (self$service == "zenodo") {

            # in R/upload-zenodo.R, which returns data on file upload only
            res <- upload_zenodo_file (
                self$id,
                url,
                self$headers,
                path
            )

            frictionless <- self$frictionless

            self <- self$deposit_retrieve (self$id)

            initial_upload <- all (
                self$hostdata$file$filename %in% fs::path_file (path)
            )
            if (initial_upload && frictionless && !self$frictionless) {
                self$frictionless <- TRUE
            }
        }

        if (compress != "no") {
            fs::file_delete (path)
        }

        invisible (self)
    }
)

compress_local_file <- function (path, compress) {

    if (compress == "tar") {
        file_ext <- ".tar.gz"
        fn <- tar
    } else if (compress == "zip") {
        file_ext <- ".zip"
        fn <- zip
    }

    binfile <- fs::path_ext_set (path, file_ext)
    if (fs::file_exists (binfile)) {
        message (
            "File [",
            binfile,
            "] already exists; will not be re-created."
        )
    } else {
        do.call (fn, list (binfile, files = path))
    }

    return (binfile)
}

#' @description Get the name of the "files" part of hostdata which contains the
#' actual names of the files.
#' @noRd
depositsClient$set ("private", "get_file_name_field", function () {

    if (self$service == "figshare") {
        ret <- "name"
    } else if (self$service == "zenodo") {
        ret <- "filename"
    }

    return (ret)
})

#' @description Get list of files from local or remote hostdata
#'
#' @param filename Name of file to be extracted. This is only used to check
#' whether that file is already listed on local hostdata. If not, full file
#' list is downloaded from remote service.
#' @return Full data on all files associated with nominated 'deposit_id',
#' including 'filename'.
#' @noRd

depositsClient$set (
    "private", "get_hostdata_files",
    function (deposit_id, filename) {

        url <- get_service_url (self, deposit_id = deposit_id)

        name_field <- private$get_file_name_field ()

        if (filename %in% self$hostdata$files [[name_field]]) {

            files <- self$hostdata$files

        } else {

            req <- create_httr2_helper (
                url,
                self$headers$Authorization,
                "GET"
            )
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            hostdata <- httr2::resp_body_json (resp, simplifyVector = TRUE)
            files <- hostdata$files
        }

        return (files)
    }
)

#' @description Delete files from remote service.
#'
#' This is called from the 'deposit_delete_file' method.
#' @param filename Name of file to be deleted as recorded on service.
#' @param deposit_id The 'id' number of deposit from which file is to be
#' deleted.
#' @return Updated host data.
#' @noRd

depositsClient$set ("private", "delete_file", function (filename) {

    if (self$service == "figshare") {

        figshare_delete_file (
            self$id,
            get_service_url (self),
            self$hostdata$files,
            self$headers,
            filename
        )

    } else if (self$service == "zenodo") {

        zenodo_delete_file (
            self$id,
            get_service_url (self),
            self$hostdata$files,
            self$headers,
            filename
        )
    }

    self$deposit_retrieve (self$id)

    invisible (self)
})

#' @description Call API methods to pre-reserve DOI
#'
#' Currently only needed for Figshare.
#' @param id ID of deposit. Needed because this is not necessarily part of
#' 'self' when this method is called on deposit_new().
#' @return The pre-reserved DOI
#' @noRd

depositsClient$set ("private", "prereserve_doi", function (id) {

    if (self$service != "figshare" || is_deposits_test_env ()) {
        return (NULL)
    }

    url <- paste0 (get_service_url (self), "/", id, "/reserve_doi")
    req <- create_httr2_helper (url, self$headers$Authorization, "POST")

    resp <- httr2::req_perform (req)
    doi <- httr2::resp_body_json (resp)

    return (doi)
})

#' @description Add prereserved DOI to metadata in "identifier" field.
#'
#' @return Updated metadata
#' @noRd

depositsClient$set ("private", "add_doi_to_metadata", function () {

    doi <- ""

    if (self$service == "figshare") {
        doi <- self$hostdata$doi
    } else if (self$service == "zenodo") {
        doi <- self$hostdata$metadata$prereserve_doi$doi
    }

    if (!nzchar (doi)) {
        return (invisible (self))
    }

    if (!"identifier" %in% names (self$metadata)) {
        self$metadata$identifier <- doi
    }

    return (invisible (self))
})

#' @description Obtain deposit 'id' from local "datapackage.json", and use it to
#' retrieve full "hostdata".
#'
#' @return (Invisibly) Updated version of self.
#' @noRd

depositsClient$set ("private", "servicedata_from_dp", function (meta_source) {

    if ("identifier" %in% names (self$metadata) && is.null (self$hostdata)) {

        ptn <- paste0 ("^.*", self$service, "\\.")
        id <- gsub (ptn, "", self$metadata$identifier)
        if (nzchar (id)) {
            id <- tryCatch (as.integer (id), error = function (e) NULL)
        } else {
            id <- NULL
        }

        metadata <- validate_metadata (
            meta_source,
            gsub ("\\-sandbox$", "", self$service)
        )
        metadata <- httptest2_created_timestamp (metadata)

        if (!is.null (id)) {
            if (id %in% private$get_deposits_ids ()) {
                self$deposit_retrieve (id)
            }
        }

        if (!identical (self$metadata, metadata$dcmi)) {
            # Local metadata has been updated; push to host
            self$metadata <- metadata$dcmi

            metadata$service <-
                httptest2_hostdata_timestamps (metadata$service, self$service)
            private$metadata_service <- metadata$service

            files <- self$hostdata$files
            file_names <- files [[private$get_file_name_field ()]]

            if (private$frictionless_json_name %in% file_names &&
                fs::file_exists (meta_source) || fs::dir_exists (meta_source)) {

                meta_source <- fs::path_abs (meta_source)

                meta_dir <- ifelse (
                    fs::is_dir (meta_source),
                    meta_source,
                    fs::path_dir (meta_source)
                )
                meta_path <- fs::path (
                    meta_dir,
                    private$frictionless_json_name
                )
                self$deposit_upload_file (meta_path)
            } # end if upload "datapackage.json"
        } # end if local metadata updated
    }

    return (invisible (self))
})
