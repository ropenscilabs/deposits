
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
    function (deposit_id, path) {

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
    }
)

#' @description Auto-generate default 'frictionless' JSON file.
#'
#' This generates an additional (default) "datapackage.json" file in the
#' directory of the file specified by 'path'.
#'
#' @param path Full path to data object to be uploaded.
#' @noRd

depositsClient$set ("private", "generate_frictionless", function (path) {

    requireNamespace ("frictionless")

    resource_name <- fs::path_ext_remove (fs::path_file (path))
    p <- frictionless::create_package ()
    op <- options (readr.show_progress = FALSE, readr.show_col_types = FALSE)
    p <- frictionless::add_resource (
        p,
        resource_name = resource_name,
        data = path
    )
    frictionless::write_package (p, fs::path_dir (path))
    options (op)
})

#' @description Add metadata to /pkg{frictionless} 'datapackage.json' file.
#'
#' @param path Path to directory containing 'datapackage.json' file.
#' @return A logical value of `TRUE` if 'datapackage.json' is updated; otherwise
#' `FALSE`.
#' @noRd

depositsClient$set ("private", "add_meta_to_dp_json", function (path) {

    ret <- FALSE

    path_json <- fs::path (path, private$frictionless_json_name)
    op <- options (readr.show_progress = FALSE, readr.show_col_types = FALSE)
    p <- frictionless::read_package (path_json)
    options (op)

    if (!"metadata" %in% names (p)) {
        p <- append (p, c (metadata = list (self$metadata)), after = 1)
        op <- options (
            readr.show_progress = FALSE,
            readr.show_col_types = FALSE
        )
        frictionless::write_package (p, path)
        options (op)
        ret <- TRUE
    }

    return (ret)
})

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

depositsClient$set ("private", "get_hostdata_files", function (deposit_id, filename) {

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
})

depositsClient$set (
    "private", "update_and_upload_frictionless",
    function (path, use_local_datapackage) {

        deposit_id <- self$id

        path_dir <- fs::path_dir (path)
        dp_path <- fs::path (path_dir, private$frictionless_json_name)
        has_dpj <- fs::file_exists (dp_path)
        metadata_updated <- TRUE # Flag for local "datapackage.json"

        if (use_local_datapackage) {
            if (!has_dpj) {
                message (
                    "A 'frictionless' file has been created at:\n   ",
                    dp_path
                )
                private$generate_frictionless (path)
            } else {
                metadata_updated <- FALSE
            }
            dpj <- jsonlite::read_json (dp_path)
            if (!"metadata" %in% names (dpj)) {
                metadata_updated <- private$add_meta_to_dp_json (path_dir)
            }

            # If not updated, then only consider updated if remote service
            # has not "metadata"
            files <- self$hostdata$files
            file_names <- files [[private$get_file_name_field ()]]
            if (!"datapackage.json" %in% file_names) {
                metadata_updated <- TRUE
            }
        } else {
            files <- self$hostdata$files
            file_names <- files [[private$get_file_name_field ()]]
            if (private$frictionless_json_name %in% file_names) {
                is_fs_private <- self$service == "figshare" &&
                    !self$hostdata$is_public
                if (!is_fs_private) {
                    self$deposit_download_file (
                        deposit_id,
                        filename = private$frictionless_json_name,
                        path = path_dir,
                        overwrite = TRUE,
                        quiet = quiet
                    )
                } else {
                    metadata_updated <- FALSE
                }
            } else if (!has_dpj) {
                private$generate_frictionless (path)
            } else {
                metadata_updated <- FALSE
            }
        }

        # httptest2 does not produce mocked download files; only the actual
        # request result. So these files can not be uploaded here.
        if (metadata_updated &&
            Sys.getenv ("DEPOSITS_TEST_ENV") != "true") {
            self <- private$upload_local_file (deposit_id, dp_path)
        }

        invisible (self)
    }
)
