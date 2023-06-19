#' @description Define service for deposits client
#' @param service (character) of a deposits service (see
#' \link{deposits_services}).
#' @param sandbox If `TRUE`, connect client to sandbox, rather than
#' actual API endpoint (for "zenodo" only).
#' @noRd

depositsClient$set (
    "private", "define_service",
    function (service, sandbox = FALSE) {

        service_names <- deposits_services ()$name
        service_names <- unique (gsub ("\\-sandbox$", "", service_names))
        service <- match.arg (tolower (service), service_names)
        checkmate::assert_logical (sandbox, len = 1L)

        service <- add_service_sandbox (service, sandbox)
        self$sandbox <- sandbox

        s <- deposits_services ()
        self$service <- service
        self$url_base <- s$api_base_url [s$name == service]
        self$service <- rm_service_sandbox (service)

        invisible (self)
    }
)

#' @description Extract list of all current deposits and store as
#' 'deposits' member element.
#' @noRd

depositsClient$set ("private", "deposits_list_extract", function () {

    url <- service_deposits_urls (self$service, self$url_base)

    req <- create_httr2_helper (url, self$headers$Authorization, "GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    self$deposits <- httr2::resp_body_json (resp, simplifyVector = TRUE)

    invisible (self)
})

#' @description Perform actual upload of local file.
#' @noRd

depositsClient$set (
    "private", "upload_local_file",
    function (path, overwrite, compress) {

        name_field <- service_filename_field (self$service)
        current_files <- self$hostdata$files [[name_field]]
        current_files_no_ext <- NULL
        if (!is.null (current_files)) {
            current_files_no_ext <- fs::path_ext_remove (current_files)
            current_files_no_ext <- gsub ("\\.tar$", "", current_files_no_ext)
        }
        path_no_ext <- fs::path_ext_remove (fs::path_file (path))
        file_exists <- path_no_ext %in% current_files_no_ext

        if (file_exists && compress == "no") {
            remote_file <-
                current_files [match (path_no_ext, current_files_no_ext)]
            compress <- compress_from_filename (remote_file)
        }
        if (compress != "no") {
            path_old <- path
            path <- compress_local_file (path, compress)
        }

        chk <- md5sums_are_same (
            path,
            self$hostdata,
            name_field,
            self$service,
            file_exists = file_exists,
            quiet = FALSE
        )
        if (chk) {
            return (invisible (self))
        }

        url <- get_service_url (self)

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

#' @description Update remote files with modified versions of local files.
#' @noRd

depositsClient$set (
    "private", "update_files",
    function (path) {

        name_field <- service_filename_field (self$service)
        files <- self$hostdata$files [[name_field]]
        files_no_ext <- NULL
        if (!is.null (files)) {
            files_no_ext <- fs::path_ext_remove (files)
            files_no_ext <- gsub ("\\.tar$", "", files_no_ext)
        }

        if (is_dcf (path)) { # description file

            # code from main 'upload_local_file' method, which shouild be put in
            # separate utils fn.
            if (!fs::is_dir (path)) {
                path <- fs::path_dir (path)
            }
            td <- fs::file_temp (pattern = "deposits_temppath_")
            fs::dir_create (td)
            fs::dir_copy (path, td)
            td <- fs::path (td, fs::path_file (path))

            flocal <- compress_local_file (td, compress = "rpkg")
        } else {

            flocal <- fs::path_abs (path)
            if (fs::is_dir (path)) {
                flocal <- fs::dir_ls (path)
            }
        }

        for (f in flocal) {
            f_base <- fs::path_file (f)
            f_ext <- fs::path_ext (f_base)
            f_no_ext <- fs::path_ext_remove (f_base)
            f_no_ext <- gsub ("\\.tar$", "", f_no_ext)

            if (!is_dcf (path) && !f_no_ext %in% files_no_ext) {
                stop (
                    "Local file [",
                    f_base,
                    "] does not exist on remote deposit. ",
                    "Please use 'deposit_upload_file()' to first upload file.",
                    call. = FALSE
                )
            }

            f_remote <- files [match (f_no_ext, files_no_ext)]
            compress <- compress_from_filename (f_remote)
            if (is_dcf (path)) {
                compress <- "no"
            }

            private$upload_local_file (f, overwrite = TRUE, compress = compress)
        }
    }
)

compress_local_file <- function (path, compress) {

    if (compress %in% c ("tar", "rpkg")) {
        file_ext <- ".tar.gz"
    } else if (compress == "zip") {
        file_ext <- ".zip"
    }

    binfile <- fs::path_ext_set (path, file_ext)
    if (fs::file_exists (binfile)) {
        message (
            "File [",
            binfile,
            "] already exists; will not be re-created."
        )
    } else if (compress == "tar") {
        binfile <- withr::with_dir (path, tar (binfile, compression = "gzip"))
    } else if (compress == "zip") {
        # Update flags from ?zip to add 'q' to suppress verbose output
        binfile <- withr::with_dir (
            path,
            zip (binfile, files = fs::dir_ls (), flags = "-r9Xq")
        )
    } else if (compress == "rpkg") {
        requireNamespace ("pkgbuild")
        binfile <- withr::with_dir (path, pkgbuild::build (vignettes = FALSE))
    }

    return (binfile)
}

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

        name_field <- service_filename_field (self$service)

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

    url <- paste0 (get_service_url (self, private = TRUE), "/", id, "/reserve_doi")
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
            if (id %in% private$get_deposits_ids ()) { # in service-methods.R
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
            file_names <- files [[service_filename_field (self$service)]]

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

#' @description Count numbers of local and remote resources to store in private
#' member fields.
#'
#' @return (Invisibly) Updated version of self.
#' @noRd

depositsClient$set ("private", "count_num_resources", function () {

    if (is.null (self$local_path) && is.null (self$hostdata$files)) {
        private$num_resources_local <- private$num_resources_remote <- 0L
        return (invisible (self))
    }

    files <- self$hostdata$files
    if (!is.null (files)) {
        file_names <- files [[service_filename_field (self$service)]]
        file_names <-
            file_names [which (!file_names == private$frictionless_json_name)]
        private$num_resources_remote <- length (file_names)
    }

    if (!is.null (self$local_path)) {

        path_dp <- fs::path (self$local_path, private$frictionless_json_name)

        if (fs::file_exists (path_dp)) {

            op <- options (
                readr.show_progress = FALSE,
                readr.show_col_types = FALSE
            )
            suppressMessages (
                dp <- frictionless::read_package (path_dp)
            )
            options (op)

            private$num_resources_local <- length (dp$resources)
        }
    }

    return (invisible (self))
})
