# Note this file needs to come alphabetically before the other `client-` files
# for the externally-defined methods to load in the proper sequence!

#' @title depositsClient
#'
#' @description R6 class for constructing deposits queries
#' @return a `depositsClient` class (R6 class)
#' @examples
#' \dontrun{
#' # make a client
#' cli <- depositsClient$new ("zenodo") # or:
#' cli <- depositsClient$new ("figshare")
#' print (cli)
#'
#' # methods
#' cli$deposits_list ()
#'
#' # Fill depositsClient metadata
#' metadata <- list (
#'     title = "New Title",
#'     abstract = "This is the abstract",
#'     creator = list ("A. Person", "B. Person")
#' )
#' cli$deposit_fill_metadata (metadata)
#' print (cli)
#'
#' # or pass metadata directly at construction of new client
#' cli <- depositsClient$new ("figshare", metadata = metadata)
#' }
#' @family client
#' @export
depositsClient <- R6::R6Class ( # nolint (not snake_case)

    "depositsClient",
    portable = TRUE,
    cloneable = FALSE,
    private = list (

        # @field metadata_service holds metadata converted to specific format
        # requires by service. Derived from `metadata`.
        metadata_service = NULL,
        # @field term_map (data.frame) Map between DCMI and deposit terms for
        # specified host service.
        term_map = NULL

        # ... other private methods in R/client-private-methods.R
    ), # end private list

    public = list (

        #' @field service (character) of deposits host service.
        service = NULL,
        #' @field sandbox (logical) Connect client with sandbox if `TRUE`
        #' (zenodo only)
        sandbox = FALSE,
        #' @field deposits (data.frame) Current deposits hosted on service, one
        #' row per deposit.
        deposits = NULL,
        #' @field url_base (character) Base URL of host service API
        url_base = NULL,
        #' @field url_service (character) URL of deposit service
        url_service = NULL,
        #' @field id (integer) Deposit identifier from host service.
        id = NULL,
        #' @field headers (list) list of named headers
        headers = NULL,
        #' @field hostdata (list) Data as stored by host platform
        hostdata = NULL,
        #' @field metadata holds list of DCMI-compliant metadata.
        metadata = NULL,

        #' @description Create a new `depositsClient` object
        #' @param service (character) of a deposits service (see
        #' \link{deposits_services}).
        #' @param metadata Either of one two possible ways of defining
        #' metadata:
        #' \itemize{
        #' \item The name (or full path) or a local file containing
        #' metadata constructed with \link{deposits_metadata_template};
        #' \item A names list of metadata with names matching values given by
        #' \link{dcmi_terms}, and values specified as individual character
        #' strings or lists for multiple entries.
        #' }
        #' @param sandbox If `TRUE`, connect client to sandbox, rather than
        #' actual API endpoint (for "zenodo" only).
        #' @param headers Any acceptable headers. See examples in \pkg{httr2}
        #' package.
        #' @return A new `depositsClient` object

        initialize = function (service,
                               metadata = NULL,
                               sandbox = FALSE,
                               headers = NULL) {

            self <- private$define_service (service, sandbox)

            if (is.null (headers)) {
                service <- self$service
                if (service == "zenodo" & self$sandbox) {
                    service <- "zenodo-sandbox"
                }
                token <- get_deposits_token (service = service)
                self$headers <- list (Authorization = paste0 ("Bearer ", token))
            }

            private$deposits_list_extract ()

            if (!is.null (metadata)) {

                metadata <- validate_metadata (
                    metadata,
                    gsub ("\\-sandbox$", "", self$service)
                )
                self$metadata <- metadata$dcmi
                private$metadata_service <- metadata$service
            }

            return (self)
        },

        #' @description print method for the `depositsClient` class
        #' @param x self
        #' @param ... ignored

        print = function (x, ...) {

            cat ("<deposits client>", sep = "\n")
            cat (paste0 (" deposits service : ", self$service), sep = "\n")
            if (self$service == "zenodo") {
                cat (paste0 ("           sandbox: ", self$sandbox), sep = "\n")
            }
            cat (paste0 ("         url_base : ", self$url_base), sep = "\n")
            if (length (self$deposits) == 0L) {
                cat (" Current deposits : <none>\n")
            } else {
                cat (
                    paste0 (
                        " Current deposits : ",
                        nrow (self$deposits),
                        " (see 'deposits' element for details)"
                    ),
                    sep = "\n"
                )
            }
            cat ("\n")

            if (!is.null (self$url_service)) {
                cat (paste0 ("url_service : ", self$url_service), sep = "\n")
            }
            if (!is.null (self$id)) {
                cat (paste0 (" deposit id : ", self$id), sep = "\n")
            }

            if (is.null (self$hostdata)) {
                cat ("   hostdata : <none>\n")
            } else {
                cat (
                    "   hostdata : list with",
                    length (self$hostdata),
                    " elements\n"
                )
            }

            if (is.null (self$metadata)) {
                cat ("   metadata : <none>\n")
            } else {
                cat (
                    paste0 (
                        "   metadata : ",
                        length (self$metadata),
                        " terms (see 'metadata' element for details)"
                    ),
                    sep = "\n"
                )
            }
        },

        #' @description Update 'deposits' item of current deposits for given
        #' service
        #' @return Updated 'deposits' client

        deposits_list = function () {

            self <- private$deposits_list_extract ()

            invisible (self)
        },

        #' @description Search all public deposits
        #' @param search_string Single string to search for
        #' @param page_size Number of records to return in one page
        #' @param page_number Starting page for return results; used in
        #' combination with 'page_size' for pagination.
        #' @param ... Named pairs of query parameters.
        #' Zenodo parameters are described at
        #' \url{https://developers.zenodo.org/#list36}, and currently include:
        #' \itemize{
        #' \item status: either "draft" or "published"
        #' \item sort: either "bestmatch" (the default) or "mostrecent"
        #' \item all_versions: Either "true" or "false"
        #' \item communities: Search for deposits only within specified
        #' communities
        #' \item type: Return deposits only of specified type
        #' \item subtype: Return deposits only of specified subtype
        #' \item bound: A geolocation bounding box
        #' \item custom: Custom keywords
        #' }
        #'
        #' Figshare parameters are described at
        #' \url{https://docs.figshare.com/#articles_search}, and currently
        #' include:
        #' \itemize{
        #' \item resource_doi: Only return deposits matching this 'resource_doi'
        #' \item item_type: Return deopsits of specified type (as integer).
        #' \item doi: Only return deposits matching this DOI
        #' \item handle: Only return deposits matching this handle
        #' \item project_id: Only return deposits from within specified project
        #' \item order: Order for sorting results; one of "published_date",
        #' "modified_date", "views", "shares", "downloads", or "cites"
        #' \item search_for: Search term.
        #' \item order_direction: "asc" or "desc"
        #' \item institution: Only return deposits from specified institution
        #' (as integer)
        #' \item group: Only return deposits from specified group (as integer)
        #' \item published_since: Only return deposits published since specified
        #' date (as YYYY-MM-DD)
        #' \item modified_since: Only return deposits modified since specified
        #' date (as YYYY-MM-DD)
        #' }
        #' @return A `data.frame` of data on deposits matching search parameters
        #' (with format depending on the deposits service.)
        #' @examples
        #' \dontrun{
        #' cli <- depositsClient$new (service = "figshare")
        #' search_results <- cli$deposits_search (
        #'     search_string = "Text string query",
        #'     page_size = 5L
        #' )
        #' }

        deposits_search = function (search_string = NULL,
                                    page_size = 10L,
                                    page_number = 1L,
                                    ...) {

            arglist <- process_search_params (
                self$service,
                search_string = search_string,
                page_size = page_size,
                page_number = page_number,
                ...
            )

            url <- paste0 (
                self$url_base,
                ifelse (self$service == "figshare",
                    "articles/search",
                    "records"
                )
            )

            method <- ifelse (self$service == "figshare", "POST", "GET")
            req <- create_httr2_helper (url, self$headers$Authorization, method)

            if (self$service == "figshare") {
                req <- httr2::req_body_json (req, arglist)
            } else {
                req <- do.call (
                    httr2::req_url_query,
                    c (.req = list (req), arglist)
                )
            }

            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            return (httr2::resp_body_json (resp, simplifyVector = TRUE))
        },

        #' @description Deleted a nominated deposit
        #' @param deposit_id Integer identifier of deposit (generally obtained
        #' from `list_deposits` method).
        #' @return Updated 'deposits' client

        deposit_delete = function (deposit_id = NULL) {

            if (is.null (deposit_id)) {
                deposit_id <- self$id
            }
            checkmate::assert_int (deposit_id)

            url <- get_service_url (self, deposit_id = deposit_id)

            req <- create_httr2_helper (
                url,
                self$headers$Authorization,
                "DELETE"
            )
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            # rm current 'hostdata' + 'metadata' if they are from self$id:
            self <- private$rm_host_meta_data ()

            # Then return client with that deposit removed from list:
            self <- self$deposits_list ()

            invisible (self)
        },

        #' @description Fill deposits client with metadata.
        #' @param metadata Either one of two possible ways of defining
        #' metadata:
        #' \itemize{
        #' \item The name (or full path) or a local file containing
        #' metadata constructed with \link{deposits_metadata_template};
        #' \item A names list of metadata with names matching values given by
        #' \link{dcmi_terms}, and values specified as individual character
        #' strings or lists for multiple entries.
        #' }
        #' @return Updated deposits client with metadata inserted.

        deposit_fill_metadata = function (metadata = NULL) {

            metadata <- validate_metadata (metadata, self$service)
            self$metadata <- metadata$dcmi
            private$metadata_service <- metadata$service

            invisible (self)
        },

        #' @description Create a new deposit
        #' @param quiet If `FALSE` (default), print integer identifier of newly
        #' created deposit.
        #' @return Updated deposits client which includes data on new deposit

        deposit_new = function (quiet = FALSE) {

            if (length (self$metadata) == 0L) {
                stop ("No metadata present; use 'fill_metadata()' first.")
            }

            # Re-run service-specific metadata validation in case anything has
            # changed:
            self$metadata <- httptest2_dcmi_created (self$metadata)
            private$metadata_service <- validate_service_metadata (
                self$metadata,
                service = self$service
            )

            url <- get_service_url (self)

            req <- create_httr2_helper (url, self$headers$Authorization, "POST")
            req <- httr2::req_body_json (req, data = private$metadata_service)

            resp <- httr2::req_perform (req)

            hostdata <- httr2::resp_body_json (resp)

            if (self$service == "figshare") {
                self$deposit_retrieve (hostdata$entity_id)
            } else if (self$service == "zenodo") {
                self$hostdata <- hostdata
            }

            self <- private$fill_service_id_url ()

            self <- private$deposits_list_extract ()

            if (!quiet) {
                cat ("ID of new deposit :", self$id, "\n")
            }

            invisible (self)
        },

        #' @description Create a new `depositsClient` object
        #' @param service (character) of a deposits service (see
        #' \link{deposits_services}).
        #' @param sandbox If `TRUE`, connect client to sandbox, rather than
        #' actual API endpoint (for "zenodo" only).
        #' @param headers Any acceptable headers. See examples in \pkg{httr2}
        #' package.

        deposit_service = function (service = NULL,
                                    sandbox = FALSE,
                                    headers = NULL) {

            self <- private$define_service (service, sandbox)

            if (is.null (headers)) {
                service <- self$service
                if (service == "zenodo" & self$sandbox) {
                    service <- "zenodo-sandbox"
                }
                token <- get_deposits_token (service = service)
                self$headers <- list (Authorization = paste0 ("Bearer ", token))
            }

            private$deposits_list_extract ()

            self$hostdata <- NULL

            invisible (self)
        },

        #' @description Update metadata for specified deposit
        #' @note Client should already contain metadata updated with the
        #' 'deposit_fill_metadata()' function.
        #' @param deposit_id The 'id' number of deposit to update. If not
        #' specified, the 'id' value of current deposits client is used.

        deposit_update = function (deposit_id = NULL) {

            if (is.null (deposit_id)) {
                deposit_id <- self$id
            }
            checkmate::assert_int (deposit_id)

            url <- get_service_url (self, deposit_id = deposit_id)

            req <- create_httr2_helper (url, self$headers$Authorization, "PUT")
            req$headers <- c (req$headers, "Content-Type" = "application/json")

            req <- httr2::req_body_json (req, data = private$metadata_service)

            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            self <- self$deposit_retrieve (deposit_id)

            invisible (self)
        },

        #' @description Upload file to an existing deposit
        #' @param deposit_id The 'id' number of deposit which file is to be
        #' uploaded to. If not specified, the 'id' value of current deposits
        #' client is used.
        #' @param path Path to local file.
        #' @param use_local_datapackage When `FALSE` (default), the
        #' \pkg{frictionless} "datapackage.json" file is obtained from the
        #' remote service (if it exists), and updated with information on the
        #' uploaded file. If `TRUE`, then the local version at the base
        #' directory of `path` will be used, with any additional resources on
        #' the remote service appended to this file. If the local version
        #' contains a "metadata" item, that metadata will also be used to
        #' overwrite any metadata both in the local client, and on any remote
        #' "datapackage.json" file.
        #' @return Updated 'deposits' client

        deposit_upload_file = function (deposit_id = NULL, path = NULL,
                                        use_local_datapackage = FALSE) {

            if (is.null (deposit_id)) {
                deposit_id <- self$id
            }

            checkmate::assert_int (deposit_id)
            checkmate::assert_character (path, len = 1L)
            checkmate::assert_file_exists (path)

            path <- fs::path_real (path)

            self <- private$upload_local_file (deposit_id, path)

            path_dir <- fs::path_dir (path)
            dp_path <- fs::path (path_dir, "datapackage.json")
            has_dpj <- fs::file_exists (dp_path)
            metadata_updated <- TRUE # Flag for local "datapackage.json"

            if (use_local_datapackage) {
                if (!has_dpj) {
                    stop ("No 'datapackage.json' file found at ['", dp_path, "']")
                }
                dpj <- jsonlite::read_json (dp_path)
                if (!"metadata" %in% names (dpj)) {
                    metadata_updated <- private$add_meta_to_dp_json (path_dir) # TRUE
                }
            } else {
                files <- self$hostdata$files
                file_names <- files [[private$get_file_name_field ()]]
                if ("datapackage.json" %in% file_names) {
                    self$deposit_download_file (
                        deposit_id,
                        filename = "datapacakge.json",
                        path = dp_path,
                        overwrite = TRUE,
                        quiet = quiet
                    )
                } else if (!has_dpj) {
                    private$generate_frictionless (path)
                } else {
                    metadata_updated <- FALSE
                }
            }

            if (metadata_updated) {
                message ("Uploading 'datapackage.json'")
                self <- private$upload_local_file (deposit_id, dp_path)
            } else {
                message ("NOT uploading 'datapackage.json'")
            }

            invisible (self)
        },

        #' @description Retrieve information on specified deposit
        #' @param deposit_id The 'id' number of deposit for which information is
        #' to be retrieved.
        #' @param quiet If `FALSE` (default), display information on screen on
        #' any issues encountered in retrieving deposit.
        #' @return Updated 'deposits' client

        deposit_retrieve = function (deposit_id, quiet = FALSE) {

            checkmate::assert_int (deposit_id)

            url <- get_service_url (self, deposit_id = deposit_id)

            req <- create_httr2_helper (url, self$headers$Authorization, "GET")
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            hostdata <- httr2::resp_body_json (resp, simplifyVector = TRUE)
            hostdata <- httptest2_hostdata_timestamps (hostdata, self$service)
            self$hostdata <- hostdata

            if (self$service == "figshare" && !self$hostdata$is_public) {
                if (!quiet) {
                    message (
                        "Files for private Figshare deposits can only be ",
                        "downloaded manually; no metadata can be retrieved ",
                        "for this deposit."
                    )
                }
            } else {
                files <- private$get_hostdata_files (deposit_id, "datapackage.json")
                name_field <- private$get_file_name_field ()
                if ("datapackage.json" %in% files [[name_field]]) {
                    # Rm any 'datapackage.json' that is in temp dir:
                    dp_path <- fs::path (fs::path_temp (), "datapackage.json")
                    if (fs::file_exists (dp_path)) {
                        fs::file_delete (dp_path)
                    }
                    dp_path <- self$deposit_download_file (deposit_id, "datapackage.json", fs::path_temp ())
                    if (fs::file_exists (dp_path)) {
                        self$metadata <- jsonlite::read_json (dp_path, simplifyVector = FALSE)$metadata
                        fs::file_delete (dp_path)
                    }
                }
            }

            self <- private$fill_service_id_url ()

            invisible (self)
        },

        #' @description Download a specified 'filename' from a deposit
        #' @param deposit_id The 'id' number of deposit which file is to be
        #' downloaded from. If not specified, the 'id' value of current deposits
        #' client is used.
        #' @param filename The name of the file to be download as specified in
        #' the deposit.
        #' @param path The local directory where file is to be downloaded.
        #' @param overwrite Do not overwrite existing files unless set to
        #' `TRUE`.
        #' @param quiet If `FALSE`, display download progress.
        #' @return The full path of the downloaded file.

        deposit_download_file = function (deposit_id = NULL,
                                          filename,
                                          path = NULL,
                                          overwrite = FALSE,
                                          quiet = FALSE) {

            if (is.null (deposit_id)) {
                deposit_id <- self$id
            }

            checkmate::assert_int (deposit_id)
            checkmate::assert_character (filename, len = 1L)
            if (!is.null (path)) {
                checkmate::assert_character (path, len = 1L)
                checkmate::assert_directory_exists (path)
            }
            checkmate::assert_logical (quiet, len = 1L)

            # repeat retrieve_deposit method to get download_url:
            name_field <- private$get_file_name_field ()

            files <- private$get_hostdata_files (deposit_id, filename)

            if (!filename %in% files [[name_field]]) {
                stop ("That deposit does not contain the specified file.")
            }

            if (self$service == "figshare") {
                download_url <- files$download_url [files$name == filename]
            } else if (self$service == "zenodo") {
                download_url <-
                    files$links$download [files$filename == filename]
            }

            if (self$service == "figshare") {
                if (!self$hostdata$is_public) {
                    stop (
                        "Figshare only enables automated downloads of public ",
                        "files.\nYou can manually download at ", download_url
                    )
                }
            }

            if (is.null (path)) {
                path <- fs::path (here::here ())
            }
            destfile <- fs::path (path, filename)
            if (fs::file_exists (destfile) & !overwrite) {
                stop (
                    "File [", destfile, "] exists; either remove ",
                    "or pass `overwrite = TRUE`."
                )
            }

            req <- create_httr2_helper (
                download_url,
                self$headers$Authorization,
                "GET"
            )

            resp <- httr2::req_perform (req, path = destfile)
            httr2::resp_check_status (resp)

            return (destfile)
        }
    ) # end public list
)
