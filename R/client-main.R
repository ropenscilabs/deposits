# Note this file needs to come alphabetically before the other `client-` files
# for the externally-defined methods to load in the proper sequence!

#' @title depositsClient
#'
#' @description An R6 client for managing deposits on external services,
#' currently including Figshare and Zenodo. Use of a 'deposits' client is
#' controlled by the methods listed below. Those looking for help with client
#' usage are advised to head to that section.
#'
#' @return A `depositsClient` class (R6 class)
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
        frictionless_json_name = "datapackage.json",

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
        #' @field frictionless (logical) Default behaviour of `TRUE` assumes
        #' uploads are data files in rectangular form, able to be described by
        #' \pkg{frictionless} metadata. \pkg{frictionless} integration is
        #' by-passed when this parameter if `FALSE`.
        frictionless = TRUE,
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

        #' @description Create a new `depositsClient` object, as an \pkg{R6}
        #' client with methods listed via `deposits_emthods()`.
        #'
        #' @param service (character) Name of a deposits service (see
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
        #' @examples
        #' \dontrun{
        #' cli <- depositsClient$new (service = "zenodo", sandbox = TRUE)
        #' # List methods of client:
        #' cli$deposits_methods ()
        #' # List all current deposits associated with user token:
        #' cli$deposits_list ()
        #' }

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
                metadata <- httptest2_created_timestamp (metadata)
                self$metadata <- metadata$dcmi
                private$metadata_service <- metadata$service
            }

            return (self)
        },

        #' @description `print` method for the `depositsClient` class, providing
        #' an on-screen overview of current contents and structure of client.
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

        # -----------
        # From here on, all methods are defined ain alphabetical order
        # ---------

        #' @description Deleted a specified deposit from the remote service.
        #' This removes the deposits from the associated service, along with all
        #' corresponding 'hostdata' in the local client.
        #' @param deposit_id Integer identifier of deposit (generally obtained
        #' from `list_deposits` method).
        #' @return (Invisibly) Updated 'deposits' client

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

        #' @description Delete a single from a deposits service.
        #'
        #' This does not modify the "datapackage.json" file, either locally or
        #' on a service.
        #'
        #' @param filename Name of file to be deleted as recorded on service.
        #' @param deposit_id The 'id' number of deposit from which file is to be
        #' deleted. If not specified, the 'id' value of current deposits client
        #' is used.
        #' @return (Invisibly) Updated 'deposits' client
        #' @examples
        #' \dontrun{
        #' # Initiate deposit and fill with metadata:
        #' metadata <- list (
        #'     Title = "Iris Dataset",
        #'     Creator = "Edgar Anderson",
        #'     Publisher = "American Iris Society",
        #'     Source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x"
        #' )
        #' cli <- depositsClient$new (
        #'     service = "zenodo",
        #'     sandbox = TRUE,
        #'     metadata = metadata
        #' )
        #' cli$deposit_new ()
        #'
        #' # Create some local data and upload to deposit:
        #' path <- fs::path (fs::path_temp (), "iris.csv")
        #' write.csv (datasets::iris, path)
        #' cli$deposit_upload_file (path = path)
        #'
        #' # Confirm that uploaded files include \pkg{frictionless}
        #' # "datapackage.json" file, and also that local version has been
        #' # created:
        #' cli$hostdata$files
        #'
        #' # Then delete one of those files:
        #' cli$deposit_delete_file ("datapackage.json")
        #' }

        deposit_delete_file = function (filename) {

            checkmate::assert_character (filename, len = 1L)

            self <- private$delete_file (filename)

            invisible (self)
        },

        #' @description Download a specified 'filename' from a deposit.
        #' @param filename The name of the file to be download as specified in
        #' the deposit.
        #' @param deposit_id The 'id' number of deposit which file is to be
        #' downloaded from. If not specified, the 'id' value of current deposits
        #' client is used.
        #' @param path The local directory where file is to be downloaded.
        #' @param overwrite Do not overwrite existing files unless set to
        #' `TRUE`.
        #' @param quiet If `FALSE`, display download progress.
        #' @return The full path of the downloaded file.

        deposit_download_file = function (filename,
                                          deposit_id = NULL,
                                          path = NULL,
                                          overwrite = FALSE,
                                          quiet = FALSE) {

            if (is.null (deposit_id)) {
                deposit_id <- self$id
            }

            checkmate::assert_int (deposit_id)
            checkmate::assert_character (filename, len = 1L)
            if (is.null (path)) {
                path <- fs::path (here::here ())
            } else {
                checkmate::assert_character (path, len = 1L)
                checkmate::assert_directory_exists (path)
                path <- fs::path_real (path)
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
                        "Figshare does not allow automated downloads of ",
                        "private files.\nYou can manually download at ",
                        download_url
                    )
                }
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
        },

        #' @description Embargo a deposit prior to publication.
        #' @param embargo_date Date of expiry of embargo. If the
        #' `deposit_publish()` method has been called, deposit will
        #' automatically be published after this date, and will not be
        #' published, nor publicly accessible, prior to this date.
        #' @param embargo_type For Figshare service only, which allows embargoes
        #' for entire deposits or single files. Ignored for other services.
        #' @param embargo_reason For Figshare service only, an optional text
        #' string describing reasons for embargo.
        #' @return (Invisibly) Updated deposits client with additional embargo
        #' information.

        deposit_embargo = function (embargo_date = NULL,
                                    embargo_type = c ("deposit", "file"),
                                    embargo_reason = NULL) {

            if (is.null (self$id)) {
                stop (
                    "Client not associated with any deposit which can ",
                    "be embargoed. Please first use `deposit_new()` ",
                    "or `deposit_retrieve()` methods.",
                    call. = FALSE
                )
            }

            # Re-generate service metadata from DCMI:
            metadata <- validate_metadata (
                self$metadata,
                gsub ("\\-sandbox$", "", self$service)
            )
            metadata <- httptest2_created_timestamp (metadata)
            self$metadata <- metadata$dcmi
            private$metadata_service <- metadata$service

            checkmate::assert_character (embargo_date, len = 1L)
            embargo_date <- strftime (embargo_date, "%Y-%m-%d")

            if (self$service == "zenodo") {

                self <- private$embargo_zenodo (embargo_date)

            } else if (self$service == "figshare") {

                embargo_type <- match.arg (embargo_type)
                if (embargo_type == "deposit") {
                    embargo_type <- "article"
                }
                if (!is.null (embargo_reason)) {
                    checkmate::assert_character (embargo_reason, len = 1L)
                }
                self <- private$embargo_figshare (
                    embargo_date, embargo_type, embargo_reason
                )
            }

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
        #' @return (Invisibly) Updated deposits client with metadata inserted.

        deposit_fill_metadata = function (metadata = NULL) {

            metadata <- validate_metadata (
                metadata,
                gsub ("\\-sandbox$", "", self$service)
            )
            metadata <- httptest2_created_timestamp (metadata)
            self$metadata <- metadata$dcmi

            metadata$service <-
                httptest2_hostdata_timestamps (metadata$service, self$service)
            private$metadata_service <- metadata$service

            invisible (self)
        },

        #' @description Initiate a new deposit on the external deposits service.
        #' @param quiet If `FALSE` (default), print integer identifier of newly
        #' created deposit.
        #' @return (Invisibly) Updated deposits client which includes data on
        #' new deposit

        deposit_new = function (quiet = FALSE) {

            if (length (self$metadata) == 0L) {
                stop ("No metadata present; use 'fill_metadata()' first.")
            }

            # Re-run service-specific metadata validation in case anything has
            # changed:
            metadata <- validate_metadata (
                self$metadata,
                gsub ("\\-sandbox$", "", self$service)
            )
            metadata <- httptest2_created_timestamp (metadata)
            self$metadata <- metadata$dcmi

            metadata$service <-
                httptest2_hostdata_timestamps (metadata$service, self$service)
            private$metadata_service <- metadata$service

            # Insert 'self$metadata' into host parameter (#65):
            private$dcmi2host ()

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

        #' @description Publish a deposit
        #' @return (Invisibly) Updated 'deposits' client

        deposit_publish = function () {

            if (is.null (self$id)) {
                stop (
                    "Client not associated with any deposit which can ",
                    "be embargoed. Please first use `deposit_new()` ",
                    "or `deposit_retrieve()` methods.",
                    call. = FALSE
                )
            }
            self$deposit_retrieve (self$id)

            if (self$service == "zenodo") {
                is_embargoed <-
                    identical (self$hostdata$metadata$access_right, "embargoed")
            } else if (self$service == "figshare") {
                is_embargoed <- self$hostdata$is_embargoed
            }

            proceed <- TRUE
            if (!is_embargoed && interactive ()) {
                ans <- readline (paste0 (
                    "Do you wish to place an embargo date ",
                    "prior to publication (y/n)? "
                ))
                # do not proceed if "y":
                proceed <- !identical (tolower (substring (ans, 1L, 1L)), "y")
                if (!proceed) {
                    message (paste0 (
                        "First call the 'deposit_embargo()' ",
                        "method prior to publication."
                    ))
                }
            }

            if (proceed && interactive ()) {
                ans <- readline (paste0 (
                    "This action can not be undone. ",
                    "Are you sure you want to publish deposit#",
                    cli$id,
                    " (y/n) ? "
                ))
                proceed <- identical (tolower (substring (ans, 1L, 1L)), "y")
            }

            if (proceed) {
                if (self$service == "zenodo") {
                    private$publish_zenodo ()
                } else if (self$service == "figshare") {
                    private$publish_figshare ()
                }
            }

            invisible (self)
        },

        #' @description Retrieve information on specified deposit.
        #' @param deposit_id The 'id' number of deposit for which information is
        #' to be retrieved.
        #' @param quiet If `FALSE` (default), display information on screen on
        #' any issues encountered in retrieving deposit.
        #' @return (Invisibly) Updated 'deposits' client

        deposit_retrieve = function (deposit_id, quiet = FALSE) {

            checkmate::assert_int (deposit_id)

            url <- get_service_url (self, deposit_id = deposit_id)

            req <- create_httr2_helper (url, self$headers$Authorization, "GET")
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            hostdata <- httr2::resp_body_json (resp, simplifyVector = TRUE)
            hostdata <- httptest2_hostdata_timestamps (hostdata, self$service)
            self$hostdata <- hostdata

            private$host2dcmi_internal ()

            if (self$service == "figshare" && !self$hostdata$is_public) {
                if (!quiet) {
                    message (
                        "Files for private Figshare deposits can only be ",
                        "downloaded manually; no metadata can be retrieved ",
                        "for this deposit."
                    )
                }
            } else {
                files <- private$get_hostdata_files (
                    deposit_id,
                    private$frictionless_json_name
                )
                name_field <- private$get_file_name_field ()
                if (private$frictionless_json_name %in% files [[name_field]]) {
                    # Rm any 'datapackage.json' that is in temp dir:
                    dp_path <- fs::path (
                        fs::path_temp (),
                        private$frictionless_json_name
                    )
                    dp_exists <- fs::file_exists (dp_path)
                    if (dp_exists) {
                        fs::file_delete (dp_path)
                    }
                    dp_path <- self$deposit_download_file (
                        deposit_id,
                        filename = private$frictionless_json_name,
                        path = fs::path_temp ()
                    )
                    if (fs::file_exists (dp_path)) {
                        suppressMessages (
                            self$metadata <- frictionless::read_package (
                                dp_path
                            )$metadata
                        )
                        # only delete if 'datapackage.json' created here:
                        if (!dp_exists) {
                            fs::file_delete (dp_path)
                        }
                    }
                } else if (length (self$hostdata$files) > 0L) {
                    self$frictionless <- nrow (self$hostdata$files) == 1L
                } else {
                    self$frictionless <- length (self$hostdata$files) == 0L
                }
            }

            self <- private$fill_service_id_url ()

            invisible (self)
        },

        #' @description Switch external services associated with a
        #' `depositsClient` object.
        #' @param service (character) Name of a deposits service (see
        #' \link{deposits_services}).
        #' @param sandbox If `TRUE`, connect client to sandbox, rather than
        #' actual API endpoint (for "zenodo" only).
        #' @param headers Any acceptable headers. See examples in \pkg{httr2}
        #' package.
        #' @return (Invisibly) Updated deposits client.

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

        #' @description Update local metadata by downloading from specified
        #' deposit. This can be used, for example, to synchronise local client
        #' metadata after they have been modified through other methods, such as
        #' online through the service's web interface.
        #' @param deposit_id The 'id' number of deposit to update. If not
        #' specified, the 'id' value of current deposits client is used.
        #' @return (Invisibly) Updated deposits client.

        deposit_update = function (deposit_id = NULL) {

            if (is.null (deposit_id)) {
                deposit_id <- self$id
            }
            checkmate::assert_int (deposit_id)

            url <- get_service_url (self, deposit_id = deposit_id)

            req <- create_httr2_helper (url, self$headers$Authorization, "PUT")
            req$headers <- c (req$headers, "Content-Type" = "application/json")

            # Re-generate service metadata:
            metadata_service <- translate_dc_to_service (
                self$metadata,
                service = gsub ("\\-sandbox$", "", self$service)
            )
            metadata_service <- httptest2_created_timestamp (metadata_service)
            metadata_service <-
                httptest2_hostdata_timestamps (metadata_service, self$service)

            req <- httr2::req_body_json (req, data = metadata_service)

            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            self <- self$deposit_retrieve (deposit_id)

            invisible (self)
        },

        #' @description Update both local and remote "datapackage.json" files
        #' with contents of client metadata. This function is intended to be
        #' used when client itself is used to update metadata, in order for any
        #' local changes to be propagated through to the \pkg{frictionless}
        #' "datapackage.json" file(s).
        #' @param deposit_id The 'id' number of deposit to update. If not
        #' specified, the 'id' value of current deposits client is used.
        #' @param path (Optional) path to local directory containing deposit
        #' data and a \pkg{frictionless} "datapackage.json" file. If specified,
        #' that local "datapackage.json" will be updated, and the updated
        #' version then uploaded to the deposits service.
        #' @return (Invisibly) Updated deposits client.

        deposit_update_frictionless = function (deposit_id = NULL,
                                                path = NULL) {

            if (!is.null (path)) {
                checkmate::assert_directory_exists (path)
            }

            path_is_local <- !is.null (path)

            if (!path_is_local) {

                name_field <- private$get_file_name_field ()
                if (!private$frictionless_json_name %in%
                    self$hostdata$files [[name_field]]) {
                    stop (
                        self$service, " deposit#", self$id, " has no '",
                        private$frictionless_json_name, "' file",
                        call. = FALSE
                    )
                }
                path <- fs::path_temp ()
                self$deposit_download_file (
                    filename = private$frictionless_json_name,
                    path = path
                )
            }

            path_json <- fs::path (path, private$frictionless_json_name)
            p <- frictionless::read_package (path_json)
            p$metadata <- self$metadata
            if (path_is_local) {
                op <- options (
                    readr.show_progress = FALSE,
                    readr.show_col_types = FALSE
                )
                frictionless::write_package (p, path)
                options (op)
            } else {
                p$directory <- NULL
                jsonlite::write_json (p, path_json, pretty = TRUE)
            }

            # Then finally upload new "datapackage.json":
            if (!is_deposits_test_env ()) {
                self <- private$upload_local_file (path_json, compress = "no")
            }

            if (!path_is_local) {
                fs::file_delete (path_json)
            }

            invisible (self)
        },

        #' @description Upload a local file or folder to an specified deposit,
        #' or update an existing version of file with new local version.
        #'
        #' @param path Path to local file or folder to be uploaded. If the file
        #' to be uploaded is able to be read as a tabular data file, an
        #' associated \pkg{frictionless} "datapackage.json" file will also be
        #' uploaded if it exists, or created if it does not. The metadata within
        #' a client will also be used to fill or update any metadata within the
        #' "datapackage.json" file.
        #' @param deposit_id The 'id' number of deposit which file is to be
        #' uploaded to. If not specified, the 'id' value of current deposits
        #' client is used.
        #' @param overwrite Set to `TRUE` to update existing files by
        #' overwriting.
        #' @param compress One of "no" (default), "zip", or "tar", where the
        #' latter two will compress data in the chosen binary format prior to
        #' uploading. All files are individually compressed; uploading binary
        #' archives of multiple files is not recommended, as it prevents people
        #' downloading selections of those files.
        #' @param quiet If `FALSE` (default), display diagnostic information on
        #' screen.
        #' @return (Invisibly) Updated 'deposits' client
        #' @examples
        #' \dontrun{
        #' # Initiate deposit and fill with metadata:
        #' metadata <- list (
        #'     creator = list (list (name = "P. S. Reynolds")),
        #'     created = "1994-01-01T00:00:00",
        #'     title = "Time-series analyses of beaver body temperatures",
        #'     description = "Original source of 'beaver' dataset."
        #' )
        #' cli <- depositsClient$new (
        #'     service = "zenodo",
        #'     sandbox = TRUE,
        #'     metadata = metadata
        #' )
        #' cli$deposit_new ()
        #'
        #' # Create some local data and upload to deposit:
        #' path <- fs::path (fs::path_temp (), "beaver.csv")
        #' write.csv (datasets::beaver2, path)
        #' cli$deposit_upload_file (path = path)
        #'
        #' # Confirm that uploaded files include \pkg{frictionless}
        #' # "datapackage.json" file, and also that local version has been
        #' # created:
        #' cli$hostdata$files
        #' fs::dir_ls (fs::path_temp (), regexp = "datapackage")
        #' }

        deposit_upload_file = function (path = NULL,
                                        deposit_id = NULL,
                                        overwrite = FALSE,
                                        compress = c ("no", "zip", "tar"),
                                        quiet = FALSE) {

            compress <- match.arg (compress)

            if (is.null (deposit_id)) {
                deposit_id <- self$id
            }

            checkmate::assert_int (deposit_id)
            checkmate::assert_character (path, len = 1L)
            if (!fs::is_dir (path)) {
                checkmate::assert_file_exists (path)
            }

            path <- fs::path_real (path)

            if (!fs::is_dir (path)) {

                self <- private$upload_local_file (path, overwrite, compress)
                # Create "datapackage.json" if it does not exist, or download
                # remote if only that exists. Either way, local version is then
                # the most up-to-date version.
                if (fs::path_file (path) != private$frictionless_json_name) {
                    self <- private$update_frictionless (
                        path,
                        overwrite = overwrite
                    )
                }

            } else {

                flist <- fs::dir_ls (path)
                flist <- flist [which (!grepl ("datapackage\\.json$", flist))]
                for (f in flist) {
                    self <- private$upload_local_file (
                        f,
                        overwrite,
                        compress
                    )
                    self <- private$update_frictionless (
                        f,
                        overwrite = overwrite
                    )
                }
            }

            # client metadata is then stored in "datapackage.json", so no longer
            # need to store in host metadata fields:
            private$remove_dcmi2host ()

            invisible (self)
        },

        #' @description Update 'deposits' item of current deposits for given
        #' service. The list of deposits contained within the "deposits" item of
        #' a client may not be up-to-date; this method can be used for force
        #' synchronisation with the external service, so that "deposits" lists
        #' all current deposits.
        #' @return (Invisibly) Updated 'deposits' client
        #' @examples
        #' \dontrun{
        #' cli <- depositsClient$new (service = "zenodo", sandbox = TRUE)
        #' print (cli)
        #' # ... then if "Current deposits" does not seem up-to-date:
        #' cli$deposits_list ()
        #' # That will ensure that all external deposits are then listed,
        #' # and can be viewed with:
        #' cli$deposits
        #' }

        deposits_list = function () {

            self <- private$deposits_list_extract ()

            invisible (self)
        },

        #' @description List public methods of a 'deposits' client.
        #' @return Nothing; methods are listed on screen.

        deposits_methods = function () {

            m <- sort (grep ("^deposit", ls (self), value = TRUE))
            fns <- vapply (m, function (i) {
                class (self [[i]]) [1] == "function"
            }, logical (1L))
            m <- m [which (fns)]
            cat (
                "List of methods for a deposits client:\n\n",
                paste0 ("  - ", m, "\n"),
                "\n",
                "see `?depositsClient` for full details of all methods.\n"
            )

            invisible (self)
        },

        #' @description Search all public deposits.
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
        #' # The 'search_string' can be used to specify precise searches:
        #' cli <- depositsClient$new (service = "zenodo")
        #' search_results <-
        #'    cli$deposits_search ("keywords='frictionlessdata'&type='dataset'")
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
        }
    ) # end public list
)
