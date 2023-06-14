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
#'     creator = list (list (name = "A. Person"), list (name = "B. Person"))
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
        term_map = NULL,
        # @field dl_frictionless (logical) Used to control whether remote
        # ":datapackage.json" should be downloaded and used to update client
        # metadata.
        dl_frictionless = TRUE,
        # @field num_resources_local Number of resources in local
        # "datapackage.json" file.
        num_resources_local = 0L,
        # @field num_resources_remote Number of resources ("files") listed on
        # remote service (excluding "datapackage.json").
        num_resources_remote = 0L

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
        #' @field local_path holds path to local directory (not file) containing
        #' current deposit.
        local_path = NULL,

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
        #'
        #' # Once a deposit has locally-stored metadata associated with it, only
        #' # that parameter is needed.
        #' path <- tempfile (pattern = "data") # A directory for data storage
        #' dir.create (path)
        #' f <- file.path (path, "beaver1.csv")
        #' write.csv (datasets::beaver1, f, row.names = FALSE)
        #' metadata <- list (
        #'     creator = list (list (name = "P. S. Reynolds")),
        #'     created = list (publisherPublication = "1994-01-01"),
        #'     title = "Time-series analyses of beaver body temperatures",
        #'     description = "Original source of 'beaver' dataset."
        #' )
        #' cli <- depositsClient$new (service = "figshare", metadata = metadata)
        #' cli$deposit_new ()
        #' cli$deposit_upload_file (f)
        #'
        #' # A new deposits client may then be constructed by passing the data
        #' # directory as the 'metadata' parameter:
        #' cli <- depositsClient$new (metadata = path)
        #' }

        initialize = function (service,
                               metadata = NULL,
                               sandbox = FALSE,
                               headers = NULL) {

            # If no 'service' specified, see if it is in 'metadata':
            if (!is.null (metadata)) {
                metadata_dcmi <- validate_dcmi_metadata (metadata, service)
                service_tmp <- service_from_metadata (metadata_dcmi, service)
                if (missing (service)) {
                    service <- service_tmp
                } else if (!is.null (service_tmp)) {
                    if (service != service_tmp) {
                        stop (
                            "Metadata are for a '",
                            service_tmp,
                            "' service, not for '",
                            service,
                            "'",
                            call. = FALSE
                        )
                    }
                }
            }

            self <- private$define_service (service, sandbox)

            if (is.null (headers)) {
                service <- add_service_sandbox (self$service, self$sandbox)
                token <- get_deposits_token (service = service)
                self$headers <- list (Authorization = paste0 ("Bearer ", token))
            }

            private$deposits_list_extract ()

            if (!is.null (metadata)) {

                meta_source <- metadata
                metadata <- validate_metadata (
                    metadata,
                    gsub ("\\-sandbox$", "", self$service)
                )
                if (!is.null (metadata$local_path)) {
                    self$local_path <- metadata$local_path
                }
                if (metadata$num_resources_local > 0L) {
                    private$num_resources_local <- metadata$num_resources_local
                }
                metadata <- httptest2_created_timestamp (metadata)
                self$metadata <- metadata$dcmi
                private$metadata_service <- metadata$service

                private$servicedata_from_dp (meta_source)
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

            if (!is.null (self$local_path)) {
                cat (paste0 (" local_path : ", self$local_path), sep = "\n")
            }
            if (private$num_resources_local > 0L ||
                private$num_resources_remote > 0L) {
                cat (paste0 (
                    "  resources : ",
                    private$num_resources_local,
                    " local, ",
                    private$num_resources_remote,
                    " remote",
                    sep = "\n"
                ))
            }
        },

        # -----------
        # From here on, all methods are defined ain alphabetical order
        # ---------

        #' @description Generate a local "datapackage.json" file, and/or add
        #' metadata from client.
        #' @param path Path to local resource to be added to client. May name an
        #' individual file or a directory.
        #' @return (Invisibly) Updated 'deposits' client

        deposit_add_resource = function (path) {

            checkmate::assert_character (path, len = 1L)
            if (fs::is_dir (path)) {
                path_resource <- fs::dir_ls (path)
            } else {
                checkmate::assert_file_exists (path)
                path_resource <- path
                path <- fs::path_dir (path)
            }
            self$local_path <- path
            private$count_num_resources () # count resources in dp.json

            path <- fs::path_real (path)
            path_resource <- fs::path_real (path_resource)

            path_dp <- fs::path (path, private$frictionless_json_name)
            path_resource <- path_resource [which (!path_resource == path_dp)]

            if (!fs::file_exists (path_dp)) {
                if (length (path_resource) == 0L) {
                    stop (
                        "'path' must contain at least one resource or file",
                        call. = FALSE
                    )
                }
                private$generate_frictionless (path_resource [1])
                private$add_meta_to_dp_json (path)
                path_resource <- path_resource [-1]
                if (length (path_resource) > 0L) {
                    for (f in path_resource) {
                        private$update_frictionless (f, quiet = TRUE)
                    }
                }
            } else {
                quiet <- FALSE
                for (f in path_resource) {
                    private$update_frictionless (f, quiet = quiet)
                    quiet <- TRUE # Only issue message once
                }
            }

            # Then update metadata from dp_json, but using jsonlite because
            # frictionless defaults to `simplifyVector = TRUE`.
            p <- jsonlite::read_json (path_dp, simplifyVector = FALSE)
            if ("metadata" %in% names (p)) {
                self$deposit_fill_metadata (p$metadata)
            }

            private$count_num_resources ()

            invisible (self)
        },

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

            url <- get_service_url (
                self,
                deposit_id = deposit_id,
                private = TRUE
            )

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

            # Finally remove 'id' if that's been set
            if (!is.null (self$id)) {
                if (self$id == deposit_id) {
                    self$id <- self$hostdata <- self$url_service <- NULL
                }
            }

            private$num_resources_remote <- 0L
            private$count_num_resources () # Will still count local resources

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
        #'     title = "Time-series analyses of beaver body temperatures",
        #'     description = "Original source of 'beaver2' data",
        #'     creator = list (list (name = "P.S. Reynolds")),
        #'     created = "1994-01-01T00:00:00",
        #'     publisher = "Case Studies in Biometry"
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
        #'
        #' # Then delete one of those files:
        #' cli$deposit_delete_file ("datapackage.json")
        #' }

        deposit_delete_file = function (filename) {

            checkmate::assert_character (filename, len = 1L)

            self <- private$delete_file (filename)

            private$count_num_resources ()

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
                if (!is.null (self$local_path)) {
                    message (
                        "File will be downloaded to client 'local_path': [",
                        self$local_path,
                        "]"
                    )
                    path <- self$local_path
                } else {
                    path <- fs::path (here::here ())
                }
            } else {
                checkmate::assert_character (path, len = 1L)
                checkmate::assert_directory_exists (path)
                path <- fs::path_real (path)
            }
            self$local_path <- path
            private$count_num_resources ()

            checkmate::assert_logical (quiet, len = 1L)

            # repeat retrieve_deposit method to get download_url:
            name_field <- service_filename_field (self$service)

            files <- private$get_hostdata_files (deposit_id, filename)

            if (!filename %in% files [[name_field]]) {
                stop ("That deposit does not contain the specified file.")
            }

            download_url <- service_download_url (self$service, files, filename)

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

            meta_source <- metadata

            metadata <- validate_metadata (
                metadata,
                gsub ("\\-sandbox$", "", self$service)
            )
            if (!is.null (metadata$local_path)) {
                self$local_path <- metadata$local_path
                private$count_num_resources ()
            }

            metadata <- httptest2_created_timestamp (metadata)
            self$metadata <- metadata$dcmi

            private$servicedata_from_dp (meta_source)

            invisible (self)
        },

        #' @description Initiate a new deposit on the external deposits service.
        #' @param prereserve_doi If `TRUE`, a Digital Object Identifier (DOI) is
        #' prereserved on the nominated service, and returned in the "hostdata".
        #' This DOI will also be inserted in the "identifier" field of the
        #' client metadata.
        #' @param quiet If `FALSE` (default), print integer identifier of newly
        #' created deposit.
        #' @return (Invisibly) Updated deposits client which includes data on
        #' new deposit

        deposit_new = function (prereserve_doi = TRUE, quiet = FALSE) {

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
            if (gsub ("\\-sandbox$", "", self$service) == "zenodo") {
                private$metadata_service$metadata$prereserve_doi <-
                    prereserve_doi
            }

            # Insert 'self$metadata' into host parameter (#65):
            private$dcmi2host ()

            url <- get_service_url (self, private = TRUE)

            req <- create_httr2_helper (url, self$headers$Authorization, "POST")
            req <- httr2::req_body_json (req, data = private$metadata_service)

            resp <- httr2::req_perform (req)

            hostdata <- httr2::resp_body_json (resp)

            if (self$service == "figshare") {
                if (prereserve_doi) {
                    doi <- private$prereserve_doi (hostdata$entity_id)
                }
                self$deposit_retrieve (hostdata$entity_id)
            } else if (self$service == "zenodo") {
                self$hostdata <- hostdata
            }

            if (prereserve_doi) {
                private$add_doi_to_metadata ()
            }

            self <- private$fill_service_id_url ()

            self <- private$deposits_list_extract ()

            if (!quiet) {
                cat ("ID of new deposit :", self$id, "\n")
            }

            invisible (self)
        },

        #' @description Prereserve a DOI. This is generally done when a deposit
        #' is first initialised, via the `prereserve_doi` parameter. This method
        #' exists only to subsequently prereserve a DOI for deposits initiated
        #' with `prereserve_doi = FALSE`.
        #' @return (Invisibly) Updated 'deposits' client

        deposit_prereserve_doi = function () {

            if (!self$service == "figshare") {
                message ("This method only has effect for Figshare")
                return (invisible (self))
            }

            if (nzchar (self$hostdata$doi)) {
                message ("This deposit already has a DOI")
                return (invisible (self))
            }

            private$prereserve_doi (self$id)
            self$deposit_retrieve (self$id)

            invisible (self)
        },


        #' @description Publish a deposit. This is an irreversible action which
        #' should only be called if you are really sure that you want to publish
        #' the deposit. Some aspects of published deposits can be subsequently
        #' edited, but they can never be deleted.
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

            is_embargoed <-
                service_is_deposit_embargboed (self$hostdata, self$service)

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

        #' @description Retrieve a specified deposit and store information in
        #' local client.
        #' @param deposit_id The 'id' number of deposit for which information is
        #' to be retrieved.
        #' @param quiet If `FALSE` (default), display information on screen on
        #' any issues encountered in retrieving deposit.
        #' @return (Invisibly) Updated 'deposits' client

        deposit_retrieve = function (deposit_id, quiet = FALSE) {

            checkmate::assert_int (deposit_id)

            url <- get_service_url (
                self,
                deposit_id = deposit_id,
                private = FALSE
            )

            req <- create_httr2_helper (url, self$headers$Authorization, "GET")
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            hostdata <- httr2::resp_body_json (resp, simplifyVector = TRUE)
            hostdata <- httptest2_hostdata_timestamps (hostdata, self$service)
            self$hostdata <- hostdata

            has_frictionless <- host_has_frictionless (
                self$service,
                self$hostdata,
                private$frictionless_json_name,
                service_filename_field (self$service)
            )

            if (!has_frictionless) {
                private$host2dcmi_internal ()
                self$frictionless <- length (self$metadata) > 0L
            } else if (self$service == "figshare" && !self$hostdata$is_public) {
                if (!quiet) {
                    message (
                        "Files for private Figshare deposits can only be ",
                        "downloaded manually; no metadata can be retrieved ",
                        "for this deposit."
                    )
                }
            } else if (private$dl_frictionless) {
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
                    # can't read via frictionless because that defaults to
                    # `simplifyVector = TRUE`.
                    self$metadata <- jsonlite::read_json (
                        dp_path,
                        simplifyVector = FALSE
                    )$metadata
                    # only delete if 'datapackage.json' created here:
                    if (!dp_exists) {
                        fs::file_delete (dp_path)
                    }
                }
            }

            private$count_num_resources ()

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
                service <- add_service_sandbox (self$service, sellf$sandbox)
                token <- get_deposits_token (service = service)
                self$headers <- list (Authorization = paste0 ("Bearer ", token))
            }

            private$deposits_list_extract ()

            self$hostdata <- NULL

            invisible (self)
        },

        #' @description Update a remote (online) deposit with local metadata.
        #'
        #' @note This method is generally intended to be used for private
        #' deposits; that is, to edit deposits prior to publication. It is
        #' nevertheless possible to edit published deposits on Zenodo, and this
        #' method will do so if called on a public Zenodo deposit. The updated
        #' data and/or metadata will not be publicly visible until the deposit
        #' is again published with the `deposit_publish()` method.
        #'
        #' @param deposit_id (Optional) The 'id' number of deposit to update. If
        #' not specified, the 'id' value of current deposits client is used.
        #' @param path (Optional) If given as path to single file, update that
        #' file on remote service. If given as a directory, update all files
        #' within that directory on remote service. If not given, path will be
        #' taken from client's "local_path" field. Only files for which local
        #' versions have been changed will be uploaded.
        #' @return (Invisibly) Updated deposits client.

        deposit_update = function (deposit_id = NULL, path = NULL) {

            if (is.null (deposit_id)) {
                deposit_id <- self$id
            }
            if (!is.null (deposit_id)) {
                checkmate::assert_int (deposit_id)

                url <- get_service_url (self, deposit_id = deposit_id, private = TRUE)

                req <- create_httr2_helper (
                    url,
                    self$headers$Authorization,
                    "PUT"
                )
                req$headers <- c (
                    req$headers,
                    "Content-Type" = "application/json"
                )
            }

            if (is.null (path) && !is.null (self$local_path)) {
                path <- self$local_path
            }

            if (!is.null (path) && (fs::is_dir (path) ||
                fs::path_file (path) == private$frictionless_json_name)) {

                self$deposit_fill_metadata (metadata = path)
            }

            # Re-generate service metadata:
            metadata_service <- validate_metadata (
                self$metadata,
                service = gsub ("\\-sandbox$", "", self$service)
            )

            local_path <- metadata_service$local_path
            if (!is.null (local_path) && !is_dcf (local_path) &&
                !identical (local_path, self$local_path)) {
                self$local_path <- local_path
                private$count_num_resources ()
            }

            metadata_service <- metadata_service$service
            metadata_service <- httptest2_created_timestamp (metadata_service)
            metadata_service <-
                httptest2_hostdata_timestamps (metadata_service, self$service)

            if (!is.null (deposit_id)) {

                # Unlock Zenodo deposit for editing if needed:
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

                # Plus Figshare doesn't seem to accept email address in author
                # lists on update method. It dumps them from the hostdata
                # structures anyway, so they're just removed here:
                if (self$service == "figshare") {
                    metadata_service$authors <- lapply (
                        metadata_service$authors, function (i) {
                            i$email <- NULL
                            return (i)
                        }
                    )
                }

                req <- httr2::req_body_json (req, data = metadata_service)

                resp <- httr2::req_perform (req)
                httr2::resp_check_status (resp)

                if (!is.null (path)) {
                    private$update_files (path)
                }

                # This prevents retrieval from updating client metadata with
                # contents of remote "datapackage.json":
                private$dl_frictionless <- FALSE
                self <- self$deposit_retrieve (deposit_id)
                private$dl_frictionless <- TRUE # default

                if (!is.null (path)) {
                    # This generates a warning if local dp differs from client
                    # metadata, but that warning should not be issued if update
                    # method is called on local-only deposit, and that updating
                    # is presumably the desired effect.
                    private$compare_dpjson_to_meta (path)
                }
            }

            private$count_num_resources ()

            invisible (self)
        },

        #' @description Upload a local file or folder to an specified deposit,
        #' or update an existing version of file with new local version.
        #'
        #' @param path Either single file name or full path to local file or
        #' folder to be uploaded. If a single file name, the path if taken from
        #' the client's "local_path" field. If the file to be uploaded is able
        #' to be read as a tabular data file, an associated \pkg{frictionless}
        #' "datapackage.json" file will also be uploaded if it exists, or
        #' created if it does not. The metadata within a client will also be
        #' used to fill or update any metadata within the "datapackage.json"
        #' file.
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
        #'     title = "Time-series analyses of beaver body temperatures",
        #'     description = "Original source of 'beaver2' data",
        #'     creator = list (list (name = "P.S. Reynolds")),
        #'     created = "1994-01-01T00:00:00",
        #'     publisher = "Case Studies in Biometry"
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

            if (is.null (path)) {
                # Upload entire directory in compressed format
                if (compress == "no") {
                    stop (
                        "Entire directories can only be ",
                        "uploaded in compressed format",
                        call. = FALSE
                    )
                }
                path <- self$local_path

                td <- fs::file_temp (pattern = "deposits_temppath_")
                fs::dir_create (td)
                fs::dir_copy (path, td)
                td <- fs::path (td, fs::path_file (path))

                if (is_dcf (path) && compress == "tar") {
                    compress <- "rpkg"
                }

                path_compressed <- compress_local_file (td, compress)
                private$upload_local_file (
                    path_compressed,
                    overwrite = TRUE,
                    compress = "no"
                )

            } else {

                checkmate::assert_character (path, len = 1L)
                if (!fs::is_dir (path)) {
                    is_filename <- length (fs::path_split (path) [[1]]) == 1L
                    if (is_filename && !is.null (self$local_path)) {
                        path <- fs::path (self$local_path, path)
                    }
                    checkmate::assert_file_exists (path)
                }

                path <- fs::path_real (path)

                if (is_dcf (path)) {
                    warning (
                        "'path' should generally not be specified for ",
                        "uploading software; calling again without path will",
                        "upload the entire repository as a single compressed ",
                        "file.",
                        call. = FALSE
                    )
                }

                if (!fs::is_dir (path)) {

                    self <- private$upload_local_file (
                        path,
                        overwrite,
                        compress
                    )
                    # Create "datapackage.json" if it does not exist, or
                    # download remote if only that exists. Either way, local
                    # version is then the most up-to-date version.
                    if (fs::path_file (path) !=
                        private$frictionless_json_name) {

                        self <- private$update_frictionless (
                            path,
                            overwrite = overwrite
                        )
                    }

                    path <- fs::path_dir (path)

                } else {

                    flist <- fs::dir_ls (path)
                    flist <-
                        flist [which (!grepl ("datapackage\\.json$", flist))]
                    for (f in flist) {
                        self <- private$upload_local_file (
                            f,
                            overwrite,
                            compress
                        )
                        self <- private$update_frictionless (
                            f,
                            overwrite = TRUE
                            # force updates after new resources added.
                        )
                    }
                }
            }

            self$local_path <- path

            # client metadata is then stored in "datapackage.json", so no longer
            # need to store in host metadata fields:
            private$remove_dcmi2host ()

            private$count_num_resources ()

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
