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
    private = list (), # end private list

    public = list (

        #' @field name (character) of deposits host service.
        name = NULL,
        #' @field sandbox (logical) Connect client with sandbox if `TRUE` (zenodo only)
        sandbox = FALSE,
        #' @field url_base (character) Base URL of host service API
        url_base = NULL,
        #' @field id (integer) Deposit identifier from host service.
        id = NULL,
        #' @field headers (list) list of named headers
        headers = NULL,
        #' @field hostdata (list) Data as stored by host platform
        hostdata = NULL,
        #' @field metadata (`atom4R::DCEntry`) holds metadata
        metadata = NULL,
        #' @field term_map (`data.frame`) Map between DCMI and deposit terms for
        #' specified host service.
        term_map = NULL,

        #' @description Create a new `depositsClient` object
        #' @param name (character) of a deposits service (see
        #' \link{deposits_services}).
        #' @param metadata Either of one three possible ways of defining
        #' metadata:
        #' \itemize{
        #' \item The name (or full path) or a local file containing
        #' metadata constructed with \link{deposits_metadata_template};
        #' \item A names list of metadata with names matching values given by
        #' \link{dcmi_terms}, and values specified as individual character
        #' strings or lists for multiple entries.
        #' \item An \pkg{atom4R} `DCEntry` object containing metadata, either
        #' constructed directly via \pkg{atom4R} routines, or via
        #' \link{deposits_meta_to_dcmi}.
        #' }
        #' @param sandbox If `TRUE`, connect client to sandbox, rather than
        #' actual API endpoint (for "zenodo" only).
        #' @param headers Any acceptable headers. See examples in \pkg{httr2}
        #' package.
        #' @return A new `depositsClient` object

        initialize = function (name,
                               metadata = NULL,
                               sandbox = FALSE,
                               headers = NULL) {

            name <- match.arg (tolower (name), c ("zenodo", "figshare"))
            checkmate::assert_logical (sandbox, len = 1L)

            if (!is.null (metadata)) {
                metadata <- process_metadata_param (metadata)
            }

            if (sandbox && name == "zenodo") {
                name <- "zenodo-sandbox"
            }
            self$sandbox <- sandbox

            s <- deposits_services ()
            self$name <- name
            self$url_base <- s$api_base_url [s$name == name]

            if (is.null (headers)) {
                token <- get_deposits_token (service = self$name)
                self$headers <- list (Authorization = paste0 ("Bearer ", token))
            }

            if (self$name == "zenodo-sandbox") {
                self$name <- "zenodo"
            }
            self$term_map <- get_dcmi_term_map (self$name)

            if (!is.null (metadata)) {
                out <- capture.output (
                    chk <- metadata$validate ()
                )
                if (!chk) {
                    stop (
                        "metadata is not valid - ",
                        "see details via metadata$validate()"
                    )
                }
                self$metadata <- metadata
            }
        },

        #' @description print method for the `depositsClient` class
        #' @param x self
        #' @param ... ignored

        print = function (x, ...) {

            cat ("<deposits client>", sep = "\n")
            cat (paste0 ("       name : ", self$name), sep = "\n")
            if (self$name == "zenodo") {
                cat (paste0 ("     sandbox: ", self$sandbox), sep = "\n")
            }
            cat (paste0 ("   url_base : ", self$url_base), sep = "\n")
            if (!is.null (self$id)) {
                cat (paste0 (" deposit id : ", self$id), sep = "\n")
            }

            if (is.null (self$hostdata)) {
                cat ("   hostdata : <none>\n")
            } else {
                cat ("   hostdata : list with", length (self$hostdata), " elements\n")
            }

            if (is.null (self$metadata)) {
                cat ("   metadata : <none>\n")
            } else {
                cat ("\n   metadata : ")
                print (self$metadata)
                cat ("\n")
            }
        },

        #' @description ping a deposits server to check authentication
        #' @return `TRUE` if successful response, `FALSE` otherwise

        deposit_authenticate = function () {

            url <- ifelse (self$name == "figshare",
                paste0 (self$url_base, "token"),
                self$url_base
            )

            req <- create_httr2_helper (url, self$headers$Authorization, "GET")
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            return (!httr2::resp_is_error (resp))
        },

        #' @description List own deposits for given service
        #' @return A list of deposits.

        deposits_list = function () {

            url <- paste0 (
                self$url_base,
                ifelse (self$name == "figshare",
                    "account/articles",
                    "deposit/depositions?size=1000"
                )
            )

            req <- create_httr2_helper (url, self$headers$Authorization, "GET")
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            httr2::resp_body_json (resp, simplifyVector = TRUE)
        },

        #' @description Deleted a nominated deposit
        #' @param deposit_id Integer identifier of deposit (generally obtained
        #' from `list_deposits` method).
        #' @return `TRUE` is deposit successfully deleted, otherwise `FALSE`.

        deposit_delete = function (deposit_id = NULL) {

            checkmate::assert_int (deposit_id)

            url <- paste0 (
                self$url_base,
                ifelse (self$name == "figshare",
                    "account/articles",
                    "deposit/depositions"
                ),
                "/",
                deposit_id
            )

            req <- create_httr2_helper (
                url,
                self$headers$Authorization,
                "DELETE"
            )
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            return (httr2::resp_status (resp) == 204L)
        },

        #' @description Fill deposits client with metadata
        #' @param metadata Either of one three possible ways of defining
        #' metadata:
        #' \itemize{
        #' \item The name (or full path) or a local file containing
        #' metadata constructed with \link{deposits_metadata_template};
        #' \item A names list of metadata with names matching values given by
        #' \link{dcmi_terms}, and values specified as individual character
        #' strings or lists for multiple entries.
        #' \item An \pkg{atom4R} `DCEntry` object containing metadata, either
        #' constructed directly via \pkg{atom4R} routines, or via
        #' \link{deposits_meta_to_dcmi}.
        #' }
        #' @return Modified form of the deposits client with metadata inserted.

        deposit_fill_metadata = function (metadata = NULL) {

            metadata <- process_metadata_param (metadata)
            self$metadata <- metadata
        },

        #' @description Create a new deposit
        #' @return A `data.frame` with details of the newly created deposit.

        deposit_new = function () {

            if (length (self$metadata) == 0L) {
                stop ("No metadata present; use 'fill_metadata()' first.")
            }
            metaterms <- construct_data_list (self$metadata, self$term_map)
            if (length (metaterms) == 0L) {
                stop (
                    "metadata is empty; please fill template or use ",
                    "`atom4R` methods described in vignette"
                )
            }
            check <- validate_terms (metaterms, deposit = self$name)
            if (length (check) > 0L) {
                warning (
                    "The following metadata terms do not conform:\n",
                    paste0 (check, collapse = "\n")
                )
            }

            body <- jsonlite::toJSON (metaterms, pretty = FALSE, auto_unbox = TRUE)

            url <- paste0 (
                self$url_base,
                ifelse (self$name == "figshare",
                    "account/articles",
                    "deposit/depositions"
                )
            )

            req <- create_httr2_helper (url, self$headers$Authorization, "POST")
            req$headers <- c (req$headers, "Content-Type" = "application/json")
            req <- httr2::req_body_raw (req, body = paste0 (body))

            resp <- httr2::req_perform (req)

            self$hostdata <- httr2::resp_body_json (resp)

            if (self$name == "figshare") {
                self$id <- self$hostdata$entity_id
            } else if (self$name == "zenodo") {
                self$id <- self$hostdata$id
            }

            invisible (self)
        },

        #' @description Update metadata for specified deposit
        #' @note Client should already contain metadata updated with the
        #' 'fill_metadata()' function.
        #' @param deposit_id The 'id' number of deposit to update.
        #' @return A `data.frame` with details of newly updated deposit

        deposit_update = function (deposit_id) {

            checkmate::assert_int (deposit_id)

            url <- paste0 (
                self$url_base,
                ifelse (self$name == "figshare",
                    "account/articles/",
                    "deposit/depositions/"
                ),
                deposit_id
            )

            req <- create_httr2_helper (url, self$headers$Authorization, "PUT")
            req$headers <- c (req$headers, "Content-Type" = "application/json")

            metaterms <- construct_data_list (self$metadata, self$term_map)
            body <- paste0 (jsonlite::toJSON (metaterms,
                pretty = FALSE,
                auto_unbox = TRUE
            ))

            req <- httr2::req_body_raw (req, body = paste0 (body))

            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            httr2::resp_body_json (resp)
        },

        #' @description Upload file to an existing deposit
        #' @param deposit_id The 'id' number of deposit which file it to be
        #' uploaded to. (generally obtained from `list_deposits` method).
        #' @param path Path to local file.
        #' @return A `data.frame` with details on newly uploaded file, including
        #' upload and download URLs, and file details.

        deposit_upload_file = function (deposit_id, path = NULL) {

            checkmate::assert_int (deposit_id)
            if (!is.null (path)) {
                checkmate::assert_character (path, len = 1L)
                checkmate::assert_directory_exists (dirname (path))
            }

            url <- paste0 (
                self$url_base,
                ifelse (self$name == "figshare",
                    "account/articles",
                    "deposit/depositions"
                )
            )

            if (self$name == "figshare") {

                # in R/upload-figshare.R
                res <- upload_figshare_file (
                    deposit_id,
                    url,
                    self$headers,
                    path
                )

            } else if (self$name == "zenodo") {

                # in R/upload-zenodo.R
                res <- upload_zenodo_file (
                    deposit_id,
                    url,
                    self$headers,
                    path
                )

            }

            return (res)
        },

        #' @description Retrieve information on specified deposit
        #' @param deposit_id The 'id' number of deposit for which information is
        #' to be retrieved.
        #' @return A `data.frame` containing full data of specified deposit.

        deposit_retrieve = function (deposit_id) {

            checkmate::assert_int (deposit_id)

            url <- paste0 (
                self$url_base,
                ifelse (self$name == "figshare",
                    "account/articles/",
                    "deposit/depositions/"
                ),
                deposit_id
            )

            req <- create_httr2_helper (url, self$headers$Authorization, "GET")
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            dep <- httr2::resp_body_json (resp, simplifyVector = TRUE)

            cli <- metadata_from_deposit (self, dep)

            invisible (cli)
        },

        #' @description Download a specified 'filename' from a deposit
        #' @param deposit_id The 'id' number of deposit for which information is
        #' to be retrieved.
        #' @param filename The name of the file to be download as specified in
        #' the deposit.
        #' @param path The local directory where file is to be downloaded.
        #' @param overwrite Do not overwrite existing files unless set to
        #' `TRUE`.
        #' @param quiet If `FALSE`, display download progress.
        #' @return The full path of the downloaded file.

        deposit_download_file = function (deposit_id,
                                          filename,
                                          path = NULL,
                                          overwrite = FALSE,
                                          quiet = FALSE) {

            checkmate::assert_int (deposit_id)
            checkmate::assert_character (filename, len = 1L)
            if (!is.null (path)) {
                checkmate::assert_character (path, len = 1L)
                checkmate::assert_directory_exists (path)
            }
            checkmate::assert_logical (quiet, len = 1L)

            # repeat retrieve_deposit method to get download_url:
            url <- paste0 (
                self$url_base,
                ifelse (self$name == "figshare",
                    "account/articles/",
                    "deposit/depositions/"
                ),
                deposit_id
            )

            req <- create_httr2_helper (url, self$headers$Authorization, "GET")
            resp <- httr2::req_perform (req)
            httr2::resp_check_status (resp)

            x <- httr2::resp_body_json (resp, simplifyVector = TRUE)

            name_field <- ifelse (self$name == "figshare",
                "name",
                "filename"
            )
            if (!filename %in% x$files [[name_field]]) {
                stop ("That deposit does not contain the specified file.")
            }

            if (self$name == "figshare") {
                download_url <- x$files$download_url [x$files$name == filename]
            } else if (self$name == "zenodo") {
                download_url <-
                    x$files$links$download [x$files$filename == filename]
            } else {
                stop ("There is not deposits service named [", self$name, "]")
            }

            if (self$name == "figshare" & !x$is_public) {
                stop (
                    "Figshare only enables automated downloads of public ",
                    "files.\nYou can manually download at ", download_url
                )
            }


            if (is.null (path)) {
                path <- here::here ()
            }
            destfile <- normalizePath (file.path (path, filename),
                mustWork = FALSE
            )
            if (file.exists (destfile) & !overwrite) {
                stop (
                    "File [", destfile, "] exists; either remove ",
                    "or pass `overwrite = TRUE`."
                )
            }

            h <- curl::new_handle (verbose = FALSE)
            curl::handle_setheaders (
                h,
                "Content-Type" = "application/octet-stream",
                "Authorization" = self$headers$Authorization
            )
            chk <- curl::curl_download (
                url = download_url,
                destfile = destfile,
                quiet = quiet,
                handle = h,
                mode = "wb"
            )

            return (chk)
        }
    ) # end public list
)
