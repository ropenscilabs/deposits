#' @title depositsClient
#'
#' @description R6 class for constructing deposits queries
#' @return a `depositsClient` class (R6 class)
#' @examples
#' \dontrun{
#' # make a client
#' d <- depositsClient$new ("zenodo") # or:
#' d <- depositsClient$new ("figshare")
#'
#' # methods
#' d$list_deposits ()
#' }
#' @family client
#' @export
depositsClient <- R6::R6Class( # nolint (not snake_case)

    "depositsClient",
    portable = TRUE,
    cloneable = FALSE,

    private = list (
    ), # end private list

    public = list (

        #' @field name (character) of deposits server
        name = NULL,
        #' @field sandbox Connect client with sandbox if `TRUE` (zenodo only)
        sandbox = FALSE,
        #' @field url (character) list of fragments
        url = NULL,
        #' @field headers list of named headers
        headers = NULL,
        #' @field schema holds schema
        schema = NULL,
        #' @field result holds result from http request
        result = NULL,
        #' @field metadata holds metadata
        metadata = NULL,
        #' @field term_map Map between DCMI and deposit terms
        term_map = NULL,

        #' @description Create a new `depositsClient` object
        #' @param name (character) of a deposits service (see
        #' \link{deposits_services}).
        #' @param metadata An \pkg{atom4R} `DCEntry` object containing metadata,
        #' either constructed directly via \pkg{atom4R} routines, or via
        #' @param sandbox If `TRUE`, connect client to sandbox, rather than
        #' actual API endpoint (for "zenodo" only).
        #' @param headers Any acceptable headers. See examples in \pkg{crul}
        #' package.
        #' @return A new `depositsClient` object

        initialize = function (name,
                               metadata = NULL,
                               sandbox = FALSE,
                               headers = NULL) {

            name <- match.arg (tolower (name), c ("zenodo", "figshare"))
            checkmate::assert_logical (sandbox, len = 1L)
            if (!is.null (metadata)) {
                checkmate::assert_class (meta, c ("DCEntry", "AtomEntry", "R6"))
            }

            if (sandbox && name == "zenodo") {
                name <- "zenodo-sandbox"
            }
            self$sandbox <- sandbox

            s <- deposits_services ()
            if (!name %in% s$name) {
                stop ("'name' must be one of [",
                      paste0 (s$name, collapse = ", "), "]",
                      call. = FALSE)
            }
            self$name <- name
            self$url <- s$api_base_url [s$name == name]

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
                    stop ("metadata is not valid - ",
                          "see details via metadata$validate()")
                }
                self$metadata <- metadata
            }
        },

        #' @description print method for the `depositsClient` class
        #' @param x self
        #' @param ... ignored

        print = function (x, ...) {

            cat ("<deposits client>", sep = "\n")
            cat (paste0("    name: ", self$name), sep = "\n")
            if (self$name == "zenodo") {
                cat (paste0(" sandbox: ", self$sandbox), sep = "\n")
            }
            cat (paste0("    url : ", self$url), sep = "\n")
            if (is.null (self$metadata)) {
                cat ("metadata: <none>")
            } else {
                cat ("metadata:")
                print (self$metadata)
            }
        },

        #' @description ping a deposits server to check authentication
        #' @return `TRUE` if successful response, `FALSE` otherwise

        ping = function() {

            url <- ifelse (self$name == "figshare",
                           paste0 (self$url, "token"),
                           self$url)

            con <- crul::HttpClient$new (url, headers = self$headers)
            res <- con$head ()
            res$raise_for_status ()
            res$success ()
        },

        #' @description List own deposits for given service
        #' @return A list of deposits.

        list_deposits = function () {

            url <- paste0 (self$url,
                           ifelse (self$name == "figshare",
                                   "account/articles",
                                   "deposit/depositions?size=1000"))

            con <- crul::HttpClient$new (url, headers = self$headers)

            res <- con$get ()
            res$raise_for_status ()
            jsonlite::fromJSON (res$parse (encoding = "UTF-8"))
        },

        #' @description Deleted a nominated deposit
        #' @param deposit_id Integer identifer of deposit (generally obtained from
        #' `list_deposits` method).
        #' @return A \pkg{crul} response object.

        delete_deposit = function (deposit_id = NULL) {

            checkmate::assert_int (deposit_id)

            url <- paste0 (self$url,
                           ifelse (self$name == "figshare",
                                   "account/articles",
                                   "deposit/depositions"),
                           "/",
                           deposit_id)
            con <- crul::HttpClient$new (url, headers = self$headers)
            res <- con$delete ()
            res$raise_for_status ()
            return (res)
        },

        #' @description Fill deposits client with metadata
        #' @param metadata An \pkg{atom4R} `DCEntry` object containing metadata,
        #' either constructed directly via \pkg{atom4R} routines, or via
        #' \link{deposits_meta_to_dcmi}.
        #' @return Modified form of the deposits client with metadata inserted.

        fill_metadata = function(metadata) {

            checkmate::assert_class (meta, c ("DCEntry", "AtomEntry", "R6"))

            out <- capture.output (
                chk <- metadata$validate ()
                )
            if (!chk) {
                stop ("metadata is not valid - ",
                      "see details via metadata$validate()")
            }
            self$metadata <- metadata
        },

        #' @description Create a new deposit
        #' @return A \pkg{crul} response object. Results can be extracted with
        #' `jsonlite::fromJSON(result$parse(format="UTF-8"))`

        new_deposit = function () {

            if (length (self$metadata) == 0L) {
                stop ("No metadata present; use 'fill_metadata()' first.")
            }
            terms <- construct_data_list (self$metadata, self$term_map)
            check <- validate_terms (terms, deposit = self$name)
            if (length (check) > 0L) {
                warning ("The following metadata terms do not conform:\n",
                         paste0 (check, collapse = "\n"))
            }

            url <- paste0 (self$url,
                           ifelse (self$name == "figshare",
                                   "account/articles",
                                   "deposit/depositions"))

            headers <- c (self$headers, "Content-Type" = "application/json")
            con <- crul::HttpClient$new (url, headers = headers)

            body <- paste0 (jsonlite::toJSON (terms,
                                              pretty = FALSE,
                                              auto_unbox = TRUE))
            res <- con$post (body = body)
            res$raise_for_status ()
            return (res)
        },

        #' @description Upload file to an existing deposit
        #' @param deposit_id The 'id' number of deposit which file it to be
        #' uploaded to. (generally obtained from `list_deposits` method).
        #' @param path Path to local file.
        #' @return A \pkg{crul} response object containing full data of deposit.
        #' including of uploaded file.

        upload_file = function (deposit_id, path = NULL) {

            checkmate::assert_int (deposit_id)
            if (!is.null (path)) {
                checkmate::assert_character (path, len = 1L)
                checkmate::assert_directory_exists (path)
            }

            url <- paste0 (self$url,
                           ifelse (self$name == "figshare",
                                   "account/articles",
                                   "deposit/depositions"))

            if (cli$name == "figshare") {
                # in R/upload-figshare.R
                out <- upload_figshare_file (deposit_id,
                                             url,
                                             self$headers,
                                             path)
            } else if (cli$name == "zenodo") {
                # in R/upload-zenodo.R
                out <- upload_zenodo_file (deposit_id,
                                           url,
                                           self$headers,
                                           path)
            }
            return (out)
        },

        #' @description Retrieve information on specified deposit
        #' @param deposit_id The 'id' number of deposit for which information is
        #' to be retrieved.
        #' @return A `data.frame` containing full data of specified deposit.

        retrieve_deposit = function (deposit_id) {

            checkmate::assert_int (deposit_id)

            url <- paste0 (self$url,
                           ifelse (self$name == "figshare",
                                   "account/articles/",
                                   "deposit/depositions/"),
                           deposit_id)

            con <- crul::HttpClient$new (url, headers = self$headers)
            res <- con$get ()
            res$raise_for_status ()
            jsonlite::fromJSON (res$parse (encoding = "UTF-8"))
        },

        #' @description Download a specified 'filename' from a deposit
        #' @param deposit_id The 'id' number of deposit for which information is
        #' to be retrieved.
        #' @param filename The name of the file to be download as specified in
        #' the deposit.
        #' @param path The local directory where file is to be downloaded.
        #' @param quiet If `FALSE`, display download progress.
        #' @return The full path of the downloaded file.

        download_file = function (deposit_id,
                                  filename,
                                  path = NULL,
                                  quiet = FALSE) {

            checkmate::assert_int (deposit_id)
            checkmate::assert_character (filename, len = 1L)
            if (!is.null (path)) {
                checkmate::assert_character (path, len = 1L)
                checkmate::assert_directory_exists (path)
            }
            checkmate::assert_logical (quiet, len = 1L)

            # repeat retrieve_deposit method to get download_url:
            url <- paste0 (self$url,
                           ifelse (self$name == "figshare",
                                   "account/articles/",
                                   "deposit/depositions/"),
                           deposit_id)

            con <- crul::HttpClient$new (url, headers = self$headers)
            res <- con$get ()
            res$raise_for_status ()
            x <- jsonlite::fromJSON (res$parse (encoding = "UTF-8"))

            name_field <- ifelse (self$name == "figshare",
                                  "name",
                                  "filename")
            if (!filename %in% x$files [[name_field]]) {
                stop ("That deposit does not contain the specified file.")
            }

            if (self$name == "figshare") {
                download_url <- x$files$download_url [x$files$name == filename]
                download_url <- sprintf ("%s/%s", download_url, filename)
            } else if (self$name == "zenodo") {
                download_url <-
                    x$files$links$download [x$files$filename == filename]
            } else {
                stop ("There is not deposits service named [", self$name, "]")
            }

            if (is.null (path)) {
                path <- here::here ()
            }
            destfile <- file.path (path, filename)

            h <- curl::new_handle (verbose = FALSE)
            curl::handle_setheaders(
                h,
                "Content-Type" = "application/binary",
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
