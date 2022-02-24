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
        #' @param headers Any acceptable headers. See examples
        #' @return A new `depositsClient` object
        initialize = function (name, metadata = NULL, sandbox = FALSE, headers = NULL) {

            name <- match.arg (tolower (name), c ("zenodo", "figshare"))
            if (sandbox & name == "zenodo") {
                name <- "zenodo-sandbox"
            }

            s <- deposits_services ()
            if (!name %in% s$name) {
                stop ("'name' must be one of [",
                      paste0 (s$name, collapse = ", "), "]",
                      call. = FALSE)
            }
            self$name <- name
            self$url <- s$api_base_url [s$name == name]

            # This accesses only the token endpoint of figshare for
            # authorisation purposes only.
            # TODO: Extend to all other endpoints.
            if (name == "figshare") {
                self$url <- paste0 (self$url, "token")
            }

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
                    stop ("metadata is not valid - see details via metadata$validate()")
                }
                self$metadata <- metadata
            }
        },

        #' @description print method for the `depositsClient` class
        #' @param x self
        #' @param ... ignored
        print = function (x, ...) {

            cat ("<deposits client>", sep = "\n")
            cat (paste0("  name: ", self$name), sep = "\n")
            cat (paste0("  url : ", self$url), sep = "\n")
        },

        #' @description ping a deposits server
        #' @param ... curl options passed on to [crul::verb-HEAD]
        #' @return `TRUE` if successful response, `FALSE` otherwise
        ping = function(...) {

            if (self$name != "figshare") {
                res <- deposits_HEAD (self$url, self$headers, ...)
            } else {
                res <- deposits_HEAD (self$url, unlist (self$headers), ...)
            }
            res$success ()
        },

        #' @description List own deposits for given service
        #' @param ... curl options passed on to [crul::verb-HEAD]
        #' @return A list of deposits.
        list_deposits = function(...) {

            self$url <- gsub ("token$", "", self$url)
            url <- paste0 (self$url,
                           ifelse (self$name == "figshare",
                                   "account/articles",
                                   "deposit/depositions"))

            con <- crul::HttpClient$new (url,
                                         headers = self$headers,
                                         opts = list (...))

            res <- con$get ()
            if (!identical (res$status_code, 200)) {
                stop (res$parse ())
            }
            jsonlite::fromJSON (res$parse (encoding = "UTF-8"))
        },

        #' @description Fill deposits client with metadata
        #' @param metadata An \pkg{atom4R} `DCEntry` object containing metadata,
        #' either constructed directly via \pkg{atom4R} routines, or via
        #' \link{deposits_meta_to_dcmi}.
        #' @return Modified form of the deposits client with metadata inserted.
        fill_metadata = function(metadata) {
            out <- capture.output (
                chk <- metadata$validate ()
                )
            if (!chk) {
                stop ("metadata is not valid - see details via metadata$validate()")
            }
            self$metadata <- metadata
            return (self)
        },

        #' @description Create a new deposit
        #' @return A \pkg{crul} response object. Results can be extracted with
        #' `jsonlite::fromJSON(result$parse(format="UTF-8"))`
        new_deposit = function () {
            if (length (self$metadata) == 0L) {
                stop ("No metadata present; use 'fill_metadata()' first.")
            }
            terms <- construct_data_list (self$metadata, self$term_map)
            check <- validate_zenodo_terms (terms)
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
        }

    ) # end public list
)
