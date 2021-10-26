#' @title depositsClient
#' @description R6 class for constructing deposits queries
#' @export
#' @return a `depositsClient` class (R6 class)
#' @examples
#' x <- depositsClient$new()
#' x
#'
#' \dontrun{
#' # make a client
#' token <- Sys.getenv("ZENODO_TOKEN")
#' cli <- depositsClient$new(
#'     url = "https://zenodo.org/api",
#'     headers = list(Authorization = paste0("Bearer ", token))
#' )
#'
#' # methods
#' ## ping - hopefully you get TRUE
#' cli$ping()
#'
#' }
depositsClient <- R6::R6Class( # nolint (not snake_case)
    "depositsClient",
    portable = TRUE,
    cloneable = FALSE,

    private = list(
    ), # end private list

    public = list(
        #' @field name (character) of deposits server
        name = NULL,
        #' @field url (character) list of fragments
        url = NULL,
        #' @field headers list of named headers
        headers = NULL,
        #' @field schema holds schema
        schema = NULL,
        #' @field result holds result from http request
        result = NULL,

        #' @description Create a new `depositsClient` object
        #' @param name (character) of a deposits service (see
        #' \link{deposits_services}).
        #' @param headers Any acceptable headers. See examples
        #' @return A new `depositsClient` object
        initialize = function(name, headers = NULL) {
            if (missing(name))
                stop ("'name' may not be missing.")
            s <- deposits_services ()
            if (!name %in% s$name)
                stop ("'name' must be one of [",
                      paste0 (s$name, collapse = ", "), "]")
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
                self$headers <- list(Authorization = paste0("Bearer ", token))
            }
        },

        #' @description print method for the `depositsClient` class
        #' @param x self
        #' @param ... ignored
        print = function(x, ...) {
            cat("<deposits client>", sep = "\n")
            cat(paste0("  name: ", self$name), sep = "\n")
            cat(paste0("  url : ", self$url), sep = "\n")
        },

        #' @description ping a deposits server
        #' @param ... curl options passed on to [crul::verb-HEAD]
        #' @return `TRUE` if successful response, `FALSE` otherwise
        ping = function(...) {
            if (self$name != "figshare") {
                res <- deposits_HEAD(self$url, self$headers, ...)
            } else {
                res <- deposits_HEAD(self$url, unlist (self$headers), ...)
            }
            res$success()
        }

    ) # end public list
)
