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
        #' @param headers Any acceptable headers, a named list. See examples
        #' @return A new `depositsClient` object
        initialize = function(name, headers) {
            if (missing(name))
                stop ("'name' may not be missing.")
            s <- deposits_services ()
            if (!name %in% s$name)
                stop ("'name' must be one of [",
                      paste0 (s$name, collapse = ", "), "]")
            self$name <- name
            self$url <- s$api_base_url [s$name == name]

            if (!missing(headers)) self$headers <- headers
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
            res <- deposits_HEAD(self$url, self$headers, ...)
            res$success()
        }

    ) # end public list
)
