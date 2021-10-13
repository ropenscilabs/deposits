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
#'   url = "https://zenodo.org/api",
#'   headers = list(Authorization = paste0("Bearer ", token))
#' )
#'
#' # methods
#' ## ping - hopefully you get TRUE
#' cli$ping()
#'
#' }
depositsClient <- R6::R6Class(
  "depositsClient",
  portable = TRUE,
  cloneable = FALSE,

  private = list(
  ), # end private list

  public = list(
    #' @field url (character) list of fragments
    url = NULL,
    #' @field headers list of named headers
    headers = NULL,
    #' @field schema holds schema
    schema = NULL,
    #' @field result holds result from http request
    result = NULL,

    #' @description Create a new `depositsClient` object
    #' @param url (character) URL for the deposits schema
    #' @param headers Any acceptable headers, a named list. See examples
    #' @return A new `depositsClient` object
    initialize = function(url, headers) {
      if (!missing(url)) self$url <- url
      if (!missing(headers)) self$headers <- headers
    },

    #' @description print method for the `depositsClient` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat('<deposits client>', sep = "\n")
      cat(paste0('  url: ', self$url), sep = "\n")
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
