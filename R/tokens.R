
#' Retrieve a token for a specified deposits service.
#'
#' Tokens should be stored as local environment variables, optionally defined in
#' a `~/.Renviron` file, and should contain the name of the desired deposits
#' service.
#'
#' @param service Name of desired service; must be a value in the "name" column
#' of \link{deposits_services).
#' @export
get_deposits_token <- function (service) {

    e <- Sys.getenv ()
    token <- unique (e [grep (service, names (e), ignore.case = TRUE)])

    if (length (token) != 1L) {
        stop ("No unambiguous token found for [", service, "] service.")
    }

    return (token)
}