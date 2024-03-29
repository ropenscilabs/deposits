#' Retrieve a token for a specified deposits service.
#'
#' Tokens should be stored as local environment variables, optionally defined in
#' a `~/.Renviron` file, and should contain the name of the desired deposits
#' service.
#'
#' @param service Name of desired service; must be a value in the "name" column
#' of \link{deposits_services}.
#' @param sandbox If `TRUE`, retrieve token for sandbox, rather than actual API.
#' @return API token for nominated service.
#'
#' @examples
#' \dontrun{
#' token <- get_deposits_token (service = "figshare")
#' }
#' @family auth
#' @export
get_deposits_token <- function (service = NULL, sandbox = FALSE) {

    checkmate::assert_character (service, len = 1L)

    service <- gsub ("\\-", ".*", service)
    e <- Sys.getenv ()
    e <- e [grep (service, names (e), ignore.case = TRUE)]
    if (length (e) != 1L) {
        if (grepl ("^zenodo$", service, ignore.case = TRUE)) {
            if (sandbox && any (grepl ("sandbox", names (e), ignore.case = TRUE))) {
                e <- e [grep ("sandbox", names (e), ignore.case = TRUE)]
            } else {
                e <- e [which (!grepl ("sandbox", names (e), ignore.case = TRUE))]
            }
        }
    }

    token <- unique (e)

    if (length (token) == 0L) {
        stop ("No token found for [", service, "] service.",
            call. = FALSE
        )
    }
    if (length (token) != 1L) {
        stop ("No unambiguous token found for [", service, "] service.",
            call. = FALSE
        )
    }

    return (token)
}
