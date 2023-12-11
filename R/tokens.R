#' Retrieve a token for a specified deposits service.
#'
#' Tokens should be stored as local environment variables, optionally defined in
#' a `~/.Renviron` file. Tokens can be defined by using the pattern 
#' `SERVICE_TOKEN`, where `SERVICE` is the name of the service in capital letters
#' followed by `_SANDBOX` if the sandbox from e.g. zotero should be used. An example
#' would be `ZOTERO_SANDBOX_TOKEN` for the sandbox of zotero.
#' If these are not found, an heuristic algorythm is used to find the token name
#' which should contain the name of the desired deposits service.
#'
#' @param service Name of desired service; must be a value in the "name" column
#' of \link{deposits_services}.
#' @param sandbox If `TRUE`, retrieve token for sandbox, rather than actual API.
#' @return API token for nominated service.
#' 
#' @md
#' @examples
#' \dontrun{
#' token <- get_deposits_token (service = "figshare")
#' }
#' @family auth
#' @export
get_deposits_token <- function (service = NULL, sandbox = FALSE) {

    checkmate::assert_character (service, len = 1L)

    service <- gsub ("\\-", ".*", service)

    # Check for fixed environmental variables ZENODO_TOKEN and ZENODO_SANDBOX_TOKEN

    e <- ""
    if (grepl("^zenodo$", service, ignore.case = TRUE)) {
        if (sandbox) {
            e <- Sys.getenv("ZENODO_SANDBOX_TOKEN")
        } else {
            e <- Sys.getenv("ZENODO_TOKEN")
        }
    } else if (grepl("^figshare$", service, ignore.case = TRUE)) {
        e <- Sys.getenv("FIGSHARE_TOKEN")
    }

    if (e == "") {
        e <- Sys.getenv()
        e <- e[grep(service, names(e), ignore.case = TRUE)]
        if (length(e) != 1L) {
            if (grepl("^zenodo$", service, ignore.case = TRUE)) {
                if (sandbox && any(grepl("sandbox", names(e), ignore.case = TRUE))) {
                    e <- e[grep("sandbox", names(e), ignore.case = TRUE)]
                } else {
                    e <- e[which(!grepl("sandbox", names(e), ignore.case = TRUE))]
                }
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
