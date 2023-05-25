#' Get URL of deposit service
#'
#' @param cli Deposits client with 'url_base' and 'service' fields.
#' @param deposit_id Optional identifier of specified deposit. If specified,
#' return URL to that deposit, otherwise return generic URL to particular
#' service, used to create or list deposits.
#' @return URL
#' @noRd

get_service_url <- function (cli, deposit_id = NULL) {

    subdom <- ""
    if (cli$service == "figshare") {

        subdom <- "articles" # public articles
        if (!is.null (deposit_id) && !is.null (cli$deposits$id)) {
            if (deposit_id %in% cli$deposits$id) {
                subdom <- "account/articles" # private articles
            }
        }

    } else if (cli$service == "zenodo") {

        subdom <- "deposit/depositions"
    }

    url <- paste0 (cli$url_base, subdom)

    if (!is.null (deposit_id)) {
        checkmate::assert_int (deposit_id)
        url <- paste0 (url, "/", deposit_id)
    }

    return (url)
}
