
#' Get URL of deposit service
#'
#' @param cli Deposits client with 'url_base' and 'service' fields.
#' @param deposit_id Optional identifier of specified deposit. If specified,
#' return URL to that deposit, otherwise return generic URL to particular
#' service, used to create or list deposits.
#' @return URL
#' @noRd

get_service_url <- function (cli, deposit_id = NULL) {

    url <- paste0 (
        cli$url_base,
        ifelse (cli$service == "figshare",
            "account/articles",
            "deposit/depositions"
        )
    )

    if (!is.null (deposit_id)) {
        checkmate::assert_int (deposit_id)
        url <- paste0 (url, "/", deposit_id)
    }

    return (url)
}
