
#' List all deposits services and associated URLs
#'
#' @return A `data.frame` with `name` and `url` values for each accessible
#' service.
#'
#' @examples
#' s <- deposits_services ()
#' @family misc
#' @export
deposits_services <- function () {

    out <- data.frame (
        rbind (
            #c ("dryad", "https://datadryad.org/api/v2/docs/"),

            c ("zenodo",
               "https://developers.zenodo.org/",
               "https://zenodo.org/api/"),

            c ("zenodo-sandbox",
               "https://developers.zenodo.org/",
               "https://sandbox.zenodo.org/api/"),

            c ("figshare",
               "https://docs.figshare.com/",
               "https://api.figshare.com/v2/")
            )
        )
    names (out) <- c ("name", "docs", "api_base_url")

    return (out)
}
