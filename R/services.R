
#' List all deposits services and associated URLs
#'
#' @return A `data.frame` with `name` and `url` values for each accessible
#' service.
#' @export
deposits_services <- function () {

    out <- data.frame (
        rbind (
            c ("zenodo", "https://zenodo.org/api")
            )
        )
    names (out) <- c ("name", "url")

    return (out)
}
