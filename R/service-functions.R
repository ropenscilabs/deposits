# Single file for service-specific functions (not methods).

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
            # c ("dryad", "https://datadryad.org/api/v2/docs/"),
            c (
                "zenodo",
                "https://developers.zenodo.org/",
                "https://zenodo.org/api/"
            ),
            c (
                "zenodo-sandbox",
                "https://developers.zenodo.org/",
                "https://sandbox.zenodo.org/api/"
            ),
            c (
                "figshare",
                "https://docs.figshare.com/",
                "https://api.figshare.com/v2/"
            )
        )
    )
    names (out) <- c ("name", "docs", "api_base_url")

    return (out)
}

#' Define methods which are NOT applicable for particular services.
#'
#' This method is only called for its immediate side-effect of erroring when
#' methodss are called which are not applicable for the nominated service.
#' @noRd
stop_if_method_not_defined <- function (service, method_name) {

    if (method_name == "deposit_prereserve_doi" && service != "figshare") {
        stop ("This method only has effect for Figshare", call. = FALSE)
    }

    invisible (NULL)
}

add_service_sandbox <- function (service, sandbox) {

    if (service == "zenodo" && sandbox) {
        service <- "zenodo-sandbox"
    }

    return (service)
}

rm_service_sandbox <- function (service) {

    if (service == "zenodo-sandbox") {
        service <- "zenodo"
    }

    return (service)
}

service_download_url <- function (service, files, filename) {

    if (service == "figshare") {
        download_url <- files$download_url [files$name == filename]
    } else if (service == "zenodo") {
        download_url <- files$links$download [files$filename == filename]
    }

    return (download_url)
}

service_is_deposit_embargboed <- function (hostdata, service) {

    if (service == "zenodo") {
        is_embargoed <- identical (hostdata$metadata$access_right, "embargoed")
    } else if (service == "figshare") {
        is_embargoed <- hostdata$is_embargoed
    }

    return (is_embargoed)
}

service_md5_field <- function (service) {

    if (service == "zenodo") {
        ret <- "checksum"
    } else if (service == "figshare") {
        ret <- "supplied_md5"
    }

    return (ret)
}

#' @description Get the name of the "files" part of hostdata which contains the
#' actual names of the files.
#' @noRd
service_filename_field <- function (service) {

    if (service == "figshare") {
        ret <- "name"
    } else if (service == "zenodo") {
        ret <- "filename"
    }

    return (ret)
}