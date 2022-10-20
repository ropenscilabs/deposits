
create_httr2_helper <- function (url, headers, method) {

    req <- httr2::request (url)
    req <- httr2::req_headers (req, "Authorization" = headers)
    req <- httr2::req_method (req, method)

    return (req)
}


#' @description Standardise timestamps put on return objects by APIs to ensure
#' consistent object hashes for `httptest`. Only activated in test environments
#' in which `DEPOSITS_TEST_ENV` envvar is set.
#' @noRd

standardise_hostdata_timestamps <- function (hostdata, service) {

    if (Sys.getenv ("DEPOSITS_TEST_ENV") == "true") {

        if (service == "figshare") {
            hostdata$created_date <- hostdata$modified_date <-
                "2022-01-01T00:00:00Z"
        } else if (service == "zenodo") {
            hostdata$created <- hostdata$modified <-
                "2022-01-01T00:00:00.0+00:00"
            hostdata$publication_date <- "2022-01-01"
        }
    }

    return (hostdata)
}
