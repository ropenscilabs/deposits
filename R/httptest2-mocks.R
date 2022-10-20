
#' @description Standardise timestamps put on return objects by APIs to ensure
#' consistent object hashes for `httptest`. Only activated in test environments
#' in which `DEPOSITS_TEST_ENV` envvar is set.
#' @noRd

httptest2_hostdata_timestamps <- function (hostdata, service) {

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

#' @description Standardise DCMI "updated" timestamps put on by `atom4R` to
#' ensure consistent object hashes for `httptest`. Only activated in test
#' environments in which `DEPOSITS_TEST_ENV` envvar is set.
#' @noRd

httptest2_dcmi_timestamps <- function (dcmi) {

    if (Sys.getenv ("DEPOSITS_TEST_ENV") == "true") {
        # atom4R inserts actual "updated" time in the metadata
        utime <- as.POSIXct ("2022-01-01 00:00:01", tz = "CEST")
        dcmi$setUpdated (utime)
    }

    return (dcmi)
}
