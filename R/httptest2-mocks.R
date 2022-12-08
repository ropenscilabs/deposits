# This file contains functions used to standardise inputs and outputs from API
# calls used by httptest2 in the test suite. Test environments are all flagged
# by setting the environment variable, `DEPOSITS_TEST_ENV` to "true". These
# functions have no effect in any other operating environment, including any
# normal usage of the package.
#
# Within `httptest2`, they mostly standardise a variety of date and time stamps
# placed upon request and return objects. This standardisation is necessary
# because `httptest2` constructs path and object names with hashes of the
# objects used in requests. These objects thus need to be entirely standardised
# to ensure reproducible hashes.
#
# The functions are all prefixed with `httptest2_` to explicitly indicate what
# they do, and this file is the only place in which the TEST_ENV environment
# variable is checked.



#' @description Standardise timestamps put on return objects by APIs to ensure
#' consistent object hashes for `httptest`. Only activated in test environments
#' in which `DEPOSITS_TEST_ENV` envvar is set.
#'
#' This is called in the main `deposit_retrieve()` method.
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

#' @description Standardise timestamp of metadata "created" field.
#'
#' This is called in the `construct_md_list_zenodo/figshare` functions.
#' @noRd

httptest2_dcmi_created <- function (metadata) {

    if (Sys.getenv ("DEPOSITS_TEST_ENV") == "true" &&
        "created" %in% names (metadata)) {
        metadata [["created"]] <- "2022-01-01T00:00:00.0+00:00"
    }

    return (metadata)
}

#' @description Standardise time and date stamps
#'
#' This is called only in the private `upload_dcmi_xml()` method.
#' @noRd

httpstest2_xml_timestamps <- function (xml) {

    if (Sys.getenv ("DEPOSITS_TEST_ENV") == "true") {

        # gsub timestamps:
        ptn <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}"
        xml <- gsub (ptn, "2022-01-01T00:00:00", xml)

        # datestamps:
        ptn <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}"
        xml <- gsub (ptn, "2022-01-01", xml)

        # and integer dataset id values:
        xml <- gsub ("dataset\\/\\_\\/[0-9]*<", "dataset/_/identifier<", xml)
    }

    return (xml)
}
