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

        mockdate <- "2022-01-01T00:00:00+00:00"

        if (service == "figshare") {

            hostdata$created_date <- hostdata$modified_date <- mockdate

        } else if (service == "zenodo") {

            if ("modified" %in% names (hostdata)) {
                hostdata$modified <- mockdate
            }
            if ("publication_date" %in% names (hostdata)) {
                hostdata$publication_date <- "2022-01-01"
            }
            if ("publication_date" %in% names (hostdata$metadata)) {
                hostdata$metadata$publication_date <- "2022-01-01"
            }
        }
    }

    return (hostdata)
}

#' @description Standardise timestamp of metadata "created" field.
#'
#' @noRd

httptest2_created_timestamp <- function (metadata) {

    if (Sys.getenv ("DEPOSITS_TEST_ENV") == "true") {
        mockdate <- "2022-01-01T00:00:00+00:00"
        mockdate_short <- strftime (mockdate, "%Y-%m-%d")
        if ("created" %in% names (metadata)) {
            metadata [["created"]] <- mockdate
        }
        if ("created" %in% names (metadata$dcmi)) {
            metadata$dcmi [["created"]] <- mockdate
        }
        if ("created" %in% names (metadata$service)) {
            metadata$service [["created"]] <- mockdate
        }
        # publication_date inserted in zenodo embargo test:
        if ("publication_date" %in% names (metadata$service$metadata)) {
            metadata$service$metadata [["publication_date"]] <- mockdate_short
        }
    }

    return (metadata)
}
