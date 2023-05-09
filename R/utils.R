#' Is a file binary?
#'
#' Taken from the {knitrdata} package:
#' https://github.com/dmkaplan2000/knitrdata/blob/master/R/utils.R
#' See the above link for full documentation. The following quote is relevant
#' "This works well for standard ASCII text, but it may be less effective for
#' complex UTF8 text (e.g., Chinese)."
#' Copyright: David M. Kaplan \email{dmkaplan2000@@gmail.com}
#'
#' The following parameter documentation entries are taken directly from
#' {knitrdata}:
#' @param file The path to the file to be examined
#' @param bin.ints List of integers with the ASCII values of control characters
#'      that are to be considered when when looking for signs a file is binary.
#'      Default includes most ASCII control characters except things like NULL,
#'      LF, CR and HT that might actually appear in an ASCII file.
#' @param nbytes Number of bytes to read in from the beginning of the file.
#' @param nbin An integer indicating the threshold on the number of control
#'      characters above which a file is considered binary. Defaults to 2.
#' @noRd

is_file_binary <- function (file, bin_ints = c (1:8, 14:25),
                            nbytes = 1000, nbin = 2) {

    x <- as.integer (readBin (file, "raw", nbytes))
    n <- sum (x %in% bin_ints)

    return (n > nbin)
}

is_deposits_test_env <- function () {
    Sys.getenv ("DEPOSITS_TEST_ENV") == "true"
}

deposit_timestamp <- function (datetime) {

    checkmate::assert_class (datetime, "POSIXct")
    datetime <- format.POSIXct (datetime, "%Y-%m-%dT%H:%M:%S%z", usetz = FALSE)
    # change terminal "+0000" to "+00:00":
    ptn <- regmatches (datetime, regexpr ("[0-9]{2}$", datetime))
    datetime <- gsub (paste0 (ptn, "$"), paste0 (":", ptn), datetime)

    return (datetime)
}

condense_linebreaks <- function (txt) {
    for (n in 4:1) {
        bs <- rep ("\\", n)
        ptn <- paste0 (paste0 (bs, collapse = ""), "n")
        txt <- gsub (ptn, "\n", txt)
    }
    return (txt)
}

#' Extract 'service' parameter from metadata if not otherwise specified.
#' @noRd
service_from_metadata <- function (metadata, service = NULL) {

    if (!"identifier" %in% names (metadata)) {
        return (NULL)
    }

    id <- metadata$identifier
    srv <- regmatches (
        id,
        regexpr ("(\\.|\\/)[a-zA-z]+\\.[0-9]+$", id)
    )
    srv <- gsub ("^\\.|\\/|\\.[0-9]+$", "", srv)

    if (!is.null (srv)) {
        service <- srv
    }

    return (service)
}

#' Compare local and remote md5sums to determine whether files differ
#' @noRd
md5sums_are_same <- function (path,
                              hostdata,
                              name_field,
                              service,
                              file_exists = FALSE,
                              quiet = FALSE) {

    md5_local <- unname (tools::md5sum (path))

    md5_remote <- NULL
    path_file <- fs::path_file (path)
    host_files <- hostdata$files
    i <- match (path_file, host_files [[name_field]])
    if (length (i) > 0L) {
        md5_field <- service_md5_field (service) # in services.R
        md5_remote <- host_files [[md5_field]] [i]
    }

    res <- identical (md5_local, md5_remote)

    if (!quiet) {
        if (res) {
            message (
                "Local file at [",
                path,
                "] is identical on host and will not be uploaded."
            )
        } else if (file_exists) {
            message (
                "Local file at [",
                path,
                "] has changed and will now be uploaded."
            )
        }
    }

    return (res)
}
