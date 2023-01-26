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

is_file_binary <- function (file, bin.ints = c (1:8, 14:25), nbytes = 1000, nbin = 2) {
    x <- as.integer (readBin (file, "raw", nbytes))
    n <- sum (x %in% bin.ints)

    return (n > nbin)
}

is_deposits_test_env <- function () {
    Sys.getenv ("DEPOSITS_TEST_ENV") == "true"
}
