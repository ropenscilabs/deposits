#' Process parameters in `deposits_search()` function
#' @noRd
process_search_params <- function (service,
                                   search_string = NULL,
                                   page_size = 10L,
                                   page_number = 1L,
                                   ...) {

    if (!is.null (search_string)) {
        checkmate::assert_character (search_string, len = 1L)
    }
    checkmate::assert_int (page_size)
    checkmate::assert_int (page_number)

    arglist <- list (...)

    if (service == "figshare") {
        if (!is.null (search_string)) {
            arglist <- c (arglist, search_for = search_string)
        }
        arglist <- c (
            arglist,
            page_size = page_size,
            page = page_number
        )
    } else if (service == "zenodo") {
        if (!is.null (search_string)) {
            arglist <- c (arglist, q = search_string)
        }
        arglist <- c (
            arglist,
            size = page_size,
            page = page_number
        )
    } else {
        # stop
    }

    return (arglist)
}
