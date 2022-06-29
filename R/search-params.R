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
    search_params <- do.call (paste0 ("search_params_", service), list ())
    search_params <- data.frame (search_params)
    names (search_params) <- c ("param", "type")

    # check names of all `...` params are in official lists:
    index <- which (!names (arglist) %in% search_params$param)
    if (length (index) > 0L) {
        stop (
            "The parameters [",
            paste0 (names (arglist) [index], collapse = ", "),
            "] are not ",
            service,
            " search parameters; see ?depositsClient for full list."
        )
    }

    # check types of all `...` params match official lists:
    for (i in seq_along (arglist)) {
        expected_type <- search_params$type [search_params$param == names (arglist) [i]]
        assert_fn <- ifelse (
            expected_type == "string",
            checkmate::assert_character,
            checkmate::assert_int
        )
        do.call (assert_fn, list (arglist [[i]]))
    }

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

search_params_zenodo <- function () {
    rbind (
        c ("status", "string"),
        c ("sort", "string"),
        c ("all_versions", "string"),
        c ("communities", "string"),
        c ("type", "string"),
        c ("subtype", "string"),
        c ("bounds", "string"),
        c ("custom", "string")
    )
}

search_params_figshare <- function () {
    rbind (
        c ("resource_doi", "string"),
        c ("item_type", "integer"),
        c ("doi", "string"),
        c ("handle", "string"),
        c ("project_id", "integer"),
        c ("order", "string"),
        c ("order_direction", "string"),
        c ("institution", "integer"),
        c ("group", "integer"),
        c ("published_since", "string"),
        c ("modified_since", "string")
    )
}
