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

        check_param_values_figshare (arglist)

        if (!is.null (search_string)) {
            arglist <- c (arglist, search_for = search_string)
        }
        arglist <- c (
            arglist,
            page_size = page_size,
            page = page_number
        )
    } else if (service == "zenodo") {

        check_param_values_zenodo (arglist)

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

check_param_values_zenodo <- function (arglist) {

    if ("all_versions" %in% names (arglist)) {
        if (!arglist$all_versions %in% c ("true", "false")) {
            stop (
                "The 'add_versions' parameter must be either 'false' or 'true'",
                call. = FALSE
            )
        }
    }

    if ("bounds" %in% names (arglist)) {
        if (!(grepl ("^bounds=", arglist$bounds) &&
            length (strsplit (arglist$bounds, ",") [[1]] == 4L))) {
            stop (
                "The 'bounds' parameter must be in format 'bounds=x1,y1,x2,y2'",
                call. = FALSE
            )
        }
    }
}

check_param_values_figshare <- function (arglist) {

    if ("item_type" %in% names (arglist)) {
        if (!arglist$item_type %in% 1:29) {
            stop (
                "The 'item_type' parameter must be an integer between 1 and 29",
                call. = FALSE
            )
        }
    }

    if ("order" %in% names (arglist)) {
        if (!arglist$order %in%
            c ("published_date", "modified_date", "views", "shares", "downloads", "cites")) {
            stop (
                "The 'order' parameter must be in the specified vocabulary; ",
                "see ?depositsClient for link to accepted values.",
                call. = FALSE
            )
        }
    }

    if ("order_direction" %in% names (arglist)) {
        if (!arglist$order_direction %in% c ("asc", "desc")) {
            stop (
                "The 'order_direction' parameter must be either 'asc' or 'desc'",
                call. = FALSE
            )
        }
    }

    if ("published_since" %in% names (arglist)) {
        if (!grepl ("^[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}$", arglist$published_since)) {
            stop (
                "The 'published_since' parameter must be in format YYYY-MM-DD",
                call. = FALSE
            )
        }
    }

    if ("modified_since" %in% names (arglist)) {
        if (!grepl ("^[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}$", arglist$modified_since)) {
            stop (
                "The 'modified_since' parameter must be in format YYYY-MM-DD",
                call. = FALSE
            )
        }
    }

}
