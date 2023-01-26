# --------------------------------------------------
# Private methods dedicated to processing {frictionless} "datapackage.json"
# files.
# --------------------------------------------------


#' @description Auto-generate default 'frictionless' JSON file.
#'
#' This generates an additional (default) "datapackage.json" file in the
#' directory of the file specified by 'path'.
#'
#' @param path Full path to data object to be uploaded.
#' @return The frictionless data package (as a named list)
#' @noRd

depositsClient$set ("private", "generate_frictionless", function (path) {

    requireNamespace ("frictionless")

    resource_name <- fs::path_ext_remove (fs::path_file (path))
    p <- frictionless::create_package ()
    op <- options (readr.show_progress = FALSE, readr.show_col_types = FALSE)
    p <- frictionless::add_resource (
        p,
        resource_name = resource_name,
        data = path
    )
    frictionless::write_package (p, fs::path_dir (path))
    options (op)

    return (p)
})

#' @description Add metadata to /pkg{frictionless} 'datapackage.json' file.
#'
#' @param path Path to directory containing 'datapackage.json' file.
#' @return A logical value of `TRUE` if 'datapackage.json' is updated; otherwise
#' `FALSE`.
#' @noRd

depositsClient$set ("private", "add_meta_to_dp_json", function (path) {

    ret <- FALSE

    path_json <- fs::path (path, private$frictionless_json_name)
    op <- options (readr.show_progress = FALSE, readr.show_col_types = FALSE)
    suppressMessages (
        p <- frictionless::read_package (path_json)
    )
    options (op)

    if (!"metadata" %in% names (p)) {
        p <- append (p, c (metadata = list (self$metadata)), after = 1)
        op <- options (
            readr.show_progress = FALSE,
            readr.show_col_types = FALSE
        )
        frictionless::write_package (p, path)
        options (op)
        ret <- TRUE
    }

    return (ret)
})

#' @description Update remote and local frictionless data files.
#'
#' Currently only called on 'upload_local_file'. This compares modification
#' times between local and remote versions (where they exist), selects the
#' version with the latest mtime, and if updated at all, uploads it once again
#' to the remote service, as well as updating any local copy that exists.
#'
#' @param path Path to local file that was uploaded.
#' @noRd

depositsClient$set ("private", "update_frictionless", function (path) {

    deposit_id <- self$id

    # -------- Ensure local "datapackage.json" is up to date
    files <- self$hostdata$files
    file_names <- files [[private$get_file_name_field ()]]
    mtime_remote <- mtime_local <- strftime ("1900-01-01 00:00:00")
    dp_remote <- ""

    if (private$frictionless_json_name %in% file_names &&
        !is_deposits_test_env ()) {
        dp_remote <- self$deposit_download_file (
            deposit_id,
            filename = private$frictionless_json_name,
            path = fs::path_temp ()
        )
        mtime_remote <- fs::file_info (dp_remote)$modification_time
    }

    path_dir <- fs::path_dir (path)
    dp_local <- fs::path (path_dir, private$frictionless_json_name)
    has_dpj <- fs::file_exists (dp_local)
    if (has_dpj) {
        mtime_local <- fs::file_info (dp_local)$modification_time
    }

    update_remote <- binary_without_frictionless <- FALSE
    if (mtime_remote > mtime_local) {
        dp <- dp_remote
    } else {
        update_remote <- TRUE
        dp <- dp_local
    }

    if (fs::file_exists (dp)) {
        suppressMessages (
            dpj <- frictionless::read_package (dp)
        )
        if (!"metadata" %in% names (dpj)) {
            update_remote <- private$add_meta_to_dp_json (path_dir)
            # That method always returns 'TRUE'
        }
    } else if (is_file_binary (path)) {
        warning (
            "There is no frictionless 'datapackage.json' file either ",
            "locally or on the '",
            self$service,
            "' deposit, nor can one be generated from binary data.\n",
            "It is recommended to first generate a local ",
            "'datapackage.json' file for you data. See documentation at ",
            "https://docs.ropensci.org/frictionless"
        )
        update_remote <- FALSE

        binary_without_frictionless <- TRUE
    } else {
        p <- private$generate_frictionless (path) # return frictionless data
        message (
            "frictionless metadata file has been generated as '",
            path,
            "'"
        )
        chk <- private$add_meta_to_dp_json (path_dir) # always true
        # 'p' is then not up-to-date, but not used from here so okay for now.
        update_remote <- TRUE
    }

    if (binary_without_frictionless) {
        invisible (return (self))
    }

    if (!identical (dp, dp_local)) {
        fs::file_copy (dp, dp_local, overwrite = TRUE)
        message (
            "frictionless metadata file [",
            path,
            "] has been updated."
        )
    }

    # -------- Update local version with new resources
    dpj <- frictionless::read_package (dp_local)
    resource_names <-
        vapply (dpj$resources, function (i) i$name, character (1L))
    new_resource_name <- fs::path_ext_remove (fs::path_file (path))
    dp_file_names <- vapply (dpj$resources, function (i) i$path, character (1L))

    if (!new_resource_name %in% resource_names) {

        p <- frictionless::create_package ()
        op <- options (
            readr.show_progress = FALSE,
            readr.show_col_types = FALSE
        )
        suppressMessages (
            p <- frictionless::add_resource (
                p,
                resource_name = new_resource_name,
                data = path
            )
        )
        options (op)

        dpj$resources <- c (dpj$resources, p$resources)

        # Update local version:
        frictionless::write_package (dpj, fs::path_dir (dp_local))

        update_remote <- TRUE

    } else if (!all (dp_file_names %in% file_names)) {

        # "datapackage.json" lists resources not yet uploaded.
        dp_not_uploaded <- dp_file_names [which (!dp_file_names %in% file_names)]
        message (
            "Your 'datapackage.json' includes the following resources ",
            "which have not yet been uploaded: [",
            paste0 (dp_not_uploaded, collapse = ", "),
            "]"
        )
    }

    # httptest2 does not produce mocked download files; only the actual
    # request result. So these files can not be uploaded here.
    if (update_remote && !is_deposits_test_env ()) {
        self <- private$upload_local_file (dp)
    }

    if (identical (dp, dp_remote)) {
        fs::file_delete (dp_remote)
    }

    invisible (self)
})
