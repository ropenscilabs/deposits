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
#' @param path Full path to local "datapackage.json" file.
#' @param overwrite Inherited from `upload_local_file`
#' @noRd

depositsClient$set (
    "private", "update_frictionless",
    function (path, overwrite = FALSE, quiet = FALSE) {

        deposit_id <- self$id

        # -------- Ensure local "datapackage.json" is up to date
        files <- self$hostdata$files
        file_names <- files [[service_filename_field (self$service)]]

        local_dp_check <- ensure_latest_local_dpjson (
            self,
            file_names,
            path,
            private$frictionless_json_name
        )

        update_remote <- local_dp_check$update_remote
        path_dir <- fs::path_dir (path)

        if (fs::file_exists (local_dp_check$dp)) {
            suppressMessages (
                dpj <- frictionless::read_package (local_dp_check$dp)
            )
            if (!"metadata" %in% names (dpj)) {
                update_remote <- private$add_meta_to_dp_json (path_dir)
                # That method always returns 'TRUE'
            }
        } else if (self$frictionless) {
            if (is_file_binary (path)) {
                warning (
                    "There is no frictionless 'datapackage.json' file either ",
                    "locally or on the '",
                    self$service,
                    "' deposit, nor can one be generated from binary data.\n",
                    "It is recommended to first generate a local ",
                    "'datapackage.json' file for you data. See documentation ",
                    "at https://docs.ropensci.org/frictionless"
                )
                update_remote <- FALSE

                self$frictionless <- FALSE
            } else {
                # return frictionless data:
                p <- private$generate_frictionless (path)
                message (
                    "frictionless metadata file has been generated as '",
                    path,
                    "'"
                )
                chk <- private$add_meta_to_dp_json (path_dir) # always true
                # 'p' is then not up-to-date, but not used from here so okay for
                # now.
                update_remote <- TRUE
            }
        }

        if (!self$frictionless) {
            invisible (return (self))
        }

        if (!identical (local_dp_check$dp, local_dp_check$dp_local)) {
            fs::file_copy (
                local_dp_check$dp,
                local_dp_check$dp_local,
                overwrite = TRUE
            )
            message (
                "frictionless metadata file [",
                path,
                "] has been updated."
            )
        }

        # -------- Update local version with new resources
        dpj <- frictionless::read_package (local_dp_check$dp_local)
        resource_names <-
            vapply (dpj$resources, function (i) i$name, character (1L))
        new_resource_name <- fs::path_ext_remove (fs::path_file (path))
        dp_file_names <- vapply (
            dpj$resources,
            function (i) i$path,
            character (1L)
        )

        if (!new_resource_name %in% resource_names) {

            p <- create_new_frictionless (new_resource_name, path)
            dpj$resources <- c (dpj$resources, p$resources)

            # Update local version:
            path_loc <- fs::path_dir (local_dp_check$dp_local)
            frictionless::write_package (dpj, path_loc)

            update_remote <- TRUE

        } else if (!all (dp_file_names %in% file_names) && !quiet) {

            # "datapackage.json" lists resources not yet uploaded.
            dp_not_uploaded <-
                dp_file_names [which (!dp_file_names %in% file_names)]
            message (
                "Your 'datapackage.json' includes the following resources ",
                "which have not yet been uploaded: [",
                paste0 (dp_not_uploaded, collapse = ", "),
                "]"
            )
        }

        # Upload frictionless if it has changed.
        #
        # This re-upload of the same resource can not be mocked with httptest2,
        # because it can only expect same mocked upload, so all upload of
        # "datapackage.json" is switched off in test environments.
        if (update_remote && !is.null (self$id) &&
            !is.null (self$url_service) && !is_deposits_test_env ()) {

            self <- private$upload_local_file (
                local_dp_check$dp,
                overwrite = overwrite,
                compress = "no"
            )
        }

        if (identical (local_dp_check$dp, local_dp_check$dp_remote)) {
            fs::file_delete (local_dp_check$dp_remote)
        }

        invisible (self)
    }
)

#' Ensure local 'datapacakge.json' is up to date.
#'
#' @param cli Client, which is 'self' in the private method which calls this.
#' Used only to call the 'deposit_download_file' method to download the
#' 'datapackage.json' file.
#' @noRd
ensure_latest_local_dpjson <- function (cli, file_names,
                                        path, frictionless_json_name) {

    mtime_remote <- mtime_local <- strftime ("1900-01-01 00:00:00")
    dp_remote <- ""

    path_dir <- fs::path_dir (path)
    dp_local <- fs::path (path_dir, frictionless_json_name)
    has_dpj <- fs::file_exists (dp_local)
    if (has_dpj) {
        mtime_local <- fs::file_info (dp_local)$modification_time
    }

    if (frictionless_json_name %in% file_names && !is_deposits_test_env ()) {

        name_field <- service_filename_field (cli$service)
        same_md5 <- md5sums_are_same (dp_local, cli$hostdata, name_field, cli$service, quiet = TRUE)

        if (same_md5) {

            mtime_remote <- mtime_local

        } else if (cli$service != "figshare") {

            # figshare does not allow private downloads, so can only check this
            # on other services:
            dp_remote <- cli$deposit_download_file (
                cli$deposit_id,
                filename = frictionless_json_name,
                path = fs::path_temp ()
            )
            mtime_remote <- fs::file_info (dp_remote)$modification_time
        }
    }

    update_remote <- FALSE
    if (mtime_remote > mtime_local) {
        dp <- dp_remote
    } else {
        update_remote <- mtime_local > mtime_remote
        dp <- dp_local
    }

    return (list (
        dp = dp,
        dp_local = dp_local,
        dp_remote = dp_remote,
        update_remote = update_remote
    ))
}

create_new_frictionless <- function (new_resource_name, path) {

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

    return (p)
}

host_has_frictionless <- function (service, hostdata,
                                   frictionless_json_name, name_field) {

    no <- length (hostdata$files) == 0L
    if (!no) {
        no <- nrow (hostdata$files) == 1L
    }
    if (!no) {
        no <- !frictionless_json_name %in% hostdata$files [[name_field]]
    }

    return (!no)
}
