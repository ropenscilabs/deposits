#' Write an empty metadata template to local file
#'
#' The fields are those defined by the Dublin Core Metadata Initiative (DCMI),
#' defined at
#' \url{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}. The
#' template produced by this function is in `json` format which can be manually
#' edited to provide metadata for a deposit.
#'
#' @param filename Name or full path to local file where template is to be
#' written. This file will be created. If a file of that name already exists, it
#' must first be deleted. The file extension '.json' will be automatically
#' appended.
#'
#' @return (Invisibly) `TRUE` if local file successfully created; otherwise
#' `FALSE`.
#'
#' @examples
#' filename <- tempfile (fileext = ".json")
#' deposits_metadata_template (filename)
#' # then edit that file to complete metadata
#' @family meta
#' @export
deposits_metadata_template <- function (filename = NULL) {

    checkmate::assert_character (filename, len = 1L)
    filepath <- fs::path_dir (filename)
    checkmate::assert_directory_exists (filepath)
    fs::path_ext (filename) <- ".json"
    if (fs::file_exists (filename)) {
        stop (
            "filename [", filename, "] already exists; please delete before ",
            "calling this function."
        )
    }

    dc <- system.file (fs::path ("extdata", "dc", "schema-template.json"),
        package = "deposits"
    )
    template <- jsonlite::read_json (dc, simplifyVector = FALSE)

    res <- tryCatch (
        suppressWarnings (
            jsonlite::write_json (template,
                filename,
                auto_unbox = TRUE,
                pretty = TRUE
            )
        ),
        error = function (e) e
    )

    if (methods::is (res, "simpleError")) {
        stop ("Error with metadata: ", res$message)
    }

    message (
        "Edit the file [",
        filename,
        "] and remove everything except the metadata fields you require.\n",
        "The filename may be then passed as the 'metadata' argument to a ",
        "'deposits' client."
    )

    invisible (!methods::is (res, "error"))
}
