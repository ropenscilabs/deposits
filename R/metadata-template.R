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

    invisible (!methods::is (res, "error"))
}

#' Read a metadata `JSON` file and convert to a metadata object.
#'
#' Metadata templates can be generated with \link{deposits_metadata_template},
#' and values manually entered. This functions loads the completed template and
#' converts it to a metadata object.
#'
#' @param filename Name of completed metadata file generated from
#' \link{deposits_metadata_template}.
#' @param id A unique identifier for the deposit, as a single character string.
#'
#' @examples
#' filename <- tempfile (fileext = ".json")
#' deposits_metadata_template (filename)
#' m <- readLines (filename)
#' m [grep ("Title", m)] <- "  \"Title\": \"New Title\""
#' m [grep ("Type", m)] <- "  \"Type\": \"Software\""
#' m [grep ("Description", m)] <-
#'     "  \"Description\": \"Description of software\""
#' # Values can be entered in JSON format:
#' m [grep ("TableOfContents", m)] <-
#'     "  \"TableOfContents\": {\"one\": \"First\", \"two\": \"Second\"}"
#' dc <- deposits_meta_from_file (filename, id = "my-id")
#' @family meta
#' @export
deposits_meta_from_file <- function (filename = NULL, id = "my-id") {

    checkmate::assert_character (filename, len = 1L)
    checkmate::assert_file_exists (filename)
    checkmate::assert_character (id, len = 1L)

    meta <- readLines (filename)
    check <- jsonlite::validate (meta)
    if (!check) {
        stop ("json is not valid.")
    }

    meta <- jsonlite::read_json (filename)
    meta <- meta [which (nchar (meta) > 0L)]

    not_dcmi <- c ("Tags", "Keywords", "^\\_")
    ptn <- paste0 (not_dcmi, collapse = "|")
    meta <- meta [which (!grepl (ptn, names (meta)))]

    return (meta)
}
