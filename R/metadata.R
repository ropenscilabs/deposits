
#' Write an empty metadata template to local file
#'
#' The fields are those defined by the Dublin Core Metadata Initiative (DCMI),
#' defined at
#' \url{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/},
#' extracted here from the \pkg{atom4R} package. The template produced by this
#' function is in `json` format which can be manually edited to provide metadata
#' for a deposit.
#'
#' @param filename Name or full path to local file where template is to be
#' written. This file will be created. If a file of that name already exists, it
#' must first be deleted.
#'
#' @return (Invisibly) `TRUE` if local file successfully created; otherwise
#' `FALSE`.
#'
#' @examples
#' filename <- tempfile (fileext = ".json")
#' deposits_metadata_template (filename)
#' # then edit that file to complete metadata
#' @family misc
#' @export
deposits_metadata_template <- function (filename = NULL) {

    if (file.exists (filename)) {
        stop ("filename [", filename, "] already exists; please delete before ",
              "calling this function.")
    }
        
    # Get DCMI metadata fields from atom4R function names:
    fields <- ls (envir = asNamespace ("atom4R"), pattern = "^DC")
    not_meta <- c ("DCElement", "DCEntry", "DCMIVocabulary")
    fields <- fields [which (!fields %in% not_meta)]

    fields <- gsub ("^DC", "", fields)
    substr (fields, 1, 1) <- tolower (substr (fields, 1, 1))
    flist <- as.list (rep ("", length (fields)))
    names (flist) <- fields

    res <- tryCatch (
        suppressWarnings (
            jsonlite::write_json (flist,
                                  filename,
                                  auto_unbox = TRUE,
                                  pretty = TRUE)
        ), error = function (e) e)

    invisible (!methods::is (res, "error"))
}
