
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
#' @family meta
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

#' Read a metadata `yaml` file and convert to \pkg{atom4R} DCMI object.
#'
#' Metadata templates can be generated with \link{deposits_metadata_template},
#' and values manually entered. This functions loads the completed template and
#' converts it to a `DCEntry` object from the \pkg{atom4R} package.
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
#'    "  \"Description\": \"Description of software\""
#' # Values can be entered in JSON format:
#' m [grep ("TableOfContents", m)] <-
#'     "  \"TableOfContents\": {\"one\": \"First\", \"two\": \"Second\"}"
#' dc <- deposits_meta_to_dcmi (filename, id = "my-id")
#' @family meta
#' @export
deposits_meta_to_dcmi <- function (filename = NULL, id = "my-id") {

    if (is.null (filename)) {
        stop ("filename must be specified.")
    }
    if (!file.exists (filename)) {
        stop ("filename [", filename, "] does not exist.")
    }


    meta <- readLines (filename)
    check <- jsonlite::validate (meta)
    if (!check) {
        stop ("json is not valid.")
    }

    if (!"atom4R" %in% loadedNamespaces ()) {
        # https://github.com/eblondel/atom4R/issues/9
        # https://github.com/eblondel/atom4R/pull/10
        library ("atom4R")
    }

    meta <- jsonlite::read_json (filename)
    meta <- meta [which (nchar (meta) > 0L)]

    dcmi <- atom4R::DCEntry$new ()

    for (n in names (meta)) {
        dc_fn <- paste0 ("addDC", n)
        meta_n <- meta [[n]]
        for (m in meta_n) {
            do.call (dcmi [[dc_fn]], list (m))
        }
    }

    check <- dcmi$validate ()

    return (dcmi)
}

#' Load metadata term translation table from local inst/extdata
#' @noRd
load_meta_terms <- function () {

    terms <- system.file (file.path ("extdata", "DCTerms.csv"),
                          package = "deposits")
    terms <- utils::read.csv (terms)
    for (i in seq (ncol (terms))) {
        terms [, i] <- gsub ("^\\s+|\\s+$", "", terms [, i])
    }
    index <- which (nchar (terms$Zenodo) > 0L | nchar (terms$Figshare) > 0L)
    return (terms [index, ])
}
