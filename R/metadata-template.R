
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
#' @param metadata A named list of DCMI metadata, where names should match
#' \link{dcmi_terms}, and entries should generally be single character values.
#' Multiple entries are generally permitted, so for example multiple authors can
#' be specified with multiple list items named "Creator", each of which
#' specifies one author.
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
deposits_metadata_template <- function (filename = NULL, metadata = NULL) {

    checkmate::assert_character (filename, len = 1L)
    filepath <- dirname (normalizePath (filename, mustWork = FALSE))
    checkmate::assert_directory_exists (filepath)
    if (file.exists (filename)) {
        stop (
            "filename [", filename, "] already exists; please delete before ",
            "calling this function."
        )
    }

    meta_terms <- dcmi_terms ()
    template <- as.list (rep ("", length (meta_terms)))
    names (template) <- meta_terms

    # add non-DCMI comment, tags and keywords fields
    template$`_comment` <- paste0 (
        "Fields starting with underscores will be ",
        "ignored (and can safely be deleted)"
    )
    template$Tags <- list ("tag1", "tag2")
    template$Keywords <- list ("keyword1", "keyword2")
    template <- template [order (names (template))]
    # insert comments before Keywords and Tags
    for (what in c ("Keywords", "Tags")) {
        i <- which (names (template) == what)
        template <- c (
            template [seq (i - 1)],
            "_comment" = paste0 (
                "These ",
                tolower (what),
                " demonstrate the required list structure, and can be deleted"
            ),
            template [seq (i, length (template))]
        )
    }

    # json field names must be unique
    i <- grep ("^\\_comment", names (template))
    names (template) [i] <- paste0 ("_comment", seq (i))

    if (!is.null (metadata)) {
        template <- fill_metadata_template (template, metadata)
    }

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

#' Fill entries in a blank metadata template with values specified in 'metadata'
#' parameter.
#' @noRd
fill_metadata_template <- function (template, metadata) {

    checkmate::assert_list (metadata)
    checkmate::assert_named (metadata)
    if (!all (tolower (names (metadata)) %in% tolower (dcmi_terms ()))) {
        stop ("metadata can only contain items listed in 'dcmi_terms()'.",
            call. = FALSE
        )
    }

    for (i in seq (metadata)) {

        j <- match (
            tolower (names (metadata [i])),
            tolower (names (template))
        )
        if (!nzchar (template [[j]])) {
            template [[j]] <- metadata [[i]]
        } else {
            index_top <- seq (j)
            index_bot <- NULL
            if (j < length (template)) {
                index_bot <- seq (j + 1, length (template))
            }
            template <- c (
                template [index_top],
                metadata [[i]],
                template [index_bot]
            )
            names (template) [j + 1] <- names (template) [j]
        }
    }

    return (template)
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
