
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

    checkmate::assert_character (filename, len = 1L)
    filepath <- dirname (normalizePath (filename, mustWork = FALSE))
    checkmate::assert_directory_exists (filepath)
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

    checkmate::assert_character (filename, len = 1L)
    checkmate::assert_directory_exists (dirname (normalizePath (filename)))
    if (!file.exists (filename)) {
        stop ("filename [", filename, "] does not exist.")
    }
    checkmate::assert_character (id, len = 1L)

    meta <- readLines (filename)
    check <- jsonlite::validate (meta)
    if (!check) {
        stop ("json is not valid.")
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
    index <- which (nzchar (terms$Zenodo) | nzchar (terms$Figshare))
    return (terms [index, ])
}

#' Constrct map between DCMI terms and those of nominated deposits service.
#' @noRd
get_dcmi_term_map <- function (deposit = "zenodo") {

    terms <- load_meta_terms ()
    this_col <- grep (deposit, names (terms), ignore.case = TRUE)
    terms <- terms [which (nzchar (terms [, this_col])), ]
    terms <- cbind (terms$DC, terms [, this_col])
    terms <- apply (terms, 1, function (i) {
        val <- strsplit (i [2], "\\|") [[1]]
        cbind (rep (i [1], length (val)), val)
    })
    terms <- do.call (rbind, terms)
    # zenodo metadata has "(m)" at end of terms
    terms <- data.frame ("dcmi" = terms [, 1],
                         "deposit" = terms [, 2],
                         "meta" = grepl ("\\(m\\)$", terms [, 2]))
    terms [, 2] <- gsub ("\\(m\\)$", "", terms [, 2])

    return (terms)
}

#' Convert metadata of atom4R::DCEntry object into a list of terms
#'
#' @param metadata The 'metadata' object of a 'deposits' client.
#' @param term_map The 'term_map' object of a 'deposits' client.
#' @noRd
construct_data_list <- function (metadata, term_map) {

    # term_map is constructed so that first DCMI translation is the preferred
    # one, with subsequent ones offering alternative translations
    term_map <- term_map [which (!duplicated (term_map$dcmi)), ]

    values <- lapply (term_map$dcmi, function (i)
                      lapply (metadata [[i]], function (j)
                              j$value))
    names (values) <- term_map$deposit
    values <- values [which (vapply (values, length, integer (1)) > 0L)]
    arrays <- c ("keywords", "contributors")
    index <- which (!names (values) %in% arrays)
    values [index] <- lapply (values [index], function (i)
                              paste0 (i, collapse = ","))

    is_zenodo <- any (term_map$meta)
    if (is_zenodo) {

        index <- which (names (values) %in%
                        term_map$deposit [which (term_map$meta)])
        meta_values <- values [index]
        values <- values [-index]

        req <- list ("upload_type" = "other",
                      "title" = "Title",
                      "creators" = "A. Person",
                      "description" = "Description")

        index <- which (!names (req) %in% names (meta_values))
        meta_values <- c (meta_values, req [index])

        if (!is.list (meta_values$creators)) {
            meta_values$creators <- list (list (name = meta_values$creators))
        } else if (names (meta_values$creators) == "name") {
            meta_values$creators <- list (meta_values$creators)
        }
        if ("upload_type" %in% names (meta_values)) {
            meta_values$upload_type <- tolower (meta_values$upload_type)
        }

        values$metadata <- meta_values

        if (!"created" %in% names (values)) {
            values <- c ("created" = paste0 (Sys.Date ()), values)
        }

    } else {
        if (!is.list (values$authors)) {
            values$authors <- list (list (name = values$authors))
        }
        if (!is.integer (values$categories)) {
            message ("Figshare categories must be integer values; ",
                     "the provided values will be removed.")
            values$categories <- NULL
        }
        if ("timeline" %in% names (values)) {
            # figshare timeline allows only:
            # [firstOnline, publisherPublication, publisherAcceptance]
            # For demonstration purposes, only use firstOneline for now
            values$timeline <- list (firstOnline = values$timeline [1])
        }
        if ("license" %in% names (values) &
            is.na (suppressWarnings (as.integer (values$license)))) {
            warning ("Figshare licenses must be integer-valued; ",
                     "the value will be reset to '1' = 'CC-BY'")
            values$license <- 1L
        }
    }

    return (values)
}
