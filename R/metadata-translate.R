#' Translate DCMI metadata into service-specifi form.
#'
#' @param metadata Validated DCMI metadata as returned from
#' `validate_dcmi_metadata()`.
#' @param service Name of target service for which metadata are to be
#' translated.
#' @noRd
translate_dc_to_service <- function (metadata, service) {

    translations <- get_service_translation (service)
    translations <-
        translations [which (translations$source %in% names (metadata)), ]

    dc <- system.file (fs::path ("extdata", "dc", "schema.json"),
        package = "deposits"
    )
    source_schema <- jsonlite::read_json (dc)$properties

    # -------- metadata translation functions -------
    #
    # initial_names are used in final `rename_metadata_items` call, to ensure
    # only items with initial DCMI names are renamed. The intermediate routines
    # create items already renamed to target format.
    initial_names <- names (metadata)
    metadata <- separate_multiple_sources (
        metadata, translations, source_schema, service
    )
    metadata <- concatenate_multiple_targets (metadata, translations)
    initial_names <-
        names (metadata) [which (names (metadata) %in% initial_names)]
    metadata <- rename_metadata_items (metadata, translations, initial_names)
    metadata <- construct_metadata_paths (metadata, translations)
    # -------- end metadata translation functions -------

    v <- validate_service_metadata (metadata, service)
    if (!v) {
        print (attr (v, "error") [, 1:5])
        stop (
            "Stopping because the metadata terms listed above ",
            "do not confirm with the expected schema for the ",
            service,
            " service."
        )
    }

    return (metadata)
}

translate_service_to_dc <- function (metadata, service) {

    translations <- get_service_translation (service)
    translations <-
        translations [which (translations$target %in% names (metadata)), ]
    s <- translations$source
    translations$source <- translations$target
    translations$target <- s

    dc <- system.file (fs::path ("extdata", service, "schema.json"),
        package = "deposits"
    )
    source_schema <- jsonlite::read_json (dc)$properties

    metadata <- separate_multiple_sources (
        metadata, translations, source_schema, service
    )
    metadata <- concatenate_multiple_targets (metadata, translations)

    metadata <- validate_dcmi_metadata (metadata)

    return (metadata)
}

# Read JSON translation schema from DCMI to specified service, and return as
# `data.frame` object.
#' @noRd
get_service_translation <- function (service) {

    tr <- system.file (fs::path ("extdata", service, "from_dc.json"),
        package = "deposits"
    )
    tr <- jsonlite::read_json (tr, simplify = TRUE)

    # root_path <- tr$rootPath
    target_path <- tr$targetPath

    tr <- tr$translations
    tr_from <- names (tr)
    tr_to <- unname (lapply (tr, function (i) i$targets))
    tr_paths <- unname (lapply (tr, function (i) i$targetPath))
    index <- which (!vapply (tr_paths, is.null, logical (1L)))
    target_path <- rep (target_path, length (tr))
    target_path [index] <- tr_paths [[index]]

    n <- vapply (tr_to, length, integer (1L))
    data.frame (
        source = rep (tr_from, times = n),
        path = rep (target_path, times = n),
        target = unlist (tr_to)
    )
}

#' Separate single source metadata entries into potentially multiple target
#' forms, divided by markdown headers.
#' @noRd
separate_multiple_sources <- function (metadata, translations,
                                       source_schema, service) {

    index <- which (duplicated (translations$source))
    multiple_sources <- unique (translations$source [index])

    for (m in multiple_sources) {
        content <- strsplit (metadata [[m]], "\n") [[1]]
        targets <- grep ("^\\#+", content)
        if (length (targets) > 0) {
            what <- gsub ("^\\#+\\s?", "", content [targets])
            index <- rep (0L, length (content))
            index [targets] <- 1L
            index <- cumsum (index)
            content <- split (content, f = as.factor (index))

            index <- seq_along (content)
            if (length (what) < length (content)) {
                # Then get name of default (first) item)
                index <- index [-1]
                desc <- strsplit (source_schema [[m]]$description, ";") [[1]]
                desc <- grep (service, desc, value = TRUE)
                desc_target <- gsub (
                    "\\:\\s?",
                    "",
                    regmatches (desc, regexpr ("\\:.*$", desc))
                )
                names (content) [1] <- desc_target
            }
            content [index] <- lapply (content [index], function (i) i [-1])
            names (content) [index] <- what

            content <- lapply (content, function (i) {
                while (!nzchar (i [1])) {
                    i <- i [-1]
                }
                while (!nzchar (i [length (i)])) {
                    i <- i [-length (i)]
                }
                return (i)
            })

            metadata <- c (metadata [which (!names (metadata) == m)], content)
        }
    }

    return (metadata)
}

#' Concatenate potentially multiple source items into single target items,
#' constructing markdown-formatted headers to separate each.
#' @noRd
concatenate_multiple_targets <- function (metadata, translations) {

    index <- which (duplicated (translations$target))
    multiple_targets <- unique (translations$target [index])

    for (m in multiple_targets) {
        sources <- translations$source [translations$target == m]
        metadata [sources] <- lapply (metadata [sources], function (i) {
            paste0 (i, collapse = ", ")
        })
        content <- cbind (sources, unlist (metadata [sources]))
        content [, 1] <- paste0 ("## ", content [, 1])
        content <-
            apply (content, 1, function (i) paste0 (i, collapse = "\n\n"))
        content <- paste0 (content, collapse = "\n\n")

        metadata <- metadata [which (!names (metadata) %in% sources)]
        metadata [m] <- content
    }

    return (metadata)
}

#' Rename items from values in "source" to values in "target".
#'
#' This is called after `separate_multiple_sources()` and
#' `concatenate_multiple_targets()`, both of which potentially construct
#' entities already renamed to target format. This then only remaps entries with
#' names matching any entries in source format.
#' @noRd
rename_metadata_items <- function (metadata, translations, initial_names) {

    initial_index <- match (initial_names, names (metadata))
    index <- match (names (metadata) [initial_index], translations$source)
    names (metadata) [initial_index] <- translations$target [index]

    return (metadata)
}

#' Use the 'path' element of the metadata translation table to rearrange
#' metadata items into the paths specified there.
#'
#' Note that this currently only works for single-depth paths, and will need
#' modification for any systems with multiple path components.
#'
#' @param metadata Service-specific metadata converted through application of
#' all of the preceding functions in this file.
#' @return Modified version of `metadata`, with items rearranged into
#' sub-components as specified by `translations$path`.
#' @noRd
construct_metadata_paths <- function (metadata, translations) {

    index <- which (translations$path == "/")
    root_targets <- translations$target [index]
    root <- metadata [root_targets]
    metadata <- metadata [-which (names (metadata) %in% root_targets)]

    if (length (metadata) == 0L) {
        return (root)
    }

    translations <- translations [-index, ]
    translations <- split (translations, f = as.factor (translations$path))

    res <- lapply (translations, function (i) {
        metadata [which (names (metadata) %in% i$target)]
    })
    paths <- vapply (translations,
        function (i) gsub ("^\\/", "", i$path [1]),
        character (1L),
        USE.NAMES = FALSE
    )
    names (res) <- paths

    return (c (root, res))
}
