#' Translate DCMI metadata into service-specifi form.
#'
#' @param metadata Validated DCMI metadata as returned from
#' `validate_dcmi_metadata()`.
#' @param service Name of target service for which metadata are to be
#' translated.
#' @noRd
translate_dc_to_service <- function (metadata, service) {

    translations <- get_service_translation (service)

    # Extract translation of names of keys within items:
    translation_items <- translations [grep ("\\@", translations$source), ]
    translation_items <- data.frame (
        item = gsub ("\\@.*$", "", translation_items$source),
        source = gsub ("^.*\\@", "", translation_items$source),
        path = translation_items$path,
        target = gsub ("^.*\\@", "", translation_items$target)
    )

    index <- which (translation_items$item %in% names (metadata))
    translation_items <- translation_items [index, ]
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
    if (nrow (translation_items) > 0L) {
        metadata <- rename_metadata_keys (metadata, translation_items)
    }
    initial_names <- names (metadata)
    metadata <- separate_multiple_sources (
        metadata, translations, source_schema, service
    )
    metadata <- concatenate_multiple_targets (metadata, translations)
    initial_names <-
        names (metadata) [which (names (metadata) %in% initial_names)]

    metadata <- rename_metadata_items (metadata, translations, initial_names)
    metadata <- construct_metadata_paths (metadata, translations)
    metadata <- insert_default_service_metadata (metadata, service)
    # -------- end metadata translation functions -------

    v <- validate_service_metadata (metadata, service)
    if (!v) {
        print (attr (v, "error") [, 1:5])
        stop (
            "Stopping because the metadata terms listed above ",
            "do not conform with the expected schema for the ",
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
    if (length (index) > 0L) {
        target_path [index] <- tr_paths [[index]]
    }

    n <- vapply (tr_to, length, integer (1L))
    res <- data.frame (
        source = rep (tr_from, times = n),
        path = rep (target_path, times = n),
        target = unlist (tr_to)
    )

    # plus any additional translations of key names within items:
    index <- which (vapply (
        tr,
        function (i) "translations" %in% names (i),
        logical (1L)
    ))
    if (length (index) > 0L) {
        tr_items <- lapply (index, function (i) {
            out <- tr [[i]]$translations
            cbind (item = rep (names (tr) [i], nrow (out)), out)
        })
        tr_items <- do.call (rbind, tr_items)
        rownames (tr_items) <- NULL
        out <- data.frame (
            source = paste0 (tr_items$item, "@", tr_items$source),
            path = res$path [match (tr_items$item, res$source)],
            target = paste0 (tr_items$item, "@", tr_items$target)
        )

        res <- rbind (res, out)
    }

    return (res)
}

#' Use schema translation of names of keys within items to modify metadata
#' names.
#'
#' This is used, for example, to change 'metadata$created$orcid' to
#' 'metadata$created$orcid_id'.
#'
#' @noRd
rename_metadata_keys <- function (metadata, translation_items) {

    for (i in seq_len (nrow (translation_items))) {
        item_i <- translation_items$item [i]
        source_i <- translation_items$source [i]
        target_i <- translation_items$target [i]

        index <- which (names (metadata [[item_i]]) == source_i)
        if (length (index) > 0L) {
            names (metadata [[item_i]]) [index] <- target_i
        } else {
            metadata [[item_i]] <- lapply (metadata [[item_i]], function (j) {
                index <- which (names (j) == source_i)
                names (j) [index] <- target_i
                return (j)
            })
        }
    }

    return (metadata)
}

#' Separate single source metadata entries into potentially multiple target
#' forms, divided by markdown headers.
#' @noRd
separate_multiple_sources <- function (metadata, translations,
                                       source_schema, service) {

    index <- which (duplicated (translations$source))
    multiple_sources <- unique (translations$source [index])

    tr_full <- get_service_translation (service)
    service_schema <- system.file (fs::path ("extdata", service, "schema.json"),
        package = "deposits"
    )
    service_schema <- jsonlite::read_json (service_schema)$properties
    if (service == "zenodo") {
        service_schema <- service_schema$metadata$properties
    }

    for (m in multiple_sources) {

        content <- strsplit (metadata [[m]], "\n") [[1]]
        targets <- grep ("^\\#+", content)

        if (targets [1] > min (which (nzchar (content)))) {
            # First content is default without markdown header
            tr_target <- tr_full$target [tr_full$source == m] [1]
            content <- c (paste0 ("## ", tr_target), content)
            targets <- grep ("^\\#+", content)
        }

        what <- gsub ("^\\#+\\s?", "", content [targets])
        index <- which (what %in% tr_full$target)
        targets <- targets [index]
        what <- what [index]

        # Check that translation source is correct:
        chk <- lapply (what, function (s) {
            src <- unique (tr_full$source [tr_full$target %in% s])
            out <- list ()
            if (!m %in% src) {
                out <- list (what = s, src = src)
            }
            return (out)
        })
        chk <- chk [which (vapply (chk, length, integer (1L)) > 0L)]

        if (length (chk) > 0L) {
            what <- chk [[1]]$what
            src <- chk [[1]]$src
            stop (
                "Metadata source for [", what, "] should be",
                ifelse (length (src) > 1, " one of", ""), " [",
                paste0 (src, collapse = ", "), "] and not [", m, "]",
                call. = FALSE
            )
        }

        if (length (targets) > 0) {
            index <- rep (0L, length (content))
            index [targets] <- 1L
            index <- cumsum (index)
            content <- split (content, f = as.factor (index))

            index <- seq_along (content)
            content [index] <- lapply (content [index], function (i) i [-1])
            names (content) [index] <- what

            content <- lapply (content, function (i) {
                while (!nzchar (i [1])) {
                    i <- i [-1]
                }
                while (!nzchar (i [length (i)])) {
                    i <- i [-length (i)]
                }
                return (paste0 (i, collapse = "\n"))
            })

            # Get expected schema type, and convert to array if needed:
            schema_types <- lapply (names (content), function (nm) {
                itype <- ifelse (
                    "items" %in% names (service_schema [[nm]]),
                    service_schema [[nm]]$items$type,
                    NA_character_
                )
                c (service_schema [[nm]]$type, itype)
            })
            schema_types <- data.frame (cbind (
                names (content),
                do.call (rbind, schema_types)
            ))
            names (schema_types) <- c ("name", "type", "item_type")

            for (i in seq_along (content)) {
                this_type <- schema_types$type [i]
                if (this_type == "array") {
                    this_content <- strsplit (content [[i]], split = "\\,\\s?|\\n") [[1]]
                    content [[i]] <- as.list (this_content)
                }
            }

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

    root <- NULL
    index <- which (translations$path == "/")

    if (length (index) > 0L) {
        root_targets <- translations$target [index]
        root_targets <-
            unique (root_targets [which (root_targets %in% names (metadata))])
        root <- metadata [root_targets]
        metadata <- metadata [-which (names (metadata) %in% root_targets)]

        translations <- translations [-index, ]
    }

    if (length (metadata) == 0L) {
        return (root)
    }

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

#' Extract all required metadata keys from service JSON schema
#'
#' @return A `data.frame` with three columns of (1) name of required key; (2)
#' default value; and (3) path in metadata structure.
#' @noRd
required_service_values <- function (service) {

    schema <- system.file (fs::path ("extdata", service, "schema.json"),
        package = "deposits"
    )
    target_schema <- jsonlite::read_json (schema, simplify = TRUE)
    required <- target_schema$required
    target_path <- rep ("", length (required))

    target_schema <- target_schema$properties
    index <- which (vapply (
        target_schema,
        function (i) "properties" %in% names (i),
        logical (1L)
    ))
    trawl_these <- trawl_names <- NULL
    if (length (index) > 0L) {
        trawl_these <- list (target_schema [[index]])
        trawl_names <- names (target_schema) [index]
    }

    while (length (trawl_these) > 0L) {

        i <- trawl_these [[1]]
        nm_i <- trawl_names [1]

        required <- c (required, i$required)
        target_path <- c (target_path, rep (nm_i, length (i$required)))

        index <- which (vapply (
            i$properties,
            function (i) "properties" %in% names (i),
            logical (1L)
        ))

        trawl_these <- c (trawl_these, i$properties [index])
        trawl_names <- c (
            trawl_names,
            paste0 (nm_i, "/", names (i$properties) [index])
        )

        trawl_these <- trawl_these [-1]
        trawl_names <- trawl_names [-1]
    }

    # Then extract default values.
    res <- data.frame (
        name = required,
        path = target_path
    )
    tmp <- split (res, f = as.factor (res$path))

    defaults <- lapply (tmp, function (i) {

        if (!nzchar (i$path [1])) {

            lapply (target_schema [i$name], function (j) {
                res_j <- NA_character_
                if ("default" %in% names (j)) {
                    res_j <- j$default
                }
                return (res_j)
            })

        } else {

            path <- strsplit (i$path [1], "\\/") [[1]]
            s <- target_schema
            while (length (path) > 0L) {
                s <- s [[path]]$properties
                path <- path [-1]
            }

            lapply (s [i$name], function (j) {
                res_j <- NA_character_
                if ("default" %in% names (j)) {
                    res_j <- j$default
                }
                return (res_j)
            })
        }
    })

    res$default <- unname (unlist (defaults))

    return (res)
}

insert_default_service_metadata <- function (metadata, service) { # nolint

    defaults <- required_service_values (service)
    # Fill default date-time stamps:
    index <- which (
        is.na (defaults$default) &
            grepl ("created", defaults$name, ignore.case = TRUE)
    )
    defaults$default [index] <- deposit_timestamp (Sys.time ())
    index <- which (
        is.na (defaults$default) &
            grepl ("date", defaults$name, ignore.case = TRUE)
    )
    defaults$default [index] <- paste0 (Sys.Date ())

    defaults <- defaults [which (!is.na (defaults$default)), ]

    for (p in unique (defaults$path)) {

        defaults_p <- defaults [which (defaults$path == p), ]
        other_paths <- unique (defaults$path [which (defaults$path != p)])

        if (!nzchar (p)) {

            # root path
            meta_p <- metadata [which (!names (metadata) %in% other_paths)]
            meta_not_p <- metadata [which (names (metadata) %in% other_paths)]
            index <- which (!defaults_p$name %in% names (meta_p))
            if (length (index) > 0L) {
                nms0 <- names (meta_p)
                meta_p <- c (meta_p, defaults_p$default [index])
                names (meta_p) <- c (nms0, defaults_p$name [index])
                metadata <- c (meta_p, meta_not_p)
            }

        } else {

            meta_p <- metadata [[which (names (metadata) %in% p)]]
            meta_not_p <- metadata [which (!names (metadata) %in% p)]
            index <- which (!defaults_p$name %in% names (meta_p))
            if (length (index) > 0L) {
                nms0 <- names (meta_p)
                meta_p <- c (meta_p, defaults_p$default [index])
                names (meta_p) <- c (nms0, defaults_p$name [index])

                nms0 <- names (meta_not_p)
                metadata <- c (meta_not_p, list (meta_p))
                names (metadata) <- c (nms0, p)
            }
        }
    }

    return (metadata)
}
