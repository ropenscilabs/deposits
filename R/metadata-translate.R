#' Translate DCMI metadata into service-specifi form.
#'
#' @param meta Validated DCMI metadata as returned from
#' `validate_dcmi_metadata()`.
#' @param service Name of target service for which metadata are to be
#' translated.
#' @noRd
translate_dc_to_service <- function (meta, service) {

    translations <- get_service_translation (service)
    translations <-
        translations [which (translations$source %in% names (meta)), ]

    dc <- system.file (fs::path ("extdata", "dc", "schema.json"),
        package = "deposits"
    )
    source_schema <- jsonlite::read_json (dc)$properties

    meta <- separate_multiple_sources (
        meta, translations, source_schema, service
    )
    meta <- concatenate_multiple_targets (meta, translations)

    v <- validate_service_metadata (meta, service)
    if (!v) {
        print (attr (v, "error") [, 1:5])
        stop (
            "Stopping because the metadata terms listed above ",
            "do not confirm with the expected schema for the ",
            service,
            " service."
        )
    }

    return (meta)
}

translate_service_to_dc <- function (meta, service) {

    translations <- get_service_translation (service)
    translations <-
        translations [which (translations$target %in% names (meta)), ]
    s <- translations$source
    translations$source <- translations$target
    translations$target <- s

    dc <- system.file (fs::path ("extdata", service, "schema.json"),
        package = "deposits"
    )
    source_schema <- jsonlite::read_json (dc)$properties

    meta <- separate_multiple_sources (
        meta, translations, source_schema, service
    )
    meta <- concatenate_multiple_targets (meta, translations)

    meta <- validate_dcmi_metadata (meta)

    return (meta)
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
separate_multiple_sources <- function (meta, translations,
                                       source_schema, service) {

    index <- which (duplicated (translations$source))
    multiple_sources <- unique (translations$source [index])

    for (m in multiple_sources) {
        content <- strsplit (meta [[m]], "\n") [[1]]
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

            meta <- c (meta [which (!names (meta) == m)], content)
        }
    }

    return (meta)
}

#' Concatenate potentially multiple source items into single target items,
#' constructing markdown-formatted headers to separate each.
#' @noRd
concatenate_multiple_targets <- function (meta, translations) {

    index <- which (duplicated (translations$target))
    multiple_targets <- unique (translations$target [index])

    for (m in multiple_targets) {
        sources <- translations$source [translations$target == m]
        meta [sources] <- lapply (meta [sources], function (i) {
            paste0 (i, collapse = ", ")
        })
        content <- cbind (sources, unlist (meta [sources]))
        content [, 1] <- paste0 ("## ", content [, 1])
        content <-
            apply (content, 1, function (i) paste0 (i, collapse = "\n\n"))
        content <- paste0 (content, collapse = "\n\n")

        meta <- meta [which (!names (meta) %in% sources)]
        meta [m] <- content
    }

    return (meta)
}

#' Validate service-specific metadata
#'
#' The validation is performed via JSON schemas included in the 'inst/extdata'
#' directory of this package, one for each deposits service. These schemas
#' specify names and details of all expected metadata terms for each service.
#'
#' @param meta Service-specific metadata
#' @return Results of `jsonvalidate::json_validate`.
#'
#' @noRd
validate_service_metadata <- function (meta, service) {

    schema <- system.file (fs::path ("extdata", service, "schema.json"),
        package = "deposits"
    )

    f <- fs::file_temp (ext = ".json")
    jsonlite::write_json (meta, f, auto_unbox = TRUE)
    res <-
        jsonvalidate::json_validate (f, schema, engine = "ajv", verbose = TRUE)

    return (res)
}
