#' Validate metadata and convert to service-specific form
#'
#' This function is a wrapper used to call the following two functions in
#' sequence.
#'
#' @param metadata Metadata as list or filename.
#' @param service Name of deposits service
#' @return A list of two elements: 'dcmi' holding validated DCMI metadata, and
#' 'service' holding the validated, service-specific translation of 'dcmi'
#' @noRd
validate_metadata <- function (metadata, service) {

    metadata_dcmi <- validate_dcmi_metadata (metadata, service)
    metadata_service <-
        translate_dc_to_service (metadata_dcmi, service = service)

    if (service == "figshare" &&
        any (c ("custom_fields", "custom_fields_list") %in%
            names (metadata_service))) {
        stop (
            "Figshare does not currently allow custom fields ",
            "for individual items. See ",
            "https://help.figshare.com/admin-faq under ",
            "'Does Figshare support metadata per item type?'",
            call. = FALSE
        )
    }

    # convert any markdown-formatted description items to HTML:
    if ("description" %in% names (metadata_service)) {

        desc <- strsplit (metadata_service$description, "\\\\n") [[1]]
        if (length (desc) == 1L) {
            desc <- strsplit (metadata_service$description, "\\\\n") [[1]]
        }
        for (i in 2:3) {
            ptn <- paste0 ("^", paste0 (rep ("\\#", i), collapse = ""), "\\s")
            index <- grep (ptn, desc)
            hptn_open <- paste0 ("<h", i, ">")
            hptn_close <- paste0 ("</h", i, ">")
            if (length (index) > 0L) {
                desc [index] <-
                    paste0 (gsub (ptn, hptn_open, desc [index]), hptn_close)
            }
            index_rm <- which (!nzchar (desc [index + 1]))
            if (length (index_rm) > 0L) {
                desc <- desc [-(index + 1) [index_rm]]
            }
            desc <- paste0 (desc, collapse = "\n")
        }
        if (!identical (desc, metadata_service$description)) {
            metadata_service$description <- desc
        }
    }

    return (list (
        dcmi = metadata_dcmi,
        service = metadata_service,
        local_path = attr (metadata_dcmi, "local_path"),
        num_resources_local = attr (metadata_dcmi, "num_resources_local")
    ))
}

#' validate metadata input to client either as "metadata" parameter, or though
#' `deposit_fill_metadata()` method.
#'
#' This only validates compliance with DCMI terminology, and standardises names
#' of metadata items. DCMI dictates no structural properties of any metadata
#' items, and thus neither does this function.
#'
#' @param metadata Metadata as a list or filename.
#' @param service Name of deposits service; only used if 'metadata' is path to
#' a 'DESCIPTION' file.
#' @return A list of metadata terms, standardised to expected DCMI nomenclature.
#'
#' @noRd
validate_dcmi_metadata <- function (metadata, service) {

    num_resources_local <- 0L
    if (methods::is (metadata, "character")) {
        metadata <- deposits_meta_from_file (metadata, service = service)
        num_resources_local <- attr (metadata, "num_resources_local")
    }
    local_path <- attr (metadata, "local_path")

    # Align all metadata term names with DCMI names:
    nms <- vapply (
        names (metadata), function (n) {
            dc <- dcmi_terms (n)
            dc <- dc [which (dc == n)]
            dc <- ifelse (length (dc) == 0L, NA_character_, dc)
            return (dc)
        }, character (1L)
    )

    index <- which (!nzchar (nms)) # invalid term names
    if (length (index) > 0L) {
        warning (
            "The following metadata terms do not conform ",
            "and will be removed:\n",
            paste0 (names (nms) [index], collapse = "\n"),
            call. = FALSE
        )
        metadata <- metadata [-index]
        nms <- nms [-index]
    }

    index <- which (names (metadata) != unname (nms))
    if (length (index) > 0L) {
        msg <- vapply (
            index,
            function (i) {
                paste0 ("   ", names (metadata) [i], " -> ", nms [i], "\n")
            },
            character (1L)
        )
        message (
            "Names of the following metadata terms have been changed:\n",
            msg
        )
        names (metadata) <- unname (nms)
    }

    metadata <- metadata [order (names (metadata))]

    schema <- system.file (fs::path ("extdata", "dc", "schema.json"),
        package = "deposits"
    )

    f <- fs::file_temp (ext = ".json")
    jsonlite::write_json (metadata, f, auto_unbox = TRUE)
    v <- jsonvalidate::json_validate (f, schema, engine = "ajv", verbose = TRUE)
    fs::file_delete (f)

    if (!v) {
        errs <- attr (v, "error")
        nms <- c ("instancePath", "schemaPath", "keyword", "params", "message")
        required <- errs$parentSchema$items$required
        errs <- errs [, nms]
        if (!is.null (required)) {
            required <- vapply (
                required,
                function (i) ifelse (is.null (i), NA_character_, i [1]),
                character (1L)
            )
            errs <- cbind (errs, required)
        }
        print (errs)
        stop (
            "Stopping because the DCMI metadata terms listed above ",
            "do not conform with the expected schema.",
            call. = FALSE
        )
    }

    attr (metadata, "local_path") <- local_path
    attr (metadata, "num_resources_local") <- num_resources_local

    return (metadata)
}

#' Validate service-specific metadata
#'
#' The validation is performed via JSON schemas included in the 'inst/extdata'
#' directory of this package, one for each deposits service. These schemas
#' specify names and details of all expected metadata terms for each service.
#'
#' @param metadata Service-specific metadata
#' @return Results of `jsonvalidate::json_validate`.
#'
#' @noRd
validate_service_metadata <- function (metadata, service) {

    schema <- system.file (fs::path ("extdata", service, "schema.json"),
        package = "deposits"
    )

    f <- fs::file_temp (ext = ".json")
    jsonlite::write_json (metadata, f, auto_unbox = TRUE)
    res <-
        jsonvalidate::json_validate (f, schema, engine = "ajv", verbose = TRUE)

    fs::file_delete (f)

    return (res)
}

deposits_meta_from_file <- function (filename = NULL,
                                     frictionless_json_name,
                                     service = NULL) {

    local_path <- NULL # for local_path client field.
    checkmate::assert_character (filename, len = 1L)
    file_is_dcf <- FALSE
    if (fs::is_dir (filename)) {
        # Only place where 'datapackage.json' is hard-coded:
        local_path <- filename
        filename <- fs::path (filename, "datapackage.json")
        if (!fs::file_exists (filename)) {
            filename <- dcf_path (local_path)
            file_is_dcf <- fs::file_exists (filename)
        }
    } else {
        file_is_dcf <- is_dcf (filename)
        local_path <- fs::path_dir (filename)
    }
    checkmate::assert_file_exists (filename)

    if (file_is_dcf) {

        # Current presumed only to be the 'DESCRIPTION' file of an R package:
        filename <- dcf_path (filename)
        descfile <- data.frame (read.dcf (filename))
        desc <- strsplit (descfile$Description, "\\n") [[1]]
        desc <- paste0 (desc, collapse = " ")
        meta <- list (
            title = descfile$Title,
            description = paste0 (
                "## Description\\n",
                desc,
                "\\n\\n## Version\\n", descfile$Version
            ),
            creator = desc_creators (descfile, service = service),
            license = desc_license (descfile, service = service),
            format = "software"
        )

        # in service-desc-meta.R:
        meta <- desc_subjects_service (meta, descfile, service)

        attr (meta, "num_resources_local") <- 0

    } else {

        meta <- readLines (filename, warn = FALSE)
        check <- jsonlite::validate (meta)
        if (!check) {
            stop ("json is not valid.")
        }

        num_resources_local <- 0L
        meta <- jsonlite::read_json (filename)
        # if (all (c ("profile", "metadata", "resources") %in% names (meta))) {
        if (all (c ("metadata", "resources") %in% names (meta))) {
            # datapackage.json:
            num_resources_local <- length (meta$resources)
            meta <- meta$metadata
        }
        
        # remove comments defined names starting with "_"
        meta <- meta[!grepl("^_", names(meta))]

        meta <- meta [which (lapply (meta, length) > 0L)]

        attr (meta, "num_resources_local") <- num_resources_local
    }
    attr (meta, "local_path") <- local_path

    return (meta)
}
