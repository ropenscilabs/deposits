# Service-specific functions called from 'desc-to-meta.R'

#' Convert character license strings to required figshare format of integer
#' values. This function has no effect for any other service.
#' @noRd
desc_license <- function (desc, service) {

    ret <- desc$License

    if (service == "figshare") {
        # url <- "https://api.figshare.com/v2/licenses"
        # cli <- depositsClient$new (service = "figshare")
        # req <- create_httr2_helper (url, cli$headers$Authorization, "GET")
        # resp <- httr2::req_perform (req)
        # licenses <- httr2::resp_body_json (resp)
        # license <- do.call (rbind, licenses)
        licenses <- data.frame (
            value = 1:7,
            name = c (
                "CC\\sBY\\s4\\.0",
                "CC0",
                "MIT",
                "GPL",
                "GPL(.*?)2",
                "GPL(.*?)3",
                "Apache\\s2"
            )
        )
        index <- vapply (
            licenses$name,
            function (i) grepl (i, ret, ignore.case = TRUE),
            logical (1L)
        )
        ret <- NULL
        if (any (index)) {
            ret <- max (which (index))
        }
    }

    return (ret)
}

desc_creator_service <- function (creators, service) {

    if (service == "zenodo") {
        # Zenodo only has "name" and "orcid", so need to
        # remove "first_name", "last_name", and "email":
        creators <- lapply (creators, function (i) {
            i$first_name <- i$last_name <- i$email <- NULL
            return (i)
        })
    }

    return (creators)
}

#' Add additional fields required by Figshare if they are present in DESC file.
#'
#' These fields include categories and keywords, and are placed in the 'subject'
#' metadata item.
#'
#' @param meta Initial metadata from description file.
#' @noRd
desc_subjects_service <- function (meta, descfile, service) {

    if (service == "figshare") {

        fs_cat <- grep (
            "figsharecategor",
            names (descfile),
            ignore.case = TRUE
        )

        if (length (fs_cat) == 1L) {

            cats <- strsplit (descfile [[fs_cat]], split = ",")
            cats <- as.integer (cats [[1]])
            meta$subject <- list (categories = as.list (cats))
        }

        fs_kw <- grep ("keyword", names (descfile), ignore.case = TRUE)
        if (length (fs_kw) == 1L) {

            kws <- strsplit (descfile [[fs_kw]], split = ",") [[1]]
            kws <- as.list (sub ("^\\s+|\\s+$", "", kws))

            if ("subject" %in% names (meta)) {
                meta$subject$keywords <- kws
            } else {
                meta$subect <- list (keywords = kws)
            }
        }
    }

    return (meta)
}
