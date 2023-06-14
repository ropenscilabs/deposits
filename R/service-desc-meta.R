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
