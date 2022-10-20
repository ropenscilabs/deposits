
construct_md_list_figshare <- function (values, term_map) {

    if ("authors" %in% names (values) && !is.list (values$authors)) {
        values$authors <- list (list (name = values$authors))
    }
    if ("categories" %in% names (values) &&
        !is.integer (values$categories)) {
        message (
            "Figshare categories must be integer values; ",
            "the provided values will be removed."
        )
        values$categories <- NULL
    }
    if ("timeline" %in% names (values)) {
        # figshare timeline allows only:
        # [firstOnline, publisherPublication, publisherAcceptance]
        # For demonstration purposes, only use firstOneline for now
        values$timeline <- list (firstOnline = values$timeline [1])
    }
    if ("license" %in% names (values)) {
        if (is.na (suppressWarnings (as.integer (values$license)))) {
            warning (
                "Figshare licenses must be integer-valued; ",
                "the value will be reset to '1' = 'CC-BY'"
            )
            values$license <- 1L
        }
    }

    values <- httptest2_dcmi_created (values)

    return (values)
}
