
#' Convert hostdata from deposit query into `atom4R::DCEntry`.
#' @noRd
metadata_from_deposit <- function (cli, hostdata) {

    if (cli$service == "figshare") {
        dcmi <- metadata_from_figshare (cli, hostdata)
    } else if (cli$service == "zenodo") {
        dcmi <- metadata_from_zenodo (cli, hostdata)
    } else {
        stop ("unknown deposit [", cli$service, "]")
    }

    # standardise "updated" timestamp inserted by atom4R for test env:
    dcmi <- standardise_dcmi_timestamps (dcmi)

    return (dcmi)
}

rm_missing_atom_terms <- function (term_map) {

    missing <- c (
        "hasPart",
        "hasVersion",
        "isPartOf",
        "isReferencedBy",
        "isReplacedBy",
        "isRequiredBy",
        "isVersionOf"
    )

    term_map [which (!term_map$dcmi %in% missing), ]
}

term_map_for_service <- function (cli, hostdata) {

    lens <- vapply (hostdata, length, integer (1L))
    dep_fields <- names (lens [which (lens > 0L)])
    dep_fields <- dep_fields [which (dep_fields %in% cli$term_map$service)]
    term_map <- cli$term_map [which (cli$term_map$service %in% dep_fields), ]

    term_map <- rm_missing_atom_terms (term_map)
    rownames (term_map) <- NULL

    return (term_map)
}


metadata_from_figshare <- function (cli, hostdata) {

    term_map <- term_map_for_service (cli, hostdata)

    dcmi <- atom4R::DCEntry$new ()
    dcmi$verbose.info <- FALSE

    for (i in seq (nrow (term_map))) {
        dc_fn <- grep (paste0 ("addDC", term_map$dcmi [i]),
            names (dcmi),
            value = TRUE,
            ignore.case = TRUE
        )

        value <- hostdata [[term_map$service [i]]]
        # https://github.com/eblondel/atom4R/issues/14
        if (dc_fn == "addDCCreator") {
            value <- paste0 (value$full_name, collapse = ", ")
        }
        do.call (dcmi [[dc_fn]], list (value))
    }
    dcmi$validate ()

    return (dcmi)
}

metadata_from_zenodo <- function (cli, hostdata) {

    term_map <- rbind (
        term_map_for_service (cli, hostdata),
        term_map_for_service (cli, hostdata$metadata)
    )
    term_map <- term_map [which (!duplicated (term_map)), ]

    dcmi <- atom4R::DCEntry$new ()
    dcmi$verbose.info <- FALSE

    for (i in seq (nrow (term_map))) {
        dc_fn <- grep (
            paste0 ("^addDC", term_map$dcmi [i], "$"),
            names (dcmi),
            value = TRUE,
            ignore.case = TRUE
        )

        if (term_map$meta [i]) {
            value <- hostdata$metadata [[term_map$service [i]]]
        } else {
            value <- hostdata [[term_map$service [i]]]
        }
        do.call (dcmi [[dc_fn]], list (value))
    }
    dcmi$validate ()

    return (dcmi)
}
