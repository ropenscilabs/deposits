
metadata_from_deposit <- function (cli, dep) {

    if (cli$name == "figshare") {
        cli <- metadata_from_figshare (cli, dep)
    } else if (cli$name == "zenodo") {
        cli <- metadata_from_zenodo (cli, dep)
    } else {
        stop ("unknown deposit [", cli$name, "]")
    }

    return (cli)
}

metadata_from_figshare <- function (cli, dep) {

    lens <- vapply (dep, length, integer (1L))
    dep_fields <- names (lens [which (lens > 0L)])
    dep_fields <- dep_fields [which (dep_fields %in% cli$term_map$deposit)]
    term_map <- cli$term_map [which (cli$term_map$deposit %in% dep_fields), ]
    rownames (term_map) <- NULL

    dcmi <- cli$metadata
    if (is.null (dcmi)) {
        dcmi <- atom4R::DCEntry$new ()
    }

    for (i in seq (nrow (term_map))) {
        dc_fn <- grep (paste0 ("addDC", term_map$dcmi [i]),
            names (dcmi),
            value = TRUE,
            ignore.case = TRUE
        )

        value <- dep [[term_map$deposit [i]]]
        # https://github.com/eblondel/atom4R/issues/14
        if (dc_fn == "addDCCreator") {
            value <- paste0 (value$full_name, collapse = ", ")
        }
        do.call (dcmi [[dc_fn]], list (value))
    }
    dcmi$validate ()

    cli$metadata <- dcmi

    return (cli)
}
