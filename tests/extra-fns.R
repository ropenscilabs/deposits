
fill_meta <- function () {

    filename <- file.path (tempdir (), "meta.json")
    if (file.exists (filename)) {
        chk <- file.remove (filename)
    }

    out <- deposits_metadata_template (filename)
    meta <- readLines (filename)

    i <- grep ("Abstract\\\"\\:", meta)
    meta [i] <- gsub ("\\\"\\\"", "\"This is the abtract\"", meta [i])

    i <- grep ("Title\\\"\\:", meta)
    meta [i] <- gsub ("\\\"\\\"", "\"A Really Good Title\"", meta [i])

    i <- grep ("Creator\\\"\\:", meta)
    meta [i] <- gsub ("\\\"\\\"", "\"A. Person\"", meta [i])

    writeLines (meta, filename)

    return (filename)
}
