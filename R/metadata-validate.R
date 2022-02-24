validate_terms <- function (terms, deposit = "zenodo") {

    deposit <- match.arg (deposit, c ("figshare", "zenodo"))

    if (deposit == "zenodo") {
        res <- validate_zenodo_terms (terms)
    } else if (deposit == "figshare") {
        res <- validate_figshare_terms (terms)
    }

    return (res)
}

validate_zenodo_terms <- function (terms) {

    out <- NULL

    meta <- terms$metadata
    terms$metadata <- NULL

    f <- system.file (file.path ("extdata", "zenodoTerms.csv"),
                      package = "deposits")
    zen_terms <- utils::read.csv (f)
    for (i in seq (ncol (zen_terms))) {
        zen_terms [, i] <- gsub ("^\\s+|\\s+$", "", zen_terms [, i])
    }
    zen_terms$metadata <- as.logical (zen_terms$metadata)

    zen_meta_terms <- zen_terms [which (zen_terms$metadata), ]
    zen_terms <- zen_terms [which (!zen_terms$metadata), ]

    index <- which (zen_meta_terms$term %in% names (meta))
    zen_meta_terms <- zen_meta_terms [index, ]
    zen_terms <- zen_terms [which (zen_terms$term %in% names (terms)), ]

    # check terms
    for (i in seq (nrow (zen_terms))) {

        if (nzchar (zen_terms$vocabulary [i])) {
            values <- strsplit (zen_terms$vocabulary [i], "\\|") [[1]]
            term_i <- terms [[zen_terms$term [i]]]
            if (!term_i %in% values) {
                out <- c (out,
                          paste0 ("Data [",
                                  zen_terms$term [i],
                                  " = '",
                                  term_i,
                                  "'] not in required vocabulary of [",
                                  zen_terms$vocabulary [i],
                                  "]"))
            }
        } else if (zen_terms$format [i] == "integer") {
            if (suppressWarnings (is.na (as.integer (term_i)))) {
                out <- c (out,
                          paste0 ("Data [",
                                  zen_terms$term [i],
                                  "] must be an integer."))
            }
        }
    }

    # check meta_terms
    for (i in seq (nrow (zen_meta_terms))) {

        if (grepl ("\\.csv$", zen_meta_terms$vocabulary [i])) {
            f <- system.file (file.path ("extdata",
                                         zen_meta_terms$vocabulary [i]),
                              package = "deposits")
            voc <- utils::read.csv (f)
            if (zen_meta_terms$term [i] == "license") {
                voc <- c ("cc-zero", "cc-by", voc$id)
            }
            term_i <- meta [[zen_meta_terms$term [i]]]
            if (zen_meta_terms$format [i] == "array") {
                if (!is.list (term_i)) {
                    out <- c (out,
                              paste0 ("Metadata [",
                                      zen_meta_terms$term [i],
                                      "] must be an array"))
                }
            } else if (!term_i %in% voc) {
                out <- c (out,
                          paste0 ("Metadata [",
                                  zen_meta_terms$term [i],
                                  " = '",
                                  term_i,
                                  "'] not in required vocabulary."))
            }
        } else if (nzchar (zen_meta_terms$vocabulary [i])) {
            values <- strsplit (zen_meta_terms$vocabulary [i], "\\|") [[1]]
            term_i <- meta [[zen_meta_terms$term [i]]]
            if (zen_meta_terms$format [i] == "array") {

                term_names <- unique (unlist (lapply (term_i, names)))
                if (!all (term_names %in% values)) {
                    out <- c (out,
                              paste0 ("Metadata [",
                                      zen_meta_terms$term [i],
                                      "] must be an array/list ",
                                      "with names in [",
                                      paste0 (values, collapse = ", "),
                                      "]"))
                }
            } else if (!term_i %in% values) {
                out <- c (out,
                          paste0 ("Metadata [",
                                  zen_meta_terms$term [i],
                                  " = '",
                                  term_i,
                                  "'] not in required vocabulary of [",
                                  zen_meta_terms$vocabulary [i],
                                  "]"))
            }
        } else if (zen_meta_terms$format [i] == "array") {
            term_i <- meta [[zen_meta_terms$term [i]]]
            if (!is.list (term_i)) {
                out <- c (out,
                          paste0 ("Metadata [",
                                  zen_meta_terms$term [i],
                                  "] must be an array/list object"))
            }
        }
    }

    return (out)
}
