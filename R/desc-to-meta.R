#' @title Check whether file is a .dcf file
#' @param path Path to directory or file
#' @return `TRUE` if file is a .dcf file, `FALSE` otherwise
#' @noRd
is_dcf <- function (path) {

    path <- fs::path_abs (path)

    if (fs::is_dir (path)) {
        path <- fs::path (path, "DESCRIPTION")
    }
    if (!fs::file_exists (path)) {
        return (FALSE)
    }

    desc <- read.dcf (path)
    ret <- all (c ("Package", "Title", "Version") %in% colnames (desc))

    return (ret)
}

#' @title Get path to .dcf file if it exists
#' @param path Path to directory or file
#' @return Path to .dcf file, or NULL if no .dcf file can be found
#' @noRd
dcf_path <- function (path) {

    path <- fs::path_abs (path)

    if (fs::is_dir (path)) {
        path <- fs::path (path, "DESCRIPTION")
    }

    ret <- NULL
    if (is_dcf (path)) {
        ret <- path
    }
    return (ret)
}

desc_authors <- function (desc) {

    given <- family <- role <- email <- comment <- NA_character_
    if ("Author" %in% names (desc)) {
        name <- desc$Author
    } else if ("Authors.R" %in% names (desc)) {
        auts <- eval (parse (text = desc$Authors.R))
        given <- format (auts, include = "given")
        family <- format (auts, include = "family")
        name <- paste (given, family)
        role <- format (auts, include = "role")
        email <- format (auts, include = "email")
        comment <- format (auts, include = "comment")
    }

    res <- data.frame (
        name = name,
        given = given,
        family = family,
        role = role,
        email = email,
        comment = comment
    )

    # Get ORCID ID from comment field:
    res$orcid <- NA_character_
    index <- grep ("orcid", res$comment, ignore.case = TRUE)
    if (length (index) > 0L) {
        orcid <- regmatches (
            res$comment [index],
            gregexpr ("<http.*>", res$comment [index])
        )
        orcid <- vapply (orcid, function (i) {
            if (length (i) == 0) {
                return (NA_character_)
            } else {
                return (i [1])
            }
        }, character (1L))
        orcid <- gsub ("^.*orcid.org\\/|>$", "", orcid)
        res$orcid [index] <- orcid
    }

    return (res)
}

desc_creators <- function (desc, service) {

    authors <- desc_authors (desc)

    creators <- lapply (seq_len (nrow (authors)), function (i) {
        res <- list (name = authors$name [i])
        if (nzchar (authors$given [i])) {
            res$first_name <- authors$given [i]
        }
        if (nzchar (authors$family [i])) {
            res$last_name <- authors$family [i]
        }
        if (nzchar (authors$email [i])) {
            res$email <- authors$email [i]
        }
        if (!is.na (authors$orcid [i])) {
            res$orcid <- authors$orcid [i]
        }
        return (res)
    })

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

desc_license <- function (desc, service) {

    if (service == "figshare") {
        return (NULL)
    } else {
        return (desc$License)
    }
}