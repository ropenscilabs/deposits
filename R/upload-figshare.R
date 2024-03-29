#' Upload a file to a nominated figshare article
#'
#' \url{https://docs.figshare.com/#upload_files_example_upload_on_figshare}
#'
#' @param article_id The generic 'deposit_id' parameter passed to the deposits
#' client, where deposits in figshare are called "articles".
#' @param url The generic endpoint for figshare articles.
#' @param headers Generic header including the OAuth token.
#' @param path Path to local file to be uploaded.
#' @noRd
upload_figshare_file <- function (article_id, url, headers, path) {

    article_url <- sprintf ("%s/%s", url, article_id)

    x <- figshare_upload_url (article_id, url, headers, path)
    upload_url <- x$upload_url
    file_id <- x$id
    # upload_token <- x$upload_token

    flist <- figshare_upload_parts (upload_url, headers, path)
    nparts <- length (flist)

    # upload parts:
    for (i in seq (nparts)) {

        url_i <- sprintf ("%s/%s", upload_url, i)
        req <- create_httr2_helper (url_i, headers$Authorization, "PUT")
        req$headers <- c (req$headers,
            "Content-Type" = "application/octet-stream"
        )
        req <- httr2::req_body_file (
            req,
            path = path
        )
        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)
    }

    # complete upload
    file_url <- sprintf ("%s/files/%s", article_url, file_id)
    req <- create_httr2_helper (file_url, headers$Authorization, "POST")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    fs::file_delete (flist)

    # and check article data:
    req <- create_httr2_helper (article_url, headers$Authorization, "GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    # return value is full hostdata with updated files component
    return (httr2::resp_body_json (resp, simplifyVector = TRUE))
}

figshare_upload_url <- function (id, url, headers, path) {

    path <- fs::path (path)
    md5 <- unname (tools::md5sum (path))
    s <- fs::file_size (path)
    body <- jsonlite::toJSON (
        data.frame (
            md5 = md5,
            name = basename (path),
            size = as.integer (s)
        ),
        pretty = FALSE,
        auto_unbox = TRUE
    )
    body <- gsub ("^\\[|\\]$", "", paste0 (body))

    url <- paste0 (url, "/", id, "/files")

    # First get upload location:
    req <- create_httr2_helper (url, headers$Authorization, "POST")
    req$headers <- c (req$headers, "Content-Type" = "application/json")
    req <- httr2::req_body_raw (req, body = paste0 (body))

    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    location <- httr2::resp_body_json (resp)

    # Then a second call to get upload URL:
    file_id <- gsub ("^.*\\/", "", location)
    url <- paste0 (url, "/", file_id)

    req <- create_httr2_helper (url, headers$Authorization, "GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
    httr2::resp_body_json (resp)
}

figshare_upload_parts <- function (upload_url, headers, path) {

    req <- create_httr2_helper (upload_url, headers$Authorization, "GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
    x <- httr2::resp_body_json (resp, simplifyVector = TRUE)

    parts <- x$parts
    part_size <- parts$endOffset [1] + 1
    tmpdir <- fs::path_dir (path)

    withr::with_dir (
        tmpdir,
        system (paste ("split -b", part_size, path, "part_", "--numeric=1"))
    )

    fs::dir_ls (tmpdir, regexp = "part\\_[0-9]+$")
}

#' Current only called from `deposit_delete_file()` method via private
#' `delete_file` method.
#' @param path Always just the file name.
#' @noRd
figshare_delete_file <- function (article_id, service_url, files, headers, path) {

    f <- fs::path_file (path)
    if (!f %in% files$name) {
        stop (
            "File [", f, "] is not held on deposit#", article_id,
            call. = FALSE
        )
    }

    file_id <- files$id [files$name == f]
    del_url <- paste0 (service_url, "/", article_id, "/files/", file_id)

    req <- create_httr2_helper (del_url, headers$Authorization, "DELETE")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)
}
