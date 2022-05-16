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

    res <- figshare_upload_url (article_id, url, headers, path)
    res$raise_for_status ()
    x <- jsonlite::fromJSON (res$parse (encoding = "UTF-8"))
    upload_url <- x$upload_url
    file_id <- x$id
    #upload_token <- x$upload_token

    flist <- figshare_upload_parts (upload_url, headers, path)
    nparts <- length (flist)

    # upload parts:
    for (i in seq (nparts)) {
        #upload_url_part <- paste0 (upload_url, "/", i)
        #con <- crul::HttpClient$new (upload_url_part, headers = headers)
        #res <- con$put (body = list (f = crul::upload (flist [i])))
        #res$raise_for_status ()

        req <- httr2::request (sprintf ("%s/%s", upload_url, i))
        req <- httr2::req_headers (
            req,
            "Authorization" = headers$Authorization,
            "Content-Type" = "application/octet-stream"
        )
        req <- httr2::req_body_file (
            req,
            path = path)
        req <- httr2::req_method (req, "PUT")
        resp <- httr2::req_perform (req)
        httr2::resp_check_status (resp)
    }

    # complete upload
    file_url <- sprintf ("%s/files/%s", article_url, file_id)
    req <- httr2::request (file_url)
    req <- httr2::req_headers (
        req,
        "Authorization" = headers$Authorization
    )
    req <- httr2::req_method (req, "POST")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    chk <- file.remove (flist) # nolint

    # and check article data:
    req <- httr2::request (article_url)
    req <- httr2::req_headers (
        req,
        "Authorization" = headers$Authorization
    )
    req <- httr2::req_method (req, "GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    return (httr2::resp_body_json (resp))
}

figshare_upload_url <- function (id, url, headers, path) {

    path <- normalizePath (path)
    md5 <- unname (tools::md5sum (path))
    s <- file.size (path)
    body <- jsonlite::toJSON (
                              data.frame (md5 = md5,
                                          name = basename (path),
                                          size = s),
                              auto_unbox = TRUE
    )
    body <- gsub ("^\\[|\\]$", "", paste0 (body))

    url <- paste0 (url, "/", id, "/files")
    headers <- c (headers, "Content-Type" = "application/json")

    # First get upload location:
    con <- crul::HttpClient$new (url, headers = headers)
    res <- con$post (body = body)
    res$raise_for_status ()

    out <- jsonlite::fromJSON (res$parse (encoding = "UTF-8"))
    location <- out$location

    # Then a second call to get upload URL:
    file_id <- gsub ("^.*\\/", "", location)
    url <- paste0 (url, "/", file_id)
    con <- crul::HttpClient$new (url, headers = headers)
    res <- con$get ()
    res$raise_for_status ()

    return (res)
}

figshare_upload_parts <- function (url, headers, path) {

    con <- crul::HttpClient$new (url, headers = headers)
    res <- con$get ()
    res$raise_for_status ()

    x <- jsonlite::fromJSON (res$parse (encoding = "UTF-8"))
    parts <- x$parts
    part_size <- parts$endOffset [1] + 1
    tmpdir <- dirname (path)

    withr::with_dir (tmpdir,
        system (paste ("split -b", part_size, path, "part_", "--numeric=1"))
        )

    list.files (tmpdir, pattern = "^part\\_", full.names = TRUE)
}
