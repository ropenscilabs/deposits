#' Upload a file to a nominated zenodo article
#'
#' @param deposit_id 'id' parameter passed to the deposits
#' client.
#' @param url The generic endpoint for zenodo articles.
#' @param headers Generic header including the OAuth token.
#' @param path Path to local file to be uploaded.
#' @noRd
upload_zenodo_file <- function (deposit_id, url, headers, path) {

    bucket_link <- get_zenodo_bucket_link (depost_id, url, headers)

    filename <- basename (path)
    file_url <- paste0 (bucket_link, "/", filename)
    headers <- c (headers, "Content-Type" = "application/octet-stream")

    con <- crul::HttpClient$new (file_url, headers = headers)
    res <- con$put (body = list (f = crul::upload (path)))
    res$raise_for_status ()

    return (res)
}

get_zenodo_bucket_link <- function (depost_id, url, headers) {

    url <- sprintf ("%s/%s", url, deposit_id)

    con <- crul::HttpClient$new (url, headers = headers)
    res <- con$get ()
    res$raise_for_status ()
    x <- jsonlite::fromJSON (res$parse (encoding = "UTF-8"))

    return (x$links$bucket)
}
