upload_zenodo_file <- function (id, url, headers, path) {

    con <- crul::HttpClient$new (url, headers = headers)
    res <- con$get ()
    res$raise_for_status ()
    res <- jsonlite::fromJSON (res)

    bucket_link <- res$links$bucket
    filename <- basename (path)
    file_url <- paste0 (bucket_link, "/", filename)
    headers <- c (headers, "Content-Type" = "application/octet-stream")

    con <- crul::HttpClient$new (file_url, headers = headers)
    res <- con$put (body = list (f = crul::upload (path)))
    res$raise_for_status ()

    return (res)
}
