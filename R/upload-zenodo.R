#' Upload a file to a nominated zenodo article
#'
#' @param deposit_id 'id' parameter passed to the deposits
#' client.
#' @param url The generic endpoint for zenodo articles.
#' @param headers Generic header including the OAuth token.
#' @param path Path to local file to be uploaded.
#' @noRd
upload_zenodo_file <- function (deposit_id, url, headers, path) {

    bucket_link <- get_zenodo_bucket_link (deposit_id, url, headers)

    filename <- basename (path)
    file_url <- paste0 (bucket_link, "/", filename)

    req <- create_httr2_helper (file_url, headers$Authorization, "PUT")
    req$headers <- c (req$headers, "Content-Type" = "application/octet-stream")
    req <- httr2::req_body_file (
        req,
        path = path
    )
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    return (httr2::resp_body_json (resp, simplifyVector = TRUE))
}

get_zenodo_bucket_link <- function (deposit_id, url, headers) {

    url <- sprintf ("%s/%s", url, deposit_id)

    req <- create_httr2_helper (url, headers$Authorization, "GET")
    resp <- httr2::req_perform (req)
    httr2::resp_check_status (resp)

    resp <- httr2::resp_body_json (resp)

    return (resp$links$bucket)
}
