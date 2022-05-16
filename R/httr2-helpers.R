
create_httr2_helper <- function (url, headers, method) {

    req <- httr2::request (url)
    req <- httr2::req_headers (req, "Authorization" = headers)
    req <- httr2::req_method (req, method)

    return (req)
}
