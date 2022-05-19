function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "https://api.figshare.com/v2/account/articles/",
        "api/articles",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://api.figshare.com/v2/",
        "api/",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "figshare.com/upload/",
        "up/",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://sandbox.zenodo.org/api/",
        "sbapi/",
        fixed = TRUE
    )

    ptn <- paste0 (rep ("[a-z0-9]*", 5), collapse = "\\-")
    resp <- httptest2::gsub_response (
        resp,
        paste0 ("files/", ptn, "/"),
        "files/hash/",
        fixed = FALSE
    )

    return (resp)
}
