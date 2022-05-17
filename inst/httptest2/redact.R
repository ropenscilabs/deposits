function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "https://api.figshare.com/v2/account/articles/",
        "api/articles",
        fixed=TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "https://api.figshare.com/v2/",
        "api/",
        fixed=TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "figshare.com/upload/",
        "up/",
        fixed=TRUE
    )

    return (resp)
}
