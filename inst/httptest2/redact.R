function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "https://api.figshare.com/v2/account/articles/",
        "api/articles/",
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
        paste0 ("(files|fup\\-eu\\-west\\-1\\.up)\\/", ptn, "/"),
        "files/hash/",
        fixed = FALSE
    )

    resp <- httptest2::gsub_response (
        resp,
        "\\.xml\\-[a-z0-9]*\\-PUT\\.json$",
        ".xml-123456-PUT.json",
        fixed = FALSE
    )

    # Timestamp pattern:
    ptn <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}T[0-9]{2}\\:[0-9]{2}\\:[0-9]{2}\\.[0-9]+"
    resp <- httptest2::gsub_response (
        resp,
        ptn,
        "2022-01-01T00:00:00",
        fixed = FALSE
    )

    # And dates without times:
    ptn <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}"
    resp <- httptest2::gsub_response (
        resp,
        ptn,
        "2022-01-01",
        fixed = FALSE
    )

    return (resp)
}
