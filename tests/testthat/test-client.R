
test_that ("Client structure", {

    service <- "figshare"
    tok <- tryCatch (get_deposits_token (service),
        error = function (e) NULL
    )

    skip_if (is.null (tok))

    expect_error (
        cli <- depositsClient$new (),
        "argument \"service\" is missing, with no default"
    )

    expect_error (
        cli <- depositsClient$new ("junk"),
        "'arg' should be one of"
    )

    expect_silent (
        cli <- depositsClient$new (service = service)
    )
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli, "R6")
    expect_identical (cli$service, service)

    expect_true ("id" %in% names (cli))
    expect_null (cli$id)
    expect_true ("url_deposit" %in% names (cli))
    expect_null (cli$url_deposit)
    expect_true ("url_base" %in% names (cli))
    expect_false (is.null (cli$url_base))

    expect_error (
        cli$deposit_new (),
        "No metadata present; use 'fill_metadata\\(\\)' first."
    )

    s <- deposits_services ()
    u <- s$api_base_url [s$name == service]
    expect_identical (cli$url_base, u)
})

test_that ("print", {

    service <- "figshare"

    expect_silent (
        cli <- depositsClient$new (service = service)
    )

    out <- capture.output (print (cli))
    expect_identical (
        out [1],
        "<deposits client>"
    )
    expect_true (any (grepl ("^\\s+deposits\\sservice\\s\\:", out)))
    expect_true (any (grepl ("^\\s+url_base\\s\\:", out)))
    expect_true (any (grepl ("^\\s+Current\\sdeposits\\s\\:", out)))
    expect_true (any (grepl ("^\\s+hostdata\\s\\:", out)))
    expect_true (any (grepl ("^\\s+metadata\\s\\:", out)))
})
