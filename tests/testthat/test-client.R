
test_that ("Client structure", {

    service <- "figshare"
    tok <- tryCatch (get_deposits_token (service),
        error = function (e) NULL
    )

    skip_if (is.null (tok))

    expect_error (
        cli <- depositsClient$new (),
        "argument \"name\" is missing, with no default"
    )

    expect_error (
        cli <- depositsClient$new ("junk"),
        "'arg' should be one of"
    )

    expect_silent (
        cli <- depositsClient$new (name = service)
    )
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli, "R6")
    expect_identical (cli$name, service)

    expect_error (
        cli$new_deposit (),
        "No metadata present; use 'fill_metadata\\(\\)' first."
    )

    s <- deposits_services ()
    u <- s$api_base_url [s$name == service]
    expect_identical (cli$url, u)
})

test_that ("print", {

    service <- "figshare"

    expect_silent (
        cli <- depositsClient$new (name = service)
    )

    out <- capture.output (print (cli))
    expect_identical (
        out [1],
        "<deposits client>"
    )
    expect_true (any (grepl ("^\\s+name\\:", out)))
    expect_true (any (grepl ("^\\s+url\\s\\:", out)))
})
