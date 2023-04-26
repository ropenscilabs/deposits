test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

# This envvar is used to convert the contents of the uploaded json file to a
# standardised form (uniform timestamps and article id values).
Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

test_that ("services", {

    expect_silent (
        s <- deposits_services ()
    )
    expect_s3_class (s, "data.frame")
    expect_equal (ncol (s), 3L)
    expect_equal (nrow (s), 3L)
    expect_identical (names (s), c ("name", "docs", "api_base_url"))
    expect_identical (s$name, c ("zenodo", "zenodo-sandbox", "figshare"))
})

test_that ("tokens", {

    expect_error (
        get_deposits_token (),
        "Assertion on 'service' failed"
    )
    expect_error (
        get_deposits_token ("aaaaa"),
        "No token found for \\[aaaaa\\]"
    )

    testthat::skip_if (!test_all)

    tok <- get_deposits_token ("figshare")
    expect_type (tok, "character")
    expect_true (nchar (tok) > 1L)

    Sys.setenv ("junkone" = "123456")
    Sys.setenv ("junktwo" = "123456")
    expect_silent (
        tok <- get_deposits_token ("junk")
    )
    expect_equal (tok, "123456")

    Sys.setenv ("junktwo" = "234567")
    expect_error (
        tok <- get_deposits_token ("junk"),
        "No unambiguous token found for \\[junk\\] service."
    )
})

testthat::skip_if (!test_all)

test_that ("deposit_service function", {

    cli <- with_mock_dir ("services_zen", {
        depositsClient$new (service = "zenodo", sandbox = TRUE)
    })
    expect_equal (cli$service, "zenodo")
    expect_true (cli$sandbox)
    expect_true (grepl ("zenodo", cli$url_base))

    cli <- with_mock_dir ("services_fs", {
        cli$deposit_service (service = "figshare")
    })
    expect_equal (cli$service, "figshare")
    expect_false (cli$sandbox)
    expect_true (grepl ("figshare", cli$url_base))
})
