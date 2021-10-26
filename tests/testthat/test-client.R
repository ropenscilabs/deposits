
test_that("Client", {

    service <- "figshare"
    tok <- tryCatch (get_deposits_token (service),
                     error = function (e) NULL)

    skip_if (is.null (tok))

    expect_error (
        cli <- depositsClient$new (),
        "'name' may not be missing")

    expect_silent (
        cli <- depositsClient$new (name = service)
        )
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli, "R6")
    expect_identical (cli$name, service)

    s <- deposits_services ()
    u <- s$api_base_url [s$name == service]
    u <- paste0 (u, "token") # for figshare only!
    expect_identical (cli$url, u)
})
