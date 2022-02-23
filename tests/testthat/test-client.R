
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

test_that("Client", {

    service <- "figshare"
    tok <- tryCatch (get_deposits_token (service),
                     error = function (e) NULL)

    skip_if (is.null (tok))

    expect_error (
        cli <- depositsClient$new (),
        "'name' may not be missing")

    expect_error (
        cli <- depositsClient$new ("junk"),
        "'name' must be one of \\[zenodo, figshare\\]")

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

test_that ("print", {

    service <- "figshare"

    expect_silent (
        cli <- depositsClient$new (name = service)
        )

    out <- capture.output (print (cli))
    expect_identical (out [1],
                      "<deposits client>")
    expect_true (any (grepl ("^\\s+name\\:", out)))
    expect_true (any (grepl ("^\\s+url\\s\\:", out)))
})

testthat::skip_if (!test_all)

test_that ("ping", {

    service <- "figshare"

    expect_silent (
        cli <- depositsClient$new (name = service)
        )
    expect_true (cli$ping ())
})
