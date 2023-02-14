test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

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
        cli <- with_mock_dir ("client-new", {
            depositsClient$new (service = service)
        })
    )
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli, "R6")
    expect_identical (cli$service, service)

    expect_true ("id" %in% names (cli))
    expect_null (cli$id)
    expect_true ("url_service" %in% names (cli))
    expect_null (cli$url_service)
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

# the following test fails on windows machines on r-universe windows machines,
# so switched off from here.
testthat::skip_if (!test_all)

test_that ("print-figshare", {

    service <- "figshare"

    expect_silent (
        cli <- with_mock_dir ("print-fs", {
            depositsClient$new (service = service)
        })
    )

    testthat::expect_snapshot (print (cli))
    testthat::expect_snapshot (cli$deposits_methods ())

    cli$hostdata <- list (data = "data")
    cli$metadata <- list (dcmi = list (updated = "2022-01-01"))
    cli$id <- "1"
    cli$url_service <- "https://my.deposit"
    cli$deposits <- data.frame (n = 1:5)

    testthat::expect_snapshot (print (cli))
})

test_that ("print-zenodo", {

    service <- "zenodo"

    expect_silent (
        cli <- with_mock_dir ("print-zen", {
            depositsClient$new (service = service, sandbox = TRUE)
        })
    )

    testthat::expect_snapshot (print (cli))
    testthat::expect_snapshot (cli$deposits_methods ())

    cli$hostdata <- list (data = "data")
    cli$metadata <- list (dcmi = list (updated = "2022-01-01"))
    cli$id <- "1"
    cli$url_service <- "https://my.deposit"
    cli$deposits <- data.frame (n = 1:5)

    testthat::expect_snapshot (print (cli))
})
