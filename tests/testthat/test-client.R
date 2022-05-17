
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

source ("../extra-fns.R")

test_that("Client structure", {

    service <- "figshare"
    tok <- tryCatch (get_deposits_token (service),
                     error = function (e) NULL)

    skip_if (is.null (tok))

    expect_error (
        cli <- depositsClient$new (),
        "argument \"name\" is missing, with no default")

    expect_error (
        cli <- depositsClient$new ("junk"),
        "'arg' should be one of")

    expect_silent (
        cli <- depositsClient$new (name = service)
        )
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli, "R6")
    expect_identical (cli$name, service)

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
    expect_identical (out [1],
                      "<deposits client>")
    expect_true (any (grepl ("^\\s+name\\:", out)))
    expect_true (any (grepl ("^\\s+url\\s\\:", out)))
})

testthat::skip_if (!test_all)

test_that ("figshare actions", {

    service <- "figshare"

    expect_silent (
        cli <- depositsClient$new (name = service)
        )

    # --------- PING
    x <- with_mock_dir ("fs_ping", {
        cli$ping ()
    })

    # --------- NEW_DEPOSIT
    filename <- fill_meta ()
    cli <- depositsClient$new (name = service, metadata = filename)
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli$metadata, "DCEntry")

    dep <- with_mock_dir ("fs_new", {
        cli$new_deposit ()
    })

    expect_type (dep, "list")
    expect_identical (names (dep), c ("entity_id", "location", "warnings"))
    expect_length (dep$warnings, 0L)

    # -------- RETRIEVE_DEPOSIT
    deposit_id <- dep$entity_id
    dep <- with_mock_dir ("fs_retr", {
        cli$retrieve_deposit (deposit_id)
    })
    expect_type (dep, "list")
    expect_length (dep$files, 0L)
    expect_identical (dep$id, deposit_id)
    expect_false (dep$is_public)

    # --------- UPLOAD_DATA
    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    dep <- with_mock_dir ("fs_up", {
        cli$upload_file (deposit_id, filename)
    })

    expect_type (dep, "list")
    expect_true (length (dep$files) > 0L)
    expect_identical (dep$files$supplied_md5,
                      dep$files$computed_md5)

    # -------- DELETE_DEPOSIT
    # can't mock that because it returns an empty body
    #dep <- with_mock_dir ("fs_del", {
    #    cli$delete_deposit (deposit_id)
    #})
    #expect_true (dep)
})
