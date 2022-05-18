
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

source ("../extra-fns.R")

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
    expect_s3_class (dep, "depositsClient")

    # --------- UPLOAD_DATA
    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    dep <- with_mock_dir ("fs_up", {
        cli$upload_file (deposit_id, filename)
    })

    expect_type (dep, "list")
    expect_true (length (dep$files) > 0L)
    expect_identical (
        dep$files$supplied_md5,
        dep$files$computed_md5
    )

    # -------- DELETE_DEPOSIT
    # can't mock that because it returns an empty body
    # dep <- with_mock_dir ("fs_del", {
    #    cli$delete_deposit (deposit_id)
    # })
    # expect_true (dep)
})
