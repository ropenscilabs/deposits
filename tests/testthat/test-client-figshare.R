
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

test_that ("figshare actions", {

    service <- "figshare"

    expect_silent (
        cli <- depositsClient$new (name = service)
    )

    # --------- PING
    x <- with_mock_dir ("fs_ping", {
        cli$deposit_authenticate ()
    })

    # --------- NEW_DEPOSIT
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )
    cli <- depositsClient$new (name = service, metadata = metadata)
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli$metadata, "DCEntry")

    dep <- with_mock_dir ("fs_new", {
        cli$deposit_new ()
    })

    expect_type (dep, "list")
    expect_identical (names (dep), c ("entity_id", "location", "warnings"))
    expect_length (dep$warnings, 0L)

    # -------- RETRIEVE_DEPOSIT
    deposit_id <- dep$entity_id
    dep <- with_mock_dir ("fs_retr", {
        cli$deposit_retrieve (deposit_id)
    })
    expect_s3_class (dep, "depositsClient")

    # --------- UPLOAD_DATA
    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    dep <- with_mock_dir ("fs_up", {
        cli$deposit_upload_file (deposit_id, filename)
    })

    expect_type (dep, "list")
    expect_true (length (dep$files) > 0L)
    expect_identical (
        dep$files$supplied_md5,
        dep$files$computed_md5
    )

    # -------- LIST_DEPOSITS
    dep <- with_mock_dir ("fs_list", {
        cli$deposits_list ()
    })

    expect_s3_class (dep, "data.frame")
    expect_equal (nrow (dep), 1L)

    # -------- DELETE_DEPOSIT
    # can't mock that because it returns an empty body
    # dep <- with_mock_dir ("fs_del", {
    #     cli$deposit_delete (deposit_id)
    # })
    # expect_true (dep)
})
