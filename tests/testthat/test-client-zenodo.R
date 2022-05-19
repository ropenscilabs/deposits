
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

test_that ("zenodo actions", {

    service <- "zenodo"

    # expect_silent (
    cli <- depositsClient$new (name = service, sandbox = TRUE)
    # )
    expect_s3_class (cli, "depositsClient")
    expect_identical (cli$name, service)

    # --------- PING
    x <- with_mock_dir ("zen_ping", {
        cli$deposit_authenticate ()
    })

    # --------- NEW_DEPOSIT
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )
    cli <- depositsClient$new (name = service, sandbox = TRUE, metadata = metadata)
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli$metadata, "DCEntry")

    dep <- with_mock_dir ("zen_new", {
        cli$deposit_new ()
    })

    expect_type (dep, "list")
    expect_identical (
        names (dep),
        c (
            "conceptrecid", "created", "doi",
            "doi_url", "files", "id",
            "links", "metadata", "modified",
            "owner", "record_id", "state",
            "submitted", "title"
        )
    )
    dep_new <- dep

    # -------- RETRIEVE_DEPOSIT
    deposit_id <- dep$id
    dep <- with_mock_dir ("zen_retr", {
        cli$deposit_retrieve (deposit_id)
    })
    expect_s3_class (dep, "depositsClient")

    # --------- UPLOAD_DATA
    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    dep <- with_mock_dir ("zen_up", {
        cli$deposit_upload_file (deposit_id, filename)
    })

    expect_type (dep, "list")
    expect_identical (
        gsub ("^md5\\:", "", dep$checksum),
        unname (tools::md5sum (filename))
    )

    # -------- LIST_DEPOSITS
    deps <- with_mock_dir ("zen_list", {
        cli$deposits_list ()
    })

    expect_s3_class (deps, "data.frame")
    expect_equal (nrow (deps), 1L)

    # -------- DELETE_DEPOSIT
    # can't mock that because it returns an empty body
    # dep <- with_mock_dir ("zen_del", {
    #    cli$deposit_delete (deposit_id)
    # })
    # expect_true (dep)
})
