
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

# Request mocking requires setting dates in some requests to constant values,
# for which this envvar is used.
Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

test_that ("zenodo actions", {

    service <- "zenodo"

    cli <- with_mock_dir ("zen_create", {
        depositsClient$new (service = service, sandbox = TRUE)
    })
    expect_s3_class (cli, "depositsClient")
    expect_identical (cli$service, service)

    # --------- AUTHENTICATE
    x <- with_mock_dir ("zen_ping", {
        cli$deposit_authenticate ()
    })

    # --------- DEPOSIT_NEW
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )
    cli <- depositsClient$new (service = service, sandbox = TRUE, metadata = metadata)
    expect_s3_class (cli, "depositsClient")
    expect_s3_class (cli$metadata, "DCEntry")
    expect_null (cli$hostdata)

    dep <- with_mock_dir ("zen_new", {
        cli$deposit_new ()
    })

    expect_s3_class (dep, "depositsClient")
    expect_identical (dep, cli)
    expect_false (is.null (cli$hostdata))
    expect_type (cli$hostdata, "list")
    expect_true (length (cli$hostdata) > 1L)

    # -------- DEPOSIT_RETRIEVE
    deposit_id <- cli$hostdata$id
    dep <- with_mock_dir ("zen_retr", {
        cli$deposit_retrieve (deposit_id)
    })
    expect_s3_class (dep, "depositsClient")

    # -------- DEPOSIT_UPDATE
    expect_equal (
        cli$hostdata$title,
        metadata$title
    )
    expect_equal (
        cli$hostdata$metadata$description,
        metadata$abstract
    )
    expect_equal (
        cli$metadata$title [[1]]$value,
        metadata$title
    )

    metadata <- list (
        title = "Modified Title",
        abstract = "This is the modified abstract",
        creator = "C. Person"
    )
    cli$deposit_fill_metadata (metadata)
    expect_equal (
        cli$metadata$title [[1]]$value,
        metadata$title
    )
    expect_false (cli$hostdata$title ==
        metadata$title)
    expect_false (cli$hostdata$metadata$description ==
        metadata$abstract)

    dep <- with_mock_dir ("zen_update", {
        cli$deposit_update ()
    })

    expect_equal (
        cli$hostdata$title,
        metadata$title
    )
    expect_equal (
        cli$hostdata$metadata$description,
        metadata$abstract
    )

    # --------- UPLOAD_DATA
    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    dep <- with_mock_dir ("zen_up", {
        cli$deposit_upload_file (deposit_id, filename)
    })

    expect_identical (dep, cli)
    expect_true (length (cli$hostdata$files) > 0L)
    expect_identical (
        gsub ("^md5\\:", "", dep$hostdata$files$checksum [1]),
        unname (tools::md5sum (filename))
    )

    # -------- DEPOSITS_LIST
    dep <- with_mock_dir ("zen_list", {
        cli$deposits_list ()
    })

    expect_s3_class (dep, "depositsClient")
    expect_identical (dep, cli)

    # -------- DEPOSIT_DOWNLOAD
    path <- tempdir ()
    ftmp <- file.path (path, "data.Rds")
    if (file.exists (ftmp)) {
        file.remove (ftmp)
    }

    path <- with_mock_dir ("zen_dl", {
        cli$deposit_download_file (
            deposit_id = deposit_id,
            filename = basename (filename),
            path = tempdir ()
        )
    })
    expect_true (file.exists (path))
    expect_identical (datasets::Orange, readRDS (path))

    # -------- DEPOSIT_DELETE
    # can't mock that because it returns an empty body
    # dep <- with_mock_dir ("zen_del", {
    #    cli$deposit_delete (deposit_id)
    # })
    # expect_true (dep)
})

Sys.unsetenv ("DEPOSITS_TEST_ENV")
