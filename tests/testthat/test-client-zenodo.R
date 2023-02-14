test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

# This envvar is used to convert the contents of the uploaded json file to a
# standardised form (uniform timestamps and article id values).
# This is also used one time in metadata.R `construct_metadata_list()` fn to set
# the "created" date for zenodo deposits.
Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

test_that ("zenodo new", {

    service <- "zenodo"

    cli <- with_mock_dir ("zen_create", {
        depositsClient$new (service = service, sandbox = TRUE)
    })
    expect_s3_class (cli, "depositsClient")
    expect_identical (cli$service, service)

    # --------- DEPOSIT_NEW
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )

    cli <- with_mock_dir ("zen_client", {
        depositsClient$new (
            service = service,
            sandbox = TRUE,
            metadata = metadata
        )
    })
    expect_s3_class (cli, "depositsClient")
    expect_type (cli$metadata, "list")
    expect_length (cli$metadata, 4L)
    expect_equal (
        names (cli$metadata),
        c ("abstract", "created", "creator", "title")
    )
    expect_type (cli$metadata, "list")
    # expect_type (cli$metadata_service, "list") # now a private field
    expect_null (cli$hostdata)

    dep <- with_mock_dir ("zen_new", {
        cli$deposit_new ()
    })

    expect_s3_class (dep, "depositsClient")
    expect_identical (dep, cli)
    expect_false (is.null (cli$hostdata))
    expect_type (cli$hostdata, "list")
    expect_true (length (cli$hostdata) > 1L)
})

# Mock client from previous tests, recreated below to test further
# functionality:
new_mock_zen_deposit <- function () {

    service <- "zenodo"
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )

    # Mock client from previous tests:
    cli <- with_mock_dir ("zen_client", {
        depositsClient$new (
            service = service,
            sandbox = TRUE,
            metadata = metadata
        )
    })
    cli <- with_mock_dir ("zen_new", {
        cli$deposit_new ()
    })

    return (cli)
}

test_that ("zenodo retrieve", {

    cli <- new_mock_zen_deposit ()
    # metadata used in `new_mock_zen_deposit` fn, but needed below to compare in
    # tests.
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )

    # -------- DEPOSIT_RETRIEVE
    deposit_id <- cli$id
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
        cli$metadata$title,
        metadata$title
    )

    metadata <- list (
        title = "Modified Title",
        abstract = "This is the modified abstract",
        creator = "C. Person"
    )

    dep <- with_mock_dir ("zen_meta", {
        cli$deposit_fill_metadata (metadata)
    })

    expect_equal (
        cli$metadata$title,
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
})

test_that ("zenodo deposits_list", {

    cli <- new_mock_zen_deposit ()

    dep <- with_mock_dir ("zen_list", {
        cli$deposits_list ()
    })

    expect_s3_class (dep, "depositsClient")
    expect_identical (dep, cli)
})


test_that ("zenodo upload", {

    cli <- new_mock_zen_deposit ()
    deposit_id <- cli$id

    # --------- UPLOAD_DATA
    # filename <- file.path (tempdir (), "data.Rds")
    # saveRDS (datasets::Orange, filename)
    filename <- fs::path (fs::path_temp (), "data.csv")
    write.csv (datasets::Orange, filename)

    dep <- with_mock_dir ("zen_up", {
        cli$deposit_upload_file (path = filename) # deposit_id from cli$id
    })

    expect_identical (dep, cli)
    # This should have two files, but zenodo requires downloading which can't be
    # mocked, and that prevents frictionless uploading. See
    # https://github.com/ropenscilabs/deposits/blob/3c8dc71809fe17f68fc0fbd83f730ae8a1c1a646/R/client-private-frictionless.R#L201-L202
    expect_true (nrow (cli$hostdata$files) > 0L)
    expect_identical (
        gsub ("^md5\\:", "", cli$hostdata$files$checksum [1]),
        unname (tools::md5sum (filename))
    )

})

test_that ("zenodo download", {

    cli <- new_mock_zen_deposit ()
    deposit_id <- cli$id
    filename <- fs::path (fs::path_temp (), "data.csv")

    path <- fs::path_temp ()
    if (fs::file_exists (filename)) { # from upload data above
        fs::file_delete (filename)
    }

    path <- with_mock_dir ("zen_dl", {
        cli$deposit_download_file (
            # deposit_id = deposit_id, # grabbed from cli$id
            filename = fs::path_file (filename),
            path = fs::path_temp ()
        )
    })
    expect_identical (filename, path)
    # The mock tests do not actually create the file, so can't test it here:
    # expect_true (file.exists (path))
    # expect_identical (datasets::Orange, readRDS (path))

    expect_error (
        with_mock_dir ("zen_dl_fail", {
            cli$deposit_download_file (
                deposit_id = deposit_id,
                filename = "does_not_exist.dat",
                path = tempdir ()
            )
        }),
        "That deposit does not contain the specified file."
    )
})

# can't mock delete because it returns an empty body
test_that ("zenodo delete", {
    # dep <- with_mock_dir ("zen_del", {
    #    cli$deposit_delete (deposit_id)
    # })
    # expect_true (dep)
})

Sys.unsetenv ("DEPOSITS_TEST_ENV")
