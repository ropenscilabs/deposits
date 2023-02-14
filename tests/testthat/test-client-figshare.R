test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

# This envvar is used to convert the contents of the uploaded json file to a
# standardised form (uniform timestamps and article id values).
Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

test_that ("figshare new", {

    service <- "figshare"

    cli <- with_mock_dir ("fs_create", {
        depositsClient$new (service = service)
    })
    expect_s3_class (cli, "depositsClient")
    expect_identical (cli$service, service)

    # --------- DEPOSIT_NEW
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )

    cli <- with_mock_dir ("fs_client", {
        depositsClient$new (
            service = service,
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
    # "created" timestamp is inserted:
    expect_true (length (cli$metadata) > length (metadata))
    expect_null (cli$hostdata)

    dep <- with_mock_dir ("fs_new", {
        cli$deposit_new ()
    })

    expect_s3_class (dep, "depositsClient")
    expect_identical (dep, cli)
    expect_false (is.null (cli$hostdata))
    expect_type (cli$hostdata, "list")
    expect_true (length (cli$hostdata) > 1L)
})

new_mock_fs_deposit <- function () {

    service <- "figshare"

    cli <- with_mock_dir ("fs_create", {
        depositsClient$new (service = service)
    })
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )
    cli <- with_mock_dir ("fs_client", {
        depositsClient$new (
            service = service,
            metadata = metadata
        )
    })
    cli <- with_mock_dir ("fs_new", {
        cli$deposit_new ()
    })

    return (cli)
}

test_that ("figshare retrieve", {

    service <- "figshare"
    cli <- new_mock_fs_deposit ()
    deposit_id <- cli$id
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )

    dep <- with_mock_dir ("fs_retr", {
        cli$deposit_retrieve (deposit_id)
    })
    expect_s3_class (dep, "depositsClient")

    expect_equal (
        cli$hostdata$title,
        metadata$title
    )
    # expect_equal (
    #     cli$hostdata$description,
    #     metadata$abstract
    # )
    expect_equal (
        cli$metadata$title,
        metadata$title
    )

})

test_that ("figshare update", {

    service <- "figshare"
    cli <- new_mock_fs_deposit ()
    deposit_id <- cli$id

    metadata <- list (
        title = "Modified Title",
        abstract = "This is the modified abstract",
        creator = "C. Person"
    )

    cli <- cli$deposit_fill_metadata (metadata)

    expect_equal (
        cli$metadata$title [[1]],
        metadata$title
    )
    expect_false (cli$hostdata$title ==
        metadata$title)
    expect_false (cli$hostdata$description ==
        metadata$abstract)

    dep <- with_mock_dir ("fs_update", {
        cli$deposit_update ()
    })

    expect_equal (
        cli$hostdata$title,
        metadata$title
    )
    # expect_equal (
    #     cli$hostdata$description,
    #     metadata$abstract
    # )
})


test_that ("figshare upload", {

    service <- "figshare"
    cli <- new_mock_fs_deposit ()
    deposit_id <- cli$id

    filename <- fs::path (fs::path_temp (), "data.csv")
    write.csv (datasets::Orange, filename)

    dep <- with_mock_dir ("fs_up", {
        cli$deposit_upload_file (deposit_id, filename)
    })

    expect_identical (dep, cli)
    expect_true (length (cli$hostdata$files) > 0L)
    expect_identical (
        dep$files$supplied_md5,
        dep$files$computed_md5
    )
    n_files <- nrow (cli$hostdata$files)

    # --------- UPLOAD ADDITIONAL DATA
    # Initial uploads differ to subsequent uploads; this tests the latter
    filename <- fs::path (fs::path_temp (), "data2.csv")
    write.csv (datasets::Orange, filename)
    cli <- with_mock_dir ("fs_up2", {
        cli$deposit_upload_file (path = filename) # deposit_id from cli$id
    })
    expect_true (nrow (cli$hostdata$files) > n_files)
    expect_true (all (c ("data.csv", "data2.csv") %in% cli$hostdata$files$name))
})

test_that ("figshare upload binary", {

    service <- "figshare"
    cli <- new_mock_fs_deposit ()
    deposit_id <- cli$id

    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    dep <- with_mock_dir ("fs_up_bin", {
        cli$deposit_upload_file (deposit_id, filename)
    })

    expect_identical (dep, cli)
    expect_true (length (cli$hostdata$files) > 0L)
    expect_identical (
        dep$hostdata$files$supplied_md5,
        dep$hostdata$files$computed_md5
    )
})

test_that ("figshare list", {

    cli <- new_mock_fs_deposit ()

    dep <- with_mock_dir ("fs_list", {
        cli$deposits_list ()
    })

    expect_s3_class (dep, "depositsClient")
    expect_identical (dep, cli)
})


test_that ("figshare download", {

    # can't test because figshare doesn't allow private download
    # ---> tested in zenodo.
})

test_that ("figshare delete", {

    # can't mock that because it returns an empty body
    # dep <- with_mock_dir ("fs_del", {
    #     cli$deposit_delete (deposit_id)
    # })
    # expect_true (dep)
})


# This is mostly tested in 'test-metadata.R'. This just tests the removal of
# unrecognised metadata terms on initial client construction.
test_that ("figshare metadata", {

    # --------- DEPOSIT_NEW
    metadata <- list (
        Title = "Iris Dataset",
        abstract = "This is the abstract",
        Creator = list ("Edgar Anderson"),
        Publisher = "American Iris Society",
        Source = "https://doi.org/10.1111/j.1469-1809.1936.tb02137.x",
        Language = "eng"
    )

    cli <- with_mock_dir ("fs_meta_unrecognised", {
        depositsClient$new (
            service = "figshare",
            metadata = metadata
        )
    })

    # expect_null (cli$metadata$language)
    # expect_null (cli$metadata$publisher)
    expect_true (length (cli$metadata$source) > 0L)
    expect_true (length (cli$metadata$title) > 0L)
    expect_true (length (cli$metadata$abstract) > 0L)
    expect_true (length (cli$metadata$creator) > 0L)
})
