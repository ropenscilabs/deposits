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
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description = paste0 (
            "## description\nThis is the description\n\n",
            "## version\n1.0"
        ),
        subject = "## keywords\none, two\nthree\n\n"
    )

    cli <- with_mock_dir ("fs_client", {
        depositsClient$new (
            service = service,
            metadata = metadata
        )
    })

    expect_s3_class (cli, "depositsClient")
    expect_type (cli$metadata, "list")
    expect_length (cli$metadata, 5L)
    expect_equal (
        names (cli$metadata),
        c ("abstract", "creator", "description", "subject", "title")
    )
    expect_true (length (cli$metadata) == length (metadata))
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

test_that ("figshare default metadata", {

    service <- "figshare"

    # The first 'description' is not named, and should default to 'description':
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description = paste0 (
            "This is the description\n\n",
            "## keywords\none, two\nthree\n\n## version\n1.0"
        )
    )

    expect_error (
        metadata <- validate_metadata (metadata, service),
        paste0 (
            "Metadata source for \\[keywords\\] should be ",
            "\\[subject\\] and not \\[description\\]"
        )
    )
    metadata$description <- "This is the description\n\n## version\n1.0"
    metadata$subject <- "## keywords\none, two\nthree"

    expect_silent (
        metadata <- validate_metadata (metadata, service)
    )

    # Expect DCMI metadata to remain the same:
    # Expect NO markdown header inserted:
    expect_false (grepl ("^\\#\\#\\sdescription", metadata$dcmi$description))
    desc <- strsplit (metadata$dcmi$description, "\n") [[1]]
    # Actual description remains as first item:
    expect_equal ("This is the description", desc [1])

    # Expect service metadata to have markdown header inserted:
    desc <- metadata$service$description
    expect_true (grepl ("\\#\\#\\sdescription", desc))
    desc <- strsplit (desc, "\n") [[1]]
    # Expect abstract is now first:
    expect_true (grepl ("^\\#\\#\\sabstract", desc [1]))
    # Expect markdown description title has been inserted:
    expect_true (any (grepl ("\\#\\#\\sdescription", desc)))
    pos_title <- grep ("\\#\\#\\sdescription", desc)
    pos_txt <- grep ("This is the description", desc)
    expect_true ((pos_txt - pos_title) > 0)
    expect_true ((pos_txt - pos_title) <= 2)
})

test_that ("figshare retrieve", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person"),
        description = paste0 (
            "## description\nThis is the description\n\n",
            "## version\n1.0"
        ),
        subject = "## keywords\none, two\nthree\n\n"
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
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    metadata <- list (
        title = "Modified Title",
        abstract = "This is the modified abstract",
        creator = list (list (name = "C. Person")),
        description = paste0 (
            "## description\nThis is the description\n\n",
            "## version\n1.0"
        ),
        subject = "## keywords\none, two\nthree\n\n"
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
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    filename <- fs::path (fs::path_temp (), "data.csv")
    write.csv (datasets::Orange, filename)

    dep <- with_mock_dir ("fs_up", {
        cli$deposit_upload_file (filename, deposit_id)
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

test_that ("figshare update frictionless", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id
    path <- fs::path (fs::path_temp (), "data")
    fs::dir_create (path)
    filename <- fs::path (path, "data.csv")
    write.csv (datasets::Orange, filename)
    dp <- fs::path (path, "datapackage.json")
    if (fs::file_exists (dp)) {
        fs::file_delete (dp)
    }
    cli <- with_mock_dir ("fs_up", {
        cli$deposit_upload_file (path = filename) # deposit_id from cli$id
    })

    files_old <- cli$hostdata$files
    p_old <- frictionless::read_package (fs::path (path, "datapackage.json"))

    metadata <- cli$metadata
    metadata$title <- "Modified Title"
    metadata$abstract <- "This is the modified abstract"
    metadata$creator <- c (cli$metadata$creator, list (list (name = "C. Person")))
    cli <- cli$deposit_fill_metadata (metadata)

    cli$deposit_update_frictionless (path = path)
    # expect_identical (files_old, cli$hostdata$files)
    p_new <- frictionless::read_package (fs::path (path, "datapackage.json"))
    expect_false (identical (p_old, p_new))
    expect_identical (p_old$resources, p_new$resources)
    expect_false (identical (p_old$metadata, p_new$metadata))
    expect_identical (p_new$metadata$title, "Modified Title")
    expect_true ("C. Person" %in% unlist (p_new$metadata$creator))
    expect_false ("C. Person" %in% unlist (p_old$metadata$creator))
})

test_that ("figshare upload binary", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    # dep <- with_mock_dir ("fs_up_bin", {
    #     cli$deposit_upload_file (deposit_id, filename)
    # })

    # expect_identical (dep, cli)
    # expect_true (length (cli$hostdata$files) > 0L)
    # expect_identical (
    #     dep$hostdata$files$supplied_md5,
    #     dep$hostdata$files$computed_md5
    # )
})

test_that ("figshare list", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)

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
        Creator = list (list (name = "Edgar Anderson")),
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
