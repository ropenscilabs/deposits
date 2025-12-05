test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

# This envvar is used to convert the contents of the uploaded json file to a
# standardised form (uniform timestamps and article id values).
Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

test_that ("figshare new", {

    service <- "figshare"

    cli <- httptest2::with_mock_dir ("fs_create", {
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
        subject = list (
            categories = list (24418L),
            keywords = as.list (c ("beaver", "temperature"))
        ),
        format = "dataset"
    )

    cli <- httptest2::with_mock_dir ("fs_client", {
        depositsClient$new (
            service = service,
            metadata = metadata
        )
    })

    expect_s3_class (cli, "depositsClient")
    expect_type (cli$metadata, "list")
    expect_length (cli$metadata, 6L)
    expect_equal (
        names (cli$metadata),
        c ("abstract", "creator", "description", "format", "subject", "title")
    )
    expect_true (length (cli$metadata) == length (metadata))
    expect_null (cli$hostdata)

    dep <- httptest2::with_mock_dir ("fs_new2", {
        cli$deposit_new (prereserve_doi = TRUE)
    })

    expect_s3_class (dep, "depositsClient")
    expect_identical (dep, cli)
    expect_false (is.null (cli$hostdata))
    expect_type (cli$hostdata, "list")
    expect_true (length (cli$hostdata) > 1L)

    # Should also have prereserved DOI in both meta and hostdata, but does not
    # pre-reserve in mock tests for some reason.
    # expect_true (nzchar (cli$hostdata$doi))
    expect_true (length (cli$metadata) == length (metadata))
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
        ),
        subject = list (
            categories = list (24418L)
        ),
        format = "dataset"
    )

    expect_error (
        metadata <- validate_metadata (metadata, service),
        paste0 (
            "Metadata source for \\[keywords\\] should be ",
            "\\[subject\\] and not \\[description\\]"
        )
    )
    metadata$description <- "This is the description\n\n## version\n1.0"
    metadata$subject$keywords <- as.list (c ("beaver", "temperature"))

    expect_silent (
        metadata <- validate_metadata (metadata, service)
    )

    # Expect DCMI metadata to remain the same:
    # Expect NO markdown header inserted:
    expect_false (grepl ("^<h2>description", metadata$dcmi$description))
    desc <- strsplit (metadata$dcmi$description, "\n") [[1]]
    # Actual description remains as first item:
    expect_equal ("This is the description", desc [1])

    # Expect service metadata to have markdown header inserted:
    desc <- metadata$service$description
    desc <- gsub ("\\\\n", "\n", desc)
    expect_true (grepl ("<h2>description", desc))
    desc <- strsplit (desc, "\n") [[1]]
    # Expect abstract is now first:
    expect_true (grepl ("^<h2>abstract", desc [1]))
    # Expect markdown description title has been inserted:
    expect_true (any (grepl ("<h2>description", desc)))
    pos_title <- grep ("<h2>description", desc)
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
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description = paste0 (
            "## description\nThis is the description\n\n",
            "## version\n1.0"
        ),
        subject = list (
            categories = list (24418L),
            keywords = as.list (c ("beaver", "temperature"))
        ),
        format = "dataset"
    )

    dep <- httptest2::with_mock_dir ("fs_retr", {
        cli$deposit_retrieve (deposit_id)
    })
    expect_s3_class (dep, "depositsClient")
    expect_true (length (dep$hostdata) > 0L)
    # metadata is filled on retreive (#65):
    expect_true (length (dep$metadata) > 0L)
    expect_identical (
        metadata [order (names (metadata))],
        dep$metadata [order (names (dep$metadata))]
    )

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
        subject = list (
            categories = list (24418L),
            keywords = as.list (c ("beaver", "temperature"))
        ),
        format = "dataset"
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

    dep <- httptest2::with_mock_dir ("fs_update", {
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

test_that ("figshare add_resource", {

    service <- "figshare"

    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description = paste0 (
            "## description\nThis is the description\n\n",
            "## version\n1.0"
        ),
        subject = list (
            categories = list (24418L),
            keywords = as.list (c ("beaver", "temperature"))
        ),
        format = "dataset"
    )

    cli <- httptest2::with_mock_dir ("fs_client", {
        depositsClient$new (
            service = service,
            metadata = metadata
        )
    })

    path <- fs::path (fs::path_temp (), "data")
    if (fs::dir_exists (path)) {
        fs::dir_delete (path)
    }
    fs::dir_create (path)

    expect_error (
        cli$deposit_add_resource (path),
        "'path' must contain at least one resource"
    )

    filename <- fs::path (path, "data.csv")
    write.csv (datasets::Orange, filename, row.names = FALSE)

    dp <- fs::path (path, "datapackage.json")
    if (fs::file_exists (dp)) {
        fs::file_delete (dp)
    }

    requireNamespace ("frictionless")
    suppressWarnings (
        cli$deposit_add_resource (filename)
    )
    files <- fs::path_file (fs::dir_ls (path))
    expect_true ("datapackage.json" %in% files)

    cli <- httptest2::with_mock_dir ("fs_create", {
        depositsClient$new (service = service)
    })
    expect_null (cli$metadata)
    expect_null (cli$hostdata)
    expect_message (
        cli$deposit_add_resource (path),
        paste0 (
            "Your \\'datapackage\\.json\\' includes the following ",
            "resources which have not yet been uploaded"
        )
    )
    expect_null (cli$hostdata)
    expect_false (is.null (cli$metadata))
    expect_type (cli$metadata, "list")
    expect_length (cli$metadata, 6L)

    expect_identical (
        cli$metadata [order (names (cli$metadata))],
        metadata [order (names (metadata))]
    )
})

test_that ("figshare upload", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    filename <- fs::path (fs::path_temp (), "data.csv")
    write.csv (datasets::Orange, filename)

    suppressWarnings (
        dep <- httptest2::with_mock_dir ("fs_up1", {
            cli$deposit_upload_file (filename, deposit_id)
        })
    )

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
    cli <- httptest2::with_mock_dir ("fs_up2", {
        cli$deposit_upload_file (path = filename) # deposit_id from cli$id
    })
    expect_true (nrow (cli$hostdata$files) >= n_files)
    expect_true (all (c ("data.csv", "data2.csv") %in% cli$hostdata$files$name))
})

test_that ("figshare update datapackage", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    path <- fs::path (fs::path_temp (), "data")
    fs::dir_create (path)
    filename <- fs::path (path, "data.csv")
    write.csv (datasets::Orange, filename)

    cli <- httptest2::with_mock_dir ("fs_up3", {
        cli$deposit_upload_file (filename, deposit_id)
    })

    # Modify local metadata:
    cli$metadata$title <- "Modified Title"
    # This should generate a warning that metadata differs, but error comes
    # first. Warning can't be generated in test env because of reasons explained
    # below.
    expect_error (
        httptest2::with_mock_dir ("fs_update_dp1", {
            cli$deposit_update (path = path)
        }),
        "Local file \\[datapackage\\.json\\] does not exist on remote"
    )

    # Modify local "datapackage.json":
    f <- fs::path (path, "datapackage.json")
    x <- readLines (f)
    i <- grep ("the\\sdescription", x)
    x [i] <- gsub ("the\\sdescription", "The modified description", x [i])
    i <- grep ("New\\sTitle", x)
    x [i] <- gsub ("New\\sTitle", "New Modified Title", x [i])
    writeLines (x, f)

    # ----- NOTE -----
    # This can't be tested at present, because the "datapackage.json" files can
    # not be uploaded in test environments, as explained in comment in private
    # "update_frictionless" method. That means that attempting to update
    # triggers an error that file does not exist on remote deposit.

    # cli <- httptest2::with_mock_dir ("zen_update_dp", {
    #     cli$deposit_update (path = path)
    # })
    expect_error (
        httptest2::with_mock_dir ("fs_update_dp2", {
            cli$deposit_update (path = path)
        }),
        "Local file \\[datapackage\\.json\\] does not exist on remote"
    )
})

test_that ("figshare upload binary", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    # dep <- httptest2::with_mock_dir ("fs_up_bin", {
    #     cli$deposit_upload_file (deposit_id, filename)
    # })

    # expect_identical (dep, cli)
    # expect_true (length (cli$hostdata$files) > 0L)
    # expect_identical (
    #     dep$hostdata$files$supplied_md5,
    #     dep$hostdata$files$computed_md5
    # )
})

test_that ("figshare version", {

    service <- "figshare"

    cli <- httptest2::with_mock_dir ("fs_create", {
        depositsClient$new (service = service)
    })
    cli <- httptest2::with_mock_dir ("fs_get_publ", {
        cli$deposit_retrieve (cli$deposits$id [1])
    })

    expect_error (
        cli$deposit_version (),
        "This method is not applicable for Figshare"
    )
})

test_that ("figshare embargo", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    nms <- c (
        "is_embargoed", "embargo_date", "embargo_type",
        "embargo_title", "embargo_reason", "embargo_options"
    )
    expect_true (all (nms %in% names (cli$hostdata)))

    expect_false (cli$hostdata$is_embargoed)
    expect_null (cli$hostdata$embargo_date)
    expect_null (cli$hostdata$embargo_type)
    expect_true (!nzchar (cli$hostdata$embargo_title))
    expect_true (!nzchar (cli$hostdata$embargo_reason))
    expect_type (cli$hostdata$embargo_options, "list")
    expect_length (cli$hostdata$embargo_options, 0L)

    expect_error (
        cli$deposit_embargo (embargo_date = 1),
        "Assertion on 'embargo_date' failed: Must be of type 'character'"
    )

    # None of this can actually be tested because Figshare's embargo method
    # returns an empty body, so httptest2 has nothing to mock.
    # embargo_date <- "2040-01-01"
    # cli <- httptest2::with_mock_dir ("fs_embargo", {
    #    cli$deposit_embargo (
    #        embargo_date = embargo_date,
    #        embargo_reason = "because"
    #    )
    # })

    # expect_true (all (nms %in% names (cli$hostdata)))
    # expect_true (cli$hostdata$is_embargoed)
    # expect_false (is.null (cli$hostdata$embargo_date))
    # expect_equal (cli$hostdata$embargo_date, embargo_date)
    # expect_false (is.null (cli$hostdata$embargo_type))
    # expect_equal (cli$hostdata$embargo_type, "article")
    # expect_true (nzchar (cli$hostdata$embargo_title))
    # expect_equal (cli$hostdata$embargo_title, "article under embargo")
    # expect_true (nzchar (cli$hostdata$embargo_reason))
    # expect_equal (cli$hostdata$embargo_reason, "because")
    # expect_type (cli$hostdata$embargo_options, "list")
    # expect_length (cli$hostdata$embargo_options, 0L)
})

test_that ("figshare list", {

    service <- "figshare"
    cli <- new_mock_deposit (service = service)

    dep <- httptest2::with_mock_dir ("fs_list", {
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
    # dep <- httptest2::with_mock_dir ("fs_del", {
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

    expect_error (
        depositsClient$new (service = "figshare", metadata = metadata),
        "Stopping because the DCMI metadata terms listed above do not conform"
    )
})
