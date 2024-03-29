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
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description =
            "## description\nThis is the description\n\n## version\n1.0",
        subject = "## keywords\none, two\nthree",
        accessRights = "closed"
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
    expect_length (cli$metadata, 6L)
    expect_equal (
        names (cli$metadata),
        c ("abstract", "accessRights", "creator", "description", "subject", "title")
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

    # Should also have prereserved DOI in both meta and hostdata:
    expect_true (nzchar (cli$hostdata$metadata$prereserve_doi$doi))
    expect_true (length (cli$metadata) > length (metadata))
    expect_true (nzchar (cli$metadata$identifier))
    expect_equal (
        cli$hostdata$metadata$prereserve_doi$doi,
        cli$metadata$identifier
    )
})

test_that ("zenodo default metadata", {

    service <- "zenodo"

    # The first 'description' is not named, and should default to 'description':
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description =
            "## description\nThis is the description\n\n## version\n1.0",
        subject = "## keywords\none, two\nthree"
    )

    expect_silent (
        metadata <- validate_metadata (metadata, service)
    )

    # Expect DCMI metadata to remain the same:
    expect_true (grepl ("^\\#\\#\\sdescription", metadata$dcmi$description))
    desc <- strsplit (metadata$dcmi$description, "\n") [[1]]
    expect_equal ("## description", desc [1])

    # Expect service metadata to have markdown header inserted:
    desc <- metadata$service$metadata$description
    desc <- gsub ("\\\\n", "\n", desc)
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

    # Expect access_right = "open":
    expect_equal (metadata$service$metadata$access_right, "open")
})

test_that ("zenodo retrieve", {

    service <- "zenodo"
    cli <- new_mock_deposit (service = service)
    # metadata used in `new_mock_deposit` fn, but needed below to compare in
    # tests.
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description =
            "## description\nThis is the description\n\n## version\n1.0",
        subject = "## keywords\none, two\nthree",
        accessRights = "closed"
    )

    # -------- DEPOSIT_RETRIEVE
    deposit_id <- cli$id
    dep <- with_mock_dir ("zen_retr", {
        cli$deposit_retrieve (deposit_id)
    })
    expect_s3_class (dep, "depositsClient")
    expect_true (length (dep$hostdata) > 0L)
    # metadata is filled on retreive (#65):
    expect_true (length (dep$metadata) > 0L)
    # And it has an additional 'identifier' field with prereserved DOI:
    expect_true (length (dep$metadata) > length (metadata))
    expect_true ("identifier" %in% names (dep$metadata))

    # -------- DEPOSIT_UPDATE
    expect_equal (
        cli$hostdata$title,
        metadata$title
    )
    expect_equal (
        cli$metadata$title,
        metadata$title
    )

    metadata <- list (
        title = "Modified Title",
        abstract = "This is the modified abstract",
        creator = list (list (name = "C. Person")),
        description =
            "## description\nThis is the description\n\n## version\n1.0",
        subject = "## keywords\none, two\nthree",
        accessRights = "closed"
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
})

test_that ("zenodo embargo", {

    service <- "zenodo"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    expect_equal (cli$hostdata$metadata$access_right, "restricted")

    expect_error (
        cli$deposit_embargo (embargo_date = 1),
        "Assertion on 'embargo_date' failed: Must be of type 'character'"
    )

    embargo_date <- "2040-01-01"
    cli <- with_mock_dir ("zen_embargo", {
        cli$deposit_embargo (embargo_date = embargo_date)
    })

    expect_equal (cli$hostdata$metadata$access_right, "embargoed")
    # embargo date is redacted by httptest2 to "2022-01-01"
})

test_that ("zenodo deposits_list", {

    service <- "zenodo"
    cli <- new_mock_deposit (service = service)

    dep <- with_mock_dir ("zen_list", {
        cli$deposits_list ()
    })

    expect_s3_class (dep, "depositsClient")
    expect_identical (dep, cli)
})

test_that ("zenodo add_resource", {

    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description =
            "## description\nThis is the description\n\n## version\n1.0",
        subject = "## keywords\none, two\nthree",
        accessRights = "closed"
    )

    cli <- with_mock_dir ("zen_client", {
        depositsClient$new (
            service = "zenodo",
            sandbox = TRUE,
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
    expect_silent (
        cli$deposit_add_resource (filename)
    )
    files <- fs::path_file (fs::dir_ls (path))
    expect_true ("datapackage.json" %in% files)

    cli <- with_mock_dir ("zen_create", {
        depositsClient$new (service = "zenodo", sandbox = TRUE)
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


test_that ("zenodo upload", {

    service <- "zenodo"
    cli <- new_mock_deposit (service = service)
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
    n_files <- nrow (cli$hostdata$files)

    # --------- UPLOAD ADDITIONAL DATA
    # Initial uploads differ to subsequent uploads; this tests the latter
    filename <- fs::path (fs::path_temp (), "data2.csv")
    write.csv (datasets::Orange, filename)
    cli <- with_mock_dir ("zen_up2", {
        cli$deposit_upload_file (path = filename) # deposit_id from cli$id
    })
    expect_true (nrow (cli$hostdata$files) > n_files)
    expect_true (
        all (c ("data.csv", "data2.csv") %in% cli$hostdata$files$filename)
    )
})

test_that ("zenodo upload binary", {

    service <- "zenodo"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    filename <- file.path (tempdir (), "data.Rds")
    saveRDS (datasets::Orange, filename)

    # cli <- with_mock_dir ("zen_up_bin", {
    #     cli$deposit_upload_file (path = filename)
    # })

    # expect_true (nrow (cli$hostdata$files) > 0L)
    # i <- which (cli$hostdata$files$filename == "data.Rds")
    # expect_identical (
    #     gsub ("^md5\\:", "", cli$hostdata$files$checksum [i]),
    #     unname (tools::md5sum (filename))
    # )

})

test_that ("zenodo download", {

    service <- "zenodo"
    cli <- new_mock_deposit (service = service)
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

# The previous update tests were updating in response to changes to internal
# client metadata. This tests updating in response to changes in external
# "datapackage.json" file.
test_that ("zenodo update", {

    service <- "zenodo"
    cli <- new_mock_deposit (service = service)
    deposit_id <- cli$id

    path <- fs::path (fs::path_temp (), "data")
    fs::dir_create (path)
    filename <- fs::path (path, "data.csv")
    write.csv (datasets::Orange, filename)

    cli <- with_mock_dir ("zen_up", {
        cli$deposit_upload_file (path = filename) # deposit_id from cli$id
    })

    # Modify local metadata:
    cli$metadata$title <- "Modified Title"
    # This should generate a warning that metadata differs, but error comes
    # first. Warning can't be generated in test env because of reasons explained
    # below.
    expect_error (
        with_mock_dir ("zen_update_dp", {
            cli$deposit_update (path = path)
        }),
        "Local file \\[datapackage\\.json\\] does not exist on remote"
    )

    # Modify local "datapackage.json":
    f <- fs::path (path, "datapackage.json")
    x <- readLines (f)
    i <- grep ("Original\\ssource", x)
    x [i] <- gsub ("Original\\ssource", "A description", x [i])
    i <- grep ("Time\\-series\\sanalyses", x)
    x [i] <- gsub (
        "Time\\-series\\sanalyses\\sof\\sbeaver\\sbody\\stemperatures",
        "New Title",
        x [i]
    )
    writeLines (x, f)

    # ----- NOTE -----
    # This can't be tested at present, because the "datapackage.json" files can
    # not be uploaded in test environments, as explained in comment in private
    # "update_frictionless" method. That means that attempting to update
    # triggers an error that file does not exist on remote deposit.

    # cli <- with_mock_dir ("zen_update_dp", {
    #     cli$deposit_update (path = path)
    # })
    expect_error (
        with_mock_dir ("zen_update_dp", {
            cli$deposit_update (path = path)
        }),
        "Local file \\[datapackage\\.json\\] does not exist on remote"
    )
})

test_that ("zenodo version", {

    service <- "zenodo"

    cli <- new_mock_deposit (service = service)

    # Increment version number:
    vers0 <- cli$hostdata$metadata$version
    vers <- regmatches (vers0, regexpr ("[0-9].*$", vers0))
    nc <- nchar (vers)
    incr <- as.integer (substring (vers, nc, nc)) + 1L
    substring (vers, nc, nc) <- paste0 (incr)

    cre <- lapply (
        cli$hostdata$metadata$creators,
        function (i) list (name = i$name)
    )
    metadata <- list (
        title = cli$hostdata$metadata$title,
        description = cli$hostdata$metadata$description,
        creator = cre,
        subject = list (version = vers),
        accessRights = "closed"
    )
    cli$deposit_fill_metadata (metadata)

    # This can no longer be tested, because deposit data are no longer returned
    # from, or listed on, sandbox:
    # cli <- with_mock_dir ("zen_vers", {
    #     cli$deposit_version ()
    # })

    # expect_equal (cli$hostdata$metadata$version, vers)
    # expect_false (cli$hostdata$metadata$version == vers0)
})

# can't mock delete because it returns an empty body
test_that ("zenodo delete", {
    # dep <- with_mock_dir ("zen_del", {
    #    cli$deposit_delete (deposit_id)
    # })
    # expect_true (dep)
})

Sys.unsetenv ("DEPOSITS_TEST_ENV")
