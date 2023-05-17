test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

test_that ("metadata template", {

    filename <- tempfile (fileext = ".json")
    # expect_silent (
    out <- deposits_metadata_template (filename)
    # )
    expect_true (out)
    expect_true (file.exists (filename))

    json <- readLines (filename)
    expect_true (jsonlite::validate (json))

    expect_error (
        out <- deposits_metadata_template (filename),
        "already exists; please delete before calling this function."
    )
})

# the following test fails on windows machines on r-universe windows machines,
# so switched off from here.
testthat::skip_if (!test_all)

test_that ("client with metadata", {

    service <- "zenodo"

    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person"))
    )

    cli1 <- with_mock_dir ("meta-new1", {
        depositsClient$new (service, sandbox = TRUE, metadata = metadata)
    })

    expect_identical (cli1$metadata$title, "New Title")
    expect_identical (cli1$metadata$abstract, "This is the abstract")
    expect_identical (cli1$metadata$creator [[1]], list (name = "A. Person"))
    expect_identical (cli1$metadata$creator [[2]], list (name = "B. Person"))

    filename <- tempfile (pattern = "meta_", fileext = ".json")
    service <- "zenodo"
    if (file.exists (filename)) {
        file.remove (filename)
    }

    filename <- fs::file_temp (ext = ".json")
    jsonlite::write_json (metadata, filename, auto_unbox = TRUE)
    cli2 <- with_mock_dir ("meta-new2", {
        depositsClient$new (service, sandbox = TRUE, metadata = filename)
    })

    # clients are not identical because calling environments differ.
    expect_equal (cli1, cli2)

    meta <- deposits_meta_from_file (filename)
    cli3 <- with_mock_dir ("meta-new3", {
        depositsClient$new (service, sandbox = TRUE, metadata = meta)
    })

    expect_equal (cli1, cli3)

    cli4 <- with_mock_dir ("meta-new4", {
        depositsClient$new (service, sandbox = TRUE)
    })
    cli4$deposit_fill_metadata (meta)

    expect_equal (cli1, cli4)
})

test_that ("client with invalid metadata", {

    service <- "zenodo"
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person"))
    )
    filename <- fs::file_temp (ext = ".json")
    jsonlite::write_json (metadata, filename, auto_unbox = TRUE)
    # meta$creator <- c (meta$creator, "wrong") # must be a 'DCCreator' object

    # expect_error (
    #     cli <- with_mock_dir ("meta-new-error", {
    #         depositsClient$new (service, sandbox = TRUE, metadata = meta)
    #     }),
    #     "metadata is not valid - see details via metadata\\$validate()"
    # )
})

test_that ("zenodo metadata terms", {

    metadata <- list (
        created = Sys.Date (),
        title = "New Title",
        description = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person"))
    )

    metadata_dcmi <- validate_dcmi_metadata (metadata)
    metadata_service <- validate_service_metadata (metadata_dcmi, service = "zenodo")

    metadata_dcmi$owner <- "me" # should be integer
    metadata_dcmi$state <- "notinvocab" # vocab=(inprogress|done|error)
    expect_false (
        v <- validate_service_metadata (metadata_dcmi, service = "zenodo")
    )
    errs <- attr (v, "errors")
    expect_false (is.null (errs))
    expect_s3_class (errs, "data.frame")
    expect_true (nrow (errs) > 0L)

    metadata_dcmi$owner <- 1L
    metadata_dcmi$state <- "done" # vocab=(inprogress|done|error)
    expect_silent (
        metadata <- validate_service_metadata (metadata_dcmi, service = "zenodo")
    )
})

test_that ("figshare metadata terms", {

    metadata <- list (
        created = Sys.Date (),
        title = "New Title",
        description = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person"))
    )
    metadata_dcmi <- validate_dcmi_metadata (metadata)
    expect_silent (
        metadata_service <- validate_service_metadata (metadata_dcmi, service = "figshare")
    )

    metadata$license <- "MIT"
    metadata_dcmi <- validate_dcmi_metadata (metadata)
    msg <- paste0 (
        "Figshare licenses must be integer-valued; ",
        "the value will be reset to '1' = 'CC-BY'"
    )
    expect_false (
        v <- validate_service_metadata (metadata_dcmi, service = "figshare")
    )
    errs <- attr (v, "errors")
    expect_false (is.null (errs))
    expect_s3_class (errs, "data.frame")
    expect_true (nrow (errs) > 0L)

    metadata$license <- 1L
    expect_silent ( # Figshare licenses are integer-valued
        metadata_dcmi <- validate_dcmi_metadata (metadata)
    )
    metadata$license <- list (1L)
    expect_error (
        metadata_dcmi <- validate_dcmi_metadata (metadata),
        "Stopping because the DCMI metadata terms listed above do not conform"
    )
})

test_that ("meta from DESCRIPTION file", {

    desc <- system.file ("DESCRIPTION", package = "deposits")
    tdir <- fs::file_temp (pattern = "pkg")
    fs::dir_create (tdir)
    fs::file_copy (desc, tdir)
    desc <- data.frame (read.dcf (desc))

    meta <- validate_metadata (tdir, service = "zenodo")
    dcmi <- meta$dcmi
    expect_equal (dcmi$title, desc$Title)
    expect_equal (dcmi$license, desc$License)
    # dcmi$Description has Version appended
    expect_false (identical (dcmi$description, desc$Description))
    expect_true (grepl ("Version", dcmi$description))
    expect_false (grepl ("Version", desc$Description))
})
