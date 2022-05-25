
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

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

test_that ("metadata to DCEntry", {

    filename <- tempfile (fileext = ".json")
    deposits_metadata_template (filename)

    m <- readLines (filename)
    m [grep ("Title", m)] <- "  \"Title\": \"New Title\","
    m [grep ("Type", m)] <- "  \"Type\": \"Software\","
    m [grep ("Description", m)] <-
        "  \"Description\": \"Description of software\","
    m [grep ("TableOfContents", m)] <-
        "  \"TableOfContents\": {\"one\": \"First\", \"two\": \"Second\"},"

    expect_true (jsonlite::validate (m))
    writeLines (m, filename)

    # expect_silent ( # produces messages on some test environments
    dc <- deposits_meta_to_dcmi (filename, id = "my-id")
    # )
    expect_s3_class (dc, "DCEntry")

    expect_identical (dc$title [[1]]$value, "New Title")
    expect_identical (dc$type [[1]]$value, "Software")
    expect_identical (dc$description [[1]]$value, "Description of software")
    expect_length (dc$tableOfContents, 2L)
    expect_identical (dc$tableOfContents [[1]]$value, "First")
    expect_identical (dc$tableOfContents [[2]]$value, "Second")
})

# the following test fails on windows machines on r-universe windows machines,
# so switched off from here.
testthat::skip_if (!test_all)

test_that ("client with metadata", {

    service <- "zenodo"
    # the following objects differ in timestamps, so all receive this one:
    the_time <- Sys.time ()

    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )

    cli1 <- with_mock_dir ("meta-new1", {
        depositsClient$new (service, sandbox = TRUE, metadata = metadata)
    })
    cli1$metadata$setUpdated (the_time)

    expect_identical (cli1$metadata$title [[1]]$value, "New Title")
    expect_identical (
        cli1$metadata$abstract [[1]]$value,
        "This is the abstract"
    )
    expect_identical (cli1$metadata$creator [[1]]$value, "A. Person")
    expect_identical (cli1$metadata$creator [[2]]$value, "B. Person")

    filename <- tempfile (pattern = "meta_", fileext = ".json")
    service <- "zenodo"
    if (file.exists (filename)) {
        file.remove (filename)
    }
    deposits_metadata_template (filename, metadata)
    cli2 <- with_mock_dir ("meta-new2", {
        depositsClient$new (service, sandbox = TRUE, metadata = filename)
    })
    cli2$metadata$setUpdated (the_time)

    # not identical because calling environments differ:
    expect_equal (cli1, cli2)

    meta <- deposits_meta_to_dcmi (filename)
    cli3 <- with_mock_dir ("meta-new3", {
        depositsClient$new (service, sandbox = TRUE, metadata = meta)
    })
    cli3$metadata$setUpdated (the_time)

    expect_equal (cli1, cli3)

    cli4 <- with_mock_dir ("meta-new4", {
        depositsClient$new (service, sandbox = TRUE)
    })
    cli4$deposit_fill_metadata (meta)
    cli4$metadata$setUpdated (the_time)

    expect_equal (cli1, cli4)
})

test_that ("client with invalid metadata", {

    service <- "zenodo"
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )
    filename <- tempfile (pattern = "meta_", fileext = ".json")
    if (file.exists (filename)) {
        file.remove (filename)
    }
    deposits_metadata_template (filename, metadata)
    meta <- deposits_meta_to_dcmi (filename)
    meta$creator <- c (meta$creator, "wrong") # must be a 'DCCreator' object

    expect_error (
        cli <- with_mock_dir ("meta-new-error", {
            depositsClient$new (service, sandbox = TRUE, metadata = meta)
        }),
        "metadata is not valid - see details via metadata\\$validate()"
    )
})

test_that ("zenodo metadata terms", {

    metaterms <- list (
        created = Sys.Date (),
        metadata = list (
            title = "New Title",
            description = "This is the abstract",
            creators = list ("A. Person", "B. Person")
        )
    )
    check <- validate_terms (metaterms, service = "zenodo")

    expect_null (check) # metadata okay

    metaterms$owner <- "me" # should be integer
    metaterms$state <- "notinvocab" # vocab=(inprogress|done|error)
    check <- validate_terms (metaterms, service = "zenodo")

    expect_true (!is.null (check))
    expect_length (check, 2L) # both terms are invalid
    expect_true (grepl ("must be an integer", check [1]))
    expect_true (grepl ("not in required vocabulary", check [2]))

    metaterms$owner <- 1L
    metaterms$state <- "done" # vocab=(inprogress|done|error)
    expect_null (validate_terms (metaterms, service = "zenodo"))

    metaterms$metadata$license <- "none"
    metaterms$metadata$dates <- Sys.Date ()
    check <- validate_terms (metaterms, service = "zenodo")

    expect_true (!is.null (check))
    expect_length (check, 2L) # both terms are invalid
    expect_true (grepl ("not in required vocabulary", check [1]))
    expect_true (grepl ("must be an array", check [2]))

    metaterms$metadata$license <- "MIT"
    metaterms$metadata$dates <- list (Sys.Date ())
    expect_null (validate_terms (metaterms, service = "zenodo"))

    metaterms$metadata$upload_type <- "notatype"
    check <- validate_terms (metaterms, service = "zenodo")
    expect_true (!is.null (check))
    expect_length (check, 1L)
    expect_true (grepl ("not in required vocabulary", check [1]))

    metaterms$metadata$upload_type <- "dataset"
    expect_null (validate_terms (metaterms, service = "zenodo"))

    metaterms$metadata$keywords <- "keyword"
    check <- validate_terms (metaterms, service = "zenodo")
    expect_true (!is.null (check))
    expect_length (check, 1L)
    expect_true (grepl ("must be an array/list object", check [1]))

    metaterms$metadata$keywords <- list ("keyword")
    expect_null (validate_terms (metaterms, service = "zenodo"))
})

test_that ("figshare metadata terms", {

    metaterms <- list (
        created = Sys.Date (),
        title = "New Title",
        description = "This is the abstract",
        creators = list ("A. Person", "B. Person")
    )
    check <- validate_terms (metaterms, service = "figshare")
    expect_null (check) # metadata okay

    metaterms$license <- "MIT" # should be integer
    check <- validate_terms (metaterms, service = "figshare")
    expect_true (!is.null (check))
    expect_length (check, 1L) # both terms are invalid
    expect_true (grepl ("is not coercible to integer", check [1]))

    metaterms$license <- 1L
    expect_null (validate_terms (metaterms, service = "figshare"))

    metaterms$defined_type <- "notatype"
    check <- validate_terms (metaterms, service = "figshare")
    expect_true (!is.null (check))
    expect_length (check, 1L) # both terms are invalid
    expect_true (grepl ("must follow fixed vocabulary of", check [1]))

    metaterms$defined_type <- "dataset"
    expect_null (validate_terms (metaterms, service = "figshare"))

    metaterms$tags <- "tag"
    check <- validate_terms (metaterms, service = "figshare")
    expect_true (!is.null (check))
    expect_length (check, 1L) # both terms are invalid
    expect_true (grepl ("must have format \\[array-string\\]", check [1]))

    metaterms$tags <- list ("tag1", "tag2")
    expect_null (validate_terms (metaterms, service = "figshare"))
})
