
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
        creator = list ("A. Person", "B. Person")
    )

    cli1 <- with_mock_dir ("meta-new1", {
        depositsClient$new (service, sandbox = TRUE, metadata = metadata)
    })

    expect_identical (cli1$metadata$metadata$title, "New Title")
    expect_identical (cli1$metadata$abstract, "This is the abstract")
    expect_identical (cli1$metadata$creator [[1]], "A. Person")
    expect_identical (cli1$metadata$creator [[2]], "B. Person")

    filename <- tempfile (pattern = "meta_", fileext = ".json")
    service <- "zenodo"
    if (file.exists (filename)) {
        file.remove (filename)
    }
    deposits_metadata_template (filename, metadata)
    cli2 <- with_mock_dir ("meta-new2", {
        depositsClient$new (service, sandbox = TRUE, metadata = filename)
    })

    # not identical because calling environments differ:
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
        creator = list ("A. Person", "B. Person")
    )
    filename <- tempfile (pattern = "meta_", fileext = ".json")
    if (file.exists (filename)) {
        file.remove (filename)
    }
    deposits_metadata_template (filename, metadata)
    meta <- deposits_meta_from_file (filename)
    meta$creator <- c (meta$creator, "wrong") # must be a 'DCCreator' object

    # expect_error (
    #     cli <- with_mock_dir ("meta-new-error", {
    #         depositsClient$new (service, sandbox = TRUE, metadata = meta)
    #     }),
    #     "metadata is not valid - see details via metadata\\$validate()"
    # )
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
    expect_true (grepl ("is not coercible to integer", check [1]))
    expect_true (grepl ("must follow fixed vocabulary of", check [2]))

    metaterms$owner <- 1L
    metaterms$state <- "done" # vocab=(inprogress|done|error)
    expect_null (validate_terms (metaterms, service = "zenodo"))

    metaterms$metadata$license <- "none"
    metaterms$metadata$dates <- Sys.Date ()
    check <- validate_terms (metaterms, service = "zenodo")

    expect_true (!is.null (check))
    expect_length (check, 2L) # both terms are invalid
    expect_true (grepl ("must be an array", check [1]))
    expect_true (grepl ("must follow fixed vocabulary", check [2]))

    metaterms$metadata$license <- "MIT"
    metaterms$metadata$dates <- list (Sys.Date ())
    expect_null (validate_terms (metaterms, service = "zenodo"))

    metaterms$metadata$upload_type <- "notatype"
    check <- validate_terms (metaterms, service = "zenodo")
    expect_true (!is.null (check))
    expect_length (check, 1L)
    expect_true (grepl ("must follow fixed vocabulary", check [1]))

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
