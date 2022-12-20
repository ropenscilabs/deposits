
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

    expect_identical (cli1$metadata$dcmi$title, "New Title")
    expect_identical (cli1$metadata$dcmi$abstract, "This is the abstract")
    # expect_identical (cli1$metadata$dcmi$creator [[1]], list (name = "A. Person"))
    # expect_identical (cli1$metadata$dcmi$creator [[2]], list (name = "B. Person"))
    expect_identical (cli1$metadata$dcmi$creator [[1]], "A. Person")
    expect_identical (cli1$metadata$dcmi$creator [[2]], "B. Person")

    filename <- tempfile (pattern = "meta_", fileext = ".json")
    service <- "zenodo"
    if (file.exists (filename)) {
        file.remove (filename)
    }
    deposits_metadata_template (filename, metadata)
    cli2 <- with_mock_dir ("meta-new2", {
        depositsClient$new (service, sandbox = TRUE, metadata = filename)
    })

    cli1$metadata$dcmi <- cli1$metadata$dcmi [order (names (cli1$metadata$dcmi))]
    cli2$metadata$dcmi <- cli2$metadata$dcmi [order (names (cli2$metadata$dcmi))]
    # not identical because calling environments differ:
    expect_equal (cli1, cli2)

    meta <- deposits_meta_from_file (filename)
    cli3 <- with_mock_dir ("meta-new3", {
        depositsClient$new (service, sandbox = TRUE, metadata = meta)
    })

    cli3$metadata$dcmi <- cli3$metadata$dcmi [order (names (cli3$metadata$dcmi))]
    expect_equal (cli1, cli3)

    cli4 <- with_mock_dir ("meta-new4", {
        depositsClient$new (service, sandbox = TRUE)
    })
    cli4$deposit_fill_metadata (meta)

    cli4$metadata$dcmi <- cli4$metadata$dcmi [order (names (cli4$metadata$dcmi))]
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

    metadata <- list (
        created = Sys.Date (),
        title = "New Title",
        description = "This is the abstract",
        creator = list ("A. Person", "B. Person")
    )

    metadata_dcmi <- validate_dcmi_metadata (metadata)
    metadata_service <- validate_service_metadata (metadata_dcmi, service = "zenodo")

    metadata_dcmi$owner <- "me" # should be integer
    metadata_dcmi$state <- "notinvocab" # vocab=(inprogress|done|error)
    expect_warning (
        metadata <- validate_service_metadata (metadata_dcmi, service = "zenodo"),
        paste0 (
            "The following metadata terms do not conform:\n",
            "Data \\[owner\\] is not coercible to integer.\n",
            "Data \\[state = 'notinvocab'\\] must follow fixed vocabulary"
        )
    )
    # internal code:
    term_map <- get_dcmi_term_map (service = "zenodo")
    metadata_service <- convert_dcmi_to_zenodo (metadata_dcmi, term_map)
    check <- validate_zenodo_terms (metadata_service)
    expect_length (check, 2L)
    expect_equal (check [1], "Data [owner] is not coercible to integer.")
    msg <- paste0 (
        "Data [state = 'notinvocab'] must follow fixed ",
        "vocabulary of [inprogress, done, error]"
    )
    expect_equal (check [2], msg)

    metadata_dcmi$owner <- 1L
    metadata_dcmi$state <- "done" # vocab=(inprogress|done|error)
    expect_silent (
        metadata <- validate_service_metadata (metadata_dcmi, service = "zenodo")
    )

    metadata_service <- convert_dcmi_to_zenodo (metadata_dcmi, term_map)
    metadata_service$metadata$license <- "none"
    metadata_service$metadata$dates <- Sys.Date ()
    check <- validate_zenodo_terms (metadata_service)

    expect_true (!is.null (check))
    expect_length (check, 2L) # both terms are invalid
    expect_true (grepl ("must be an array", check [1]))
    expect_true (grepl ("must follow fixed vocabulary", check [2]))

    metadata_service$metadata$license <- "MIT"
    metadata_service$metadata$dates <- list (Sys.Date ())
    expect_null (validate_zenodo_terms (metadata_service))

    metadata_service$metadata$upload_type <- "notatype"
    check <- validate_zenodo_terms (metadata_service)
    expect_true (!is.null (check))
    expect_length (check, 1L)
    expect_true (grepl ("must follow fixed vocabulary", check [1]))

    metadata_service$metadata$upload_type <- "dataset"
    expect_null (validate_zenodo_terms (metadata_service))

    metadata_service$metadata$keywords <- "keyword"
    check <- validate_zenodo_terms (metadata_service)
    expect_true (!is.null (check))
    expect_length (check, 1L)
    expect_true (grepl ("must be an array/list object", check [1]))

    metadata_service$metadata$keywords <- list ("keyword")
    expect_null (validate_zenodo_terms (metadata_service))
})

test_that ("figshare metadata terms", {

    metadata <- list (
        created = Sys.Date (),
        title = "New Title",
        description = "This is the abstract",
        creator = list ("A. Person", "B. Person")
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
    expect_warning (
        metadata_service <- validate_service_metadata (metadata_dcmi, service = "figshare"),
        msg
    )
    metadata$license <- 1L
    metadata_dcmi <- validate_dcmi_metadata (metadata)
    expect_silent (
        metadata_service <- validate_service_metadata (metadata_dcmi, service = "figshare")
    )

    # internal code:
    term_map <- get_dcmi_term_map (service = "figshare")
    metadata_service <- convert_dcmi_to_figshare (metadata_dcmi, term_map)
    check <- validate_figshare_terms (metadata_service)
    expect_null (check) # metadata okay

    metadata_service$license <- "MIT" # should be integer
    check <- validate_figshare_terms (metadata_service)
    expect_true (!is.null (check))
    expect_length (check, 1L) # both terms are invalid
    expect_true (grepl ("is not coercible to integer", check [1]))

    metadata_service$license <- 1L
    expect_null (validate_figshare_terms (metadata_service))

    metadata_service$defined_type <- "notatype"
    check <- validate_figshare_terms (metadata_service)
    expect_true (!is.null (check))
    expect_length (check, 1L) # both terms are invalid
    expect_true (grepl ("must follow fixed vocabulary of", check [1]))

    metadata_service$defined_type <- "dataset"
    expect_null (validate_figshare_terms (metadata_service))

    metadata_service$tags <- "tag"
    check <- validate_figshare_terms (metadata_service)
    expect_true (!is.null (check))
    expect_length (check, 1L) # both terms are invalid
    expect_true (grepl ("must have format \\[array-string\\]", check [1]))

    metadata_service$tags <- list ("tag1", "tag2")
    expect_null (validate_figshare_terms (metadata_service))
})
