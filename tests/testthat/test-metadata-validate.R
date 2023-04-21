test_that ("metadata validate", {

    ORCID <- "0000-0000-0000-0000" # nolint
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (
            list (name = "A. Person", affiliation = "no", orcid = ORCID),
            list (name = "B. Person")
        ),
        description = paste0 (
            "## description\nThis is the description\n\n",
            "## keywords\none, two\nthree\n\n## version\n1.0"
        ),
        not = "Not a property"
    )

    expect_error (
        suppressWarnings (
            metadata_valid <- validate_metadata (metadata, service = "zenodo")
        ),
        "Stopping because the DCMI metadata terms listed above do not conform"
    )

    metadata$not <- NULL
    expect_error ( # id must be integer
        metadata_valid <- validate_metadata (metadata, service = "figshare"),
        "Metadata source for \\[keywords\\] should be \\[subject\\]"
    )
    metadata$description <- "## description\nThis is the description"
    metadata$subject <- "## keywords\none, two\nthree"
    metadata$creator [[1]]$affiliation <- NULL
    expect_silent (
        metadata_valid <- validate_metadata (metadata, service = "figshare")
    )

    metadata$creator <- list (
        list (name = "A. Person", id = "no", orcid = ORCID),
        list (name = "B. Person")
    )
    expect_error ( # id must be integer
        metadata_valid <- validate_metadata (metadata, service = "figshare"),
        "Stopping because the DCMI metadata terms listed above do not conform"
    )

    # Numeric in R still passes as integer in JSON:
    metadata$creator [[1]]$id <- 1
    expect_silent (
        metadata_valid <- validate_metadata (metadata, service = "figshare")
    )

    # But fails if passed any decimals:
    metadata$creator [[1]]$id <- 1.1
    expect_error (
        metadata_valid <- validate_metadata (metadata, service = "figshare"),
        "Stopping because the DCMI metadata terms listed above do not conform"
    )
})

# Test parsing multiple metadata sources in different formats. Here, "keywords"
# and "subjects" have to be specified in single DCMI "subject" field. This tests
# that multiple ways of specifying these 2 fields produce same result. See #63.
test_that ("metadata parsing", {


    service <- "zenodo"
    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description = paste0 (
            "## description\nThis is the description\n\n",
            "## version\n1.0"
        ),
        subject = "## keywords\none\ntwo, three"
    )

    # The 2 validation calls in main client initialization:
    expect_silent (
        metadata_dcmi1 <- validate_dcmi_metadata (metadata)
    )
    expect_silent (
        metadata_service1 <- translate_dc_to_service (metadata_dcmi1, service = service)
    )

    # Alternative specification:
    metadata$subject <- list (
        keywords = list ("one", "two", "three")
    )
    expect_silent (
        metadata_dcmi2 <- validate_dcmi_metadata (metadata)
    )
    expect_silent (
        metadata_service2 <- translate_dc_to_service (metadata_dcmi2, service = service)
    )

    expect_false (identical (metadata_dcmi1, metadata_dcmi2))

    # timestamps can differ:
    metadata_service1$created <- metadata_service2$created <- NULL
    expect_identical (metadata_service1, metadata_service2)
})
