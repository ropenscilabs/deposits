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

    expect_warning (
        metadata_valid <- validate_metadata (metadata, service = "zenodo"),
        "The following metadata terms do not conform and will be removed"
    )
    nms_in <- sort (names (metadata))
    nms_out <- sort (names (metadata_valid$dcmi))
    # 'not' has been removed:
    expect_equal (length (nms_in) - 1L, length (nms_out))
    expect_identical (nms_in [which (!nms_in == "not")], nms_out)
    # Then remove for further tests:
    metadata$not <- NULL

    # Zenodo allows creators to have 'name', 'affiliation', 'orcid', 'gnd'
    # Figshare allows creators to have 'name', 'id', 'first_name', 'last_name',
    # 'email', 'orcid'

    expect_silent (
        metadata_valid <- validate_metadata (metadata, service = "zenodo")
    )
    expect_error (
        metadata_valid <- validate_metadata (metadata, service = "figshare"),
        "Stopping because the metadata terms listed above do not conform"
    )

    metadata$creator <- list (
        list (name = "A. Person", id = "no", orcid = ORCID), # id must be integer
        list (name = "B. Person")
    )
    expect_error (
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
