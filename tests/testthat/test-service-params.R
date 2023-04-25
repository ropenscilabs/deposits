test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

test_that ("service parameters internal", {

    sp <- list (prereserve_doi = TRUE)
    expect_silent (
        sp2 <- validate_service_params (sp)
    )
    expect_identical (sp, sp2)

    sp <- list (prereserve_doi = FALSE)
    expect_silent (
        sp2 <- validate_service_params (sp)
    )
    expect_identical (sp, sp2)

    sp <- list (prereserve_doi = c (TRUE, FALSE))
    expect_error (
        sp2 <- validate_service_params (sp),
        "Stopping because the \'service\\_parameters\\' terms"
    )

    sp <- list (prereserve_doi = "a")
    expect_error (
        sp2 <- validate_service_params (sp),
        "Stopping because the \'service\\_parameters\\' terms"
    )

    # Currently only accepts 'prereserve_doi':
    sp <- list (prereserve_doi = TRUE, nope = FALSE)
    expect_error (
        sp2 <- validate_service_params (sp),
        "Stopping because the \'service\\_parameters\\' terms"
    )
})

testthat::skip_if (!test_all)

test_that ("service parameters client", {

    service <- "zenodo"

    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description =
            "## description\nThis is the description\n\n## version\n1.0",
        subject = "## keywords\none, two\nthree"
    )
    sp <- list (prereserve_doi = TRUE)
    cli <- with_mock_dir ("zen_servpars", {
        depositsClient$new (
            service = service,
            sandbox = TRUE,
            metadata = metadata,
            service_parameters = sp
        )
    })
    cli <- with_mock_dir ("zen_servpars_new", {
        cli$deposit_new ()
    })

    expect_true ("prereserve_doi" %in% names (cli$hostdata$metadata))
    pr <- cli$hostdata$metadata$prereserve_doi
    expect_type (pr, "list")
    expect_length (pr, 2L)
    expect_equal (names (pr), c ("doi", "recid"))
    expect_type (pr$doi, "character")
    expect_type (pr$recid, "integer")
})
