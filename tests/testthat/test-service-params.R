test_that ("service parameters", {

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
})
