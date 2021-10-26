
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
             identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

test_that("services", {

    expect_silent (
        s <- deposits_services ()
        )
    expect_s3_class (s, "data.frame")
    expect_equal (ncol (s), 3L)
    expect_equal (nrow (s), 2L)
    expect_identical (names (s), c ("name", "docs", "api_base_url"))
    expect_identical (s$name, c ("zenodo", "figshare"))
})

test_that ("tokens", {

    expect_error (get_deposits_token (),
                  "invalid 'pattern' argument")
    expect_error (get_deposits_token ("aaaaa"),
                  "No token found for")

    testthat::skip_if (!test_all)

    tok <- get_deposits_token ("figshare")
    expect_type (tok, "character")
    expect_true (nchar (tok) > 1L)
})
