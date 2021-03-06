
test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

# This envvar is used only in the private 'upload_dcmi_xml()' function, in which
# it converts the contents of the uploaded XML file to a standardised form
# (uniform timestamps and article id values).
Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

test_that ("figshare search", {

    cli <- with_mock_dir ("fs_create", {
        depositsClient$new (service = "figshare")
    })

    x <- with_mock_dir ("search_fs", {
        cli$deposits_search (search_string = "random", page_size = 5L)
    })

    expect_s3_class (x, "data.frame")
    expect_equal (nrow (x), 5L)
    expect_true (any (grepl ("random", x$title)))
})

test_that ("zenodo search", {

    cli <- with_mock_dir ("zen_create", {
        depositsClient$new (service = "zenodo", sandbox = TRUE)
    })

    x <- with_mock_dir ("search_zen", {
        cli$deposits_search (search_string = "random", page_size = 5L)
    })

    expect_type (x, "list")
    expect_equal (length (x), 3L)
    expect_equal (names (x), c ("aggregations", "hits", "links"))

    meta <- x$hits$hits$metadata
    expect_true (any (grepl ("random", meta$description)))
})

test_that ("general search errors", {

    cli <- with_mock_dir ("fs_create", {
        depositsClient$new (service = "figshare") # service doesn't matter here
    })

    expect_error (
        cli$deposits_search (search_string = 1L),
        "Assertion on 'search_string' failed: Must be of type 'character'"
    )

    expect_error (
        cli$deposits_search (page_size = "a"),
        "Assertion on 'page_size' failed: Must be of type 'single integerish value'"
    )

    expect_error (
        cli$deposits_search (page_number = "a"),
        "Assertion on 'page_number' failed: Must be of type 'single integerish value'"
    )
})

test_that ("figshare search errors", {

    cli <- with_mock_dir ("fs_create", {
        depositsClient$new (service = "figshare") # service doesn't matter here
    })

    expect_error (
        cli$deposits_search (search_string = "search string", not_an_arg = TRUE),
        "The parameters \\[not_an_arg\\] are not figshare search parameters"
    )

    expect_error (
        cli$deposits_search (search_string = "search string", item_type = "this"),
        "Assertion on '\"this\"' failed: Must be of type 'single integerish value'"
    )

})

test_that ("zenodo search errors", {

    cli <- with_mock_dir ("zen_create", {
        depositsClient$new (service = "zenodo", sandbox = TRUE)
    })


    expect_error (
        cli$deposits_search (search_string = "search string", not_an_arg = TRUE),
        "The parameters \\[not_an_arg\\] are not zenodo search parameters"
    )

    expect_error (
        cli$deposits_search (search_string = "search string", type = 1L),
        "Assertion on '1L' failed: Must be of type 'character'"
    )

})
