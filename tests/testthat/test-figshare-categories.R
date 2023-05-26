# This envvar turns off the interactive readline parts of the
# figshare_categories function.
Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") |
    identical (Sys.getenv ("GITHUB_WORKFLOW"), "test-coverage"))

testthat::skip_if (!test_all)

test_that ("figsahre categories", {

    cats <- figshare_categories ()
    expect_s3_class (cats, "data.frame")
    nms <- c (
        "is_selectable",
        "has_children",
        "id",
        "title",
        "parent_id",
        "path",
        "source_id",
        "taxonomy_id",
        "p1",
        "p2"
    )
    expect_identical (names (cats), nms)
    expect_true (nrow (cats) > 1000)
})
