# This envvar turns off the interactive readline parts of the
# figshare_categories function.
Sys.setenv ("DEPOSITS_TEST_ENV" = "true")

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
