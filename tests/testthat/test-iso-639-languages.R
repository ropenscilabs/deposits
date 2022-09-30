
test_that ("iso 639 languages", {

    x <- iso_639_2_language_codes ()
    expect_type (x, "character")
    expect_equal (dim (x), c (546L, 2L))
    expect_true (all (nchar (x [, 1]) == 3L))
})
