
test_that ("iso 639 languages", {

    x <- iso_639_2_language_codes ()
    expect_type (x, "character")
    expect_length (x, 547)
    expect_true (all (nchar (x) == 3L))
})
