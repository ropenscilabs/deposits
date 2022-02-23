
test_that("metadata template", {

    filename <- tempfile (fileext = ".json")
    expect_silent (
        out <- deposits_metadata_template (filename)
    )
    expect_true (file.exists (filename))

    json <- readLines (filename)
    expect_true (jsonlite::validate (json))

    expect_error (
        out <- deposits_metadata_template (filename),
        "already exists; please delete before calling this function.")
})
