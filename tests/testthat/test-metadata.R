
test_that("metadata template", {

    filename <- tempfile (fileext = ".json")
    #expect_silent (
        out <- deposits_metadata_template (filename)
    #)
    expect_true (out)
    expect_true (file.exists (filename))

    json <- readLines (filename)
    expect_true (jsonlite::validate (json))

    expect_error (
        out <- deposits_metadata_template (filename),
        "already exists; please delete before calling this function.")
})

test_that("metadata to DCEntry", {

    library(atom4R) # https://github.com/eblondel/atom4R/pull/10
    filename <- tempfile (fileext = ".json")
    deposits_metadata_template (filename)

    m <- readLines (filename)
    m [grep ("Title", m)] <- "  \"Title\": \"New Title\","
    m [grep ("Type", m)] <- "  \"Type\": \"Software\","
    m [grep ("Description", m)] <-
        "  \"Description\": \"Description of software\","
    m [grep ("TableOfContents", m)] <-
        "  \"TableOfContents\": {\"one\": \"First\", \"two\": \"Second\"},"

    expect_true (jsonlite::validate (m))
    writeLines (m, filename)

    expect_output (
        dc <- deposits_meta_to_dcmi (filename, id = "my-id")
        )
    expect_s3_class (dc, "DCEntry")

    expect_identical (dc$title [[1]]$value, "New Title")
    expect_identical (dc$type [[1]]$value, "Software")
    expect_identical (dc$description [[1]]$value, "Description of software")
    expect_length (dc$tableOfContents, 2L)
    expect_identical (dc$tableOfContents [[1]]$value, "First")
    expect_identical (dc$tableOfContents [[2]]$value, "Second")
})
