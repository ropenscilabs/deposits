# Function to construct single mock deposit
new_mock_deposit <- function (service = "zenodo") {

    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description =
            "## description\nThis is the description\n\n## version\n1.0",
        subject = "## keywords\none, two\nthree"
    )

    prfx <- ifelse (service == "zenodo", "zen", "fs")

    cli <- httptest2::with_mock_dir (paste0 (prfx, "_client"), {
        depositsClient$new (
            service = service,
            sandbox = TRUE,
            metadata = metadata
        )
    })
    cli <- httptest2::with_mock_dir (paste0 (prfx, "_new"), {
        cli$deposit_new (prereserve_doi = TRUE)
    })

    return (cli)
}
