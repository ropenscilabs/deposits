# Function to construct single mock deposit
new_mock_deposit <- function (service = "zenodo") {

    metadata <- list (
        title = "New Title",
        abstract = "This is the abstract",
        creator = list (list (name = "A. Person"), list (name = "B. Person")),
        description = paste0 (
            "## description\nThis is the description\n\n",
            "## keywords\none, two\nthree\n\n## version\n1.0"
        )
    )

    prfx <- ifelse (service == "zenodo", "zen", "fs")

    cli <- with_mock_dir (paste0 (prfx, "_client"), {
        depositsClient$new (
            service = service,
            sandbox = TRUE,
            metadata = metadata
        )
    })
    cli <- with_mock_dir (paste0 (prfx, "_new"), {
        cli$deposit_new ()
    })

    return (cli)
}
