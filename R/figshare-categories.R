#' Select figshare categories and return corresponding integer identifier.
#'
#' These identifiers should then be added in deposit metadata as, for example,
#' `subject(categories=c(1,2))`.
#' @export
figshare_categories <- function () {

    url <- "https://api.figshare.com/v2/categories"
    req <- httr2::request (url)
    req <- httr2::req_method (req, "GET")
    resp <- httr2::req_perform (req)
    res <- httr2::resp_body_json (resp, simplifyVector = TRUE)

    # Then next path lists:
    paths <- lapply (
        strsplit (res$path, "/"),
        function (i) i [which (nzchar (i))]
    )

    # hard-code for 3-part hierarchy:
    res$p1 <- vapply (paths, function (i) i [1], character (1L))
    res$p2 <- vapply (paths, function (i) i [2], character (1L))

    # First split, the names of which are not encoded in the data, so taken here
    # from figshare itself:
    res <- split (res, f = as.factor (res$p1))
    nms <- c (
        "Agricultural, veterinary and food sciences",
        "Biological sciences",
        "Biomedical and clinical sciences",
        "Built environment and design",
        "Chemical sciences",
        "Commerce, management, tourism and services",
        "Creative arts and writing",
        "Earth sciences",
        "Economics",
        "Education",
        "Engineering",
        "Environmental sciences",
        "Health sciences",
        "History, heritage and archaeology",
        "Human society",
        "Indigenous studies",
        "Information and computing sciences",
        "Language, communication and culture",
        "Law and legal studies",
        "Mathematical sciences",
        "Philosophy and religious studies",
        "Physical sciences",
        "Psychology"
    )
    stopifnot (length (nms) == length (res))
    names (res) <- nms

    # Second split. The names of each sub-category are then the first items in
    # each group.
    res <- lapply (res, function (i) {
        res_i <- split (i, f = as.factor (i$p2))
        names (res_i) <- vapply (
            res_i,
            function (j) j$title [1],
            character (1L)
        )
        res_i <- lapply (res_i, function (j) j [-1L, ])
        return (res_i)
    })

    if (!interactive () || is_deposits_test_env ()) {
        out <- lapply (res, function (i) do.call (rbind, i))
        return (do.call (rbind, out))
    }

    # ------ Interactive readline code to select category: ------
    message (
        "Choose a number corresponding to one of the ",
        "following categorical groups:"
    )
    print (names (res), width = 20L)
    n1 <- as.integer (readline ("Number: "))
    while (n1 < 1 || n1 > length (res)) {
        message ("Value must be between 1 and ", length (res))
        n1 <- as.integer (readline ("Number: "))
    }

    res_i <- res [[n1]]
    message (
        "Choose a number corresponding to one of the ",
        "following sub-groups in that category:"
    )
    print (names (res_i), width = 20L)
    n2 <- as.integer (readline ("Number: "))
    while (n2 < 1 || n2 > length (res_i)) {
        message ("Value must be between 1 and ", length (res_i))
        n2 <- as.integer (readline ("Number: "))
    }

    res_ij <- res_i [[n2]]
    message (
        "Finally, choose a number corresponding to one ",
        "of the following categories:"
    )
    print (res_ij$title, width = 20L)
    n3 <- as.integer (readline ("Number: "))
    while (n3 < 1 || n3 > length (res_ij)) {
        message ("Value must be between 1 and ", length (res_ij))
        n3 <- as.integer (readline ("Number: "))
    }

    cat_id <- res_ij$id [n3]
    message ("Figshare category id = ", cat_id)

    return (cat_id)
}
