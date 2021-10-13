
deposits_HEAD <- function(url, headers, ...) { # nolint (not snake_case)
    con <- crul::HttpClient$new(url, headers = headers, opts = list(...))
    res <- con$head()
    res$raise_for_status()
    res
}
