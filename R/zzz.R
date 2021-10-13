zenodo_base <- function() "https://zenodo.org/api"

deposits_HEAD <- function(url, headers, ...){
  con <- crul::HttpClient$new(url, headers = headers, opts = list(...))
  res <- con$head()
  res$raise_for_status()
  res
}
