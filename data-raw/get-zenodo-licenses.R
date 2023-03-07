token <- Sys.getenv ("ZENODO_TOKEN")
url <- "https://zenodo.org/api/licenses?size=1000"
headers <- list (Authorization = paste0 ("Bearer ", token))
con <- crul::HttpClient$new (url, headers = headers)
res <- con$get ()
lic <- jsonlite::fromJSON (res$parse (encoding = "UTF-8"))
lic <- lic$hits$hits$metadata
lic <- lic [, c ("id", "title", "url")]

path <- fs::path (here::here (), "inst", "extdata", "zenodo", "zenodo_licenses.csv")
write.csv (lic, file = path, row.names = FALSE, fileEncoding = "UTF-8")
