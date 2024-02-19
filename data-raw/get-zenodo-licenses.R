token <- Sys.getenv ("ZENODO_TOKEN")
url <- "https://zenodo.org/api/licenses"
headers <- list (Authorization = paste0 ("Bearer ", token))
req <- httr2::request (url)
req <- httr2::req_method (req, "GET")
req <- httr2::req_url_query (req, size = 1000)
resp <- httr2::req_perform (req)
resp_body <- httr2::resp_body_json (resp, simplify = TRUE)

lic <- resp_body$hits$hits [, c ("id", "title", "links")]
lic$title <- unlist (lic$title)
lic$links <- unlist (lic$links)
names (lic) [3] <- "url"

path <- fs::path (here::here (), "inst", "extdata", "zenodo", "zenodo_licenses.csv")
write.csv (lic, file = path, row.names = FALSE, fileEncoding = "UTF-8")
