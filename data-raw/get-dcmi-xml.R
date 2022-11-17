
# From https://www.dublincore.org/schemas/xmls/

urls <- c (
    "https://www.dublincore.org/schemas/xmls/qdc/dc.xsd",
    "https://www.dublincore.org/schemas/xmls/qdc/dcterms.xsd",
    "https://www.dublincore.org/schemas/xmls/qdc/dcmitype.xsd"
)

for (u in urls) {
    f <- file.path (here::here (), "inst", "extdata", "dc", basename (u))
    download.file (u, f)
}
