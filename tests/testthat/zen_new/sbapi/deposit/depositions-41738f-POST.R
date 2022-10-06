structure (list (
    method = "POST", url = "sbapi/deposit/depositions",
    status_code = 201L, headers = structure (list (
        Server = "nginx",
        Date = "Thu, 06 Oct 2022 12:03:21 GMT", `Content-Type` = "application/json",
        `Content-Length` = "1247", ETag = "\"0\"", `Last-Modified` = "Thu, 06 Oct 2022 12:03:21 GMT",
        Link = "<https://sandbox.zenodo.org/api/deposit/depositions/1111545/files>; rel=\"files\", <https://sandbox.zenodo.org/api/deposit/depositions/1111545/actions/edit>; rel=\"edit\", <https://sandbox.zenodo.org/api/deposit/depositions/1111545>; rel=\"self\", <https://sandbox.zenodo.org/api/deposit/depositions/1111545/actions/publish>; rel=\"publish\", <https://sandbox.zenodo.org/deposit/1111545>; rel=\"html\", <https://sandbox.zenodo.org/api/deposit/depositions/1111545/actions/discard>; rel=\"discard\"",
        location = "sbapi/deposit/depositions/1111545", `X-RateLimit-Limit` = "100",
        `X-RateLimit-Remaining` = "97", `X-RateLimit-Reset` = "1665057861",
        `Retry-After` = "59", `X-Frame-Options` = "sameorigin",
        `X-XSS-Protection` = "1; mode=block", `X-Content-Type-Options` = "nosniff",
        `Strict-Transport-Security` = "max-age=0", `Referrer-Policy` = "strict-origin-when-cross-origin",
        `Access-Control-Allow-Origin` = "*", `Access-Control-Expose-Headers` = "Content-Type, ETag, Link, X-RateLimit-Limit, X-RateLimit-Remaining, X-RateLimit-Reset",
        `X-User-ID` = "115518", `X-Request-ID` = "6aa444a1f106f7e6bc0bb442b1ddb18d"
    ), class = "httr2_headers"),
    body = charToRaw ("{\"conceptrecid\":\"1111544\",\"created\":\"2022-01-01T00:00:00+00:00\",\"doi\":\"\",\"doi_url\":\"https://doi.org/\",\"files\":[],\"id\":1111545,\"links\":{\"bucket\":\"sbapi/files/e1ac282d-b747-4414-a76a-283bea81f748\",\"discard\":\"sbapi/deposit/depositions/1111545/actions/discard\",\"edit\":\"sbapi/deposit/depositions/1111545/actions/edit\",\"files\":\"sbapi/deposit/depositions/1111545/files\",\"html\":\"https://sandbox.zenodo.org/deposit/1111545\",\"latest_draft\":\"sbapi/deposit/depositions/1111545\",\"latest_draft_html\":\"https://sandbox.zenodo.org/deposit/1111545\",\"publish\":\"sbapi/deposit/depositions/1111545/actions/publish\",\"self\":\"sbapi/deposit/depositions/1111545\"},\"metadata\":{\"access_right\":\"open\",\"creators\":[{\"name\":\"A. Person,B. Person\"}],\"description\":\"This is the abstract\",\"doi\":\"\",\"license\":\"CC-BY-4.0\",\"prereserve_doi\":{\"doi\":\"10.5072/zenodo.1111545\",\"recid\":1111545},\"publication_date\":\"2022-01-01\",\"title\":\"New Title\",\"upload_type\":\"other\"},\"modified\":\"2022-01-01T00:00:00+00:00\",\"owner\":115518,\"record_id\":1111545,\"state\":\"unsubmitted\",\"submitted\":false,\"title\":\"New Title\"}")
), class = "httr2_response")
