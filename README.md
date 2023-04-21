# deposits

<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/deposits/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/deposits/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropenscilabs/deposits/branch/main/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/deposits)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The `deposits` R package is a universal client for depositing and
accessing research data anywhere. Currently supported services are
[zenodo](https://zenodo.org) and [figshare](https://figshare.com). The
package works seamlessly with [the “frictionless” data
workflow](https://frictionlessdata.io/). Instructions for installing and
setting up the package are in [a separate
vignette](https://docs.ropensci.org/deposits/articles/install-setup.html).
This README gives a brief overview of package usage, with more detail in
[the introductory
vignette](https://docs.ropensci.org/deposits/articles/deposits.html).

## Data Repositories

The list of data repositories currently supported is accessible by the
`deposits_services()` function:

``` r
library (deposits)
deposits_services ()
```

    ##             name                           docs                    api_base_url
    ## 1         zenodo https://developers.zenodo.org/         https://zenodo.org/api/
    ## 2 zenodo-sandbox https://developers.zenodo.org/ https://sandbox.zenodo.org/api/
    ## 3       figshare     https://docs.figshare.com/    https://api.figshare.com/v2/

The [`zenodo`](https://zenodo.org) “sandbox” environment offers an ideal
environment for testing the functionality of this package.

## The “frictionless” data workflow

Prior to describing the `deposits` workflow, it is useful to understand
the [“frictionless” data workflow](https://frictionlessdata.io/), which
provides a standardised way to document the contents and structure of
data files through a separate metadata file. The `deposits` package
links directly with [rOpenSci’s `frictionless`
package](https://docs.ropensci.org/frictionless) for writing, editing,
and reading frictionless metadata files.

The `deposits` package nevertheless aims to make this integration as
seamless and painless as possible. No knowledge of the frictionless
workflow is necessary, and the `deposits` workflow should “just work” in
many cases, appropriately constructing frictionless metadata files and
uploading them to deposits services. It is of course also possible to
use the `deposits` package without frictionless metadata.

## The deposits workflow

The `deposits` package uses [the `R6`
package](https://github.com/r-lib/R6) to create [a
`depositsClient`](https://docs.ropensci.org/deposits/reference/depositsClient.html)
used to perform all functions. All `deposits` operations start with a
client constructed with the `new()` function:

``` r
cli <- depositsClient$new (service = "zenodo", sandbox = TRUE)
print (cli)
#> <deposits client>
#>  deposits service : zenodo
#>            sandbox: TRUE
#>          url_base : https://sandbox.zenodo.org/api/
#>  Current deposits : <none>
#>
#>    hostdata : <none>
#>    metadata : <none>
```

The upper section of information shown by printing the client describes
information on the client in general, and aspects of the nominated
service associated with the user (identified through the token, as
described in [the “Installation and Setup”
vignette](https://docs.ropensci.org/deposits/articles/install-setup.html#setup-api-tokens)).
The lower section contains information on the current deposit held
within the client. A new client initially contains no information on a
deposit, and so these lower sections are empty.

The code above demonstrates how `R6` objects work, through calling
“methods” or functions on the main “object” via the `$` symbol. The
“new” method must be called on [a `depositsClient`
object](https://docs.ropensci.org/deposits/reference/depositsClient.html).
From that point on, all other methods are called on that object itself.
For example, the following calls lists all methods implemented by the
client:

``` r
cli$deposits_methods ()
#> List of methods for a deposits client:
#>
#>    - deposit_delete
#>    - deposit_delete_file
#>    - deposit_download_file
#>    - deposit_embargo
#>    - deposit_fill_metadata
#>    - deposit_new
#>    - deposit_publish
#>    - deposit_retrieve
#>    - deposit_service
#>    - deposit_update
#>    - deposit_update_frictionless
#>    - deposit_upload_file
#>    - deposits_list
#>    - deposits_methods
#>    - deposits_search
#>
#>  see `?depositsClient` for full details of all methods.
```

All of those methods then work similarly, by calling
`cli$<method>(<parameters>)`. Note that R6 functions are called directly
on the client, with the object itself (`cli`) updated by the call. For
example, [the `deposit_fill_metadata()`
method](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-fill-metadata-)
described below can be used to specify metadata for a deposit. This
works by simply calling,

``` r
cli$deposit_fill_metadata (metadata)
```

The `cli` object is then updated by this call, without needing to be
assigned to a return value (like `cli <- cli$<method>`).

A typical deposits workflow, and associated methods, involves the
following steps:

1.  Fill the client with metadata (using the
    [`new()`](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-new-)
    or
    [`deposit_fill_metadata()`](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-depositsClient-deposit_fill_metadata)
    methods);
2.  Create a new deposit on the nominated service (using the
    [`deposit_new()`](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-new-)
    method); and
3.  Upload files for the deposit (using the
    [`deposit_upload_file()`](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-upload-file-)
    method).

The following sections describe each of these steps in more detail.

### Fill client with metadata

The *metadata* of a deposit describe the nature and properties of the
data being deposited. A deposit is first created by uploading metadata
to a deposits service; only then can actual data be uploaded and
associated with the deposit described by the metadata. The `deposits`
package works with two main types of metadata:

1.  Metadata which describe a deposit and associated properties, such as
    author names and affiliations, deposit titles and descriptions,
    dates, keywords, links to other deposits or publications, and many
    other terms.
2.  Metadata which describe the actual contents of the data to be
    deposited. These kinds of metadata are (optionally) generated and
    (always) handled here by [the `frictionless`
    package](https://docs.ropensci.org/frictionless).

As explained at the outset, the `deposits` package can be used without
knowing or understanding anything about [the `frictionless`
package](https://docs.ropensci.org/frictionless) or [frictionless
workflows](https://frictionlessdata.io). Most of this present section
therefore concerns the first of these two kinds of metadata, with brief
demonstrations of the second kind used to illustrate how the
frictionless workflow integrates with the general `deposits` workflow.
The term “metadata” throughout the following should be understood to
refer to the first of the above kinds of metadata; the second kind is
always referred to here as “frictionless metadata.”

Metadata can be included in a deposits client in two main ways, either
on initial construction, or through attaching metadata to an existing
client. One of the easiest of specifying metadata is as a list of named
terms like the following example:

``` r
metadata <- list (
    title = "New Title",
    abstract = "This is the abstract",
    creator = list (list (name = "A. Person"), list (name = "B. Person"))
)
```

These data can be used in construction of a new client by passing a
`metadata` argument:

``` r
cli <- depositsClient$new (
    service = "zenodo",
    sandbox = TRUE,
    metadata = metadata
)
print (cli)
#> <deposits client>
#>  deposits service : zenodo
#>            sandbox: TRUE
#>          url_base : https://sandbox.zenodo.org/api/
#>  Current deposits : <none>
#>
#>    hostdata : <none>
#>    metadata : 3 terms (see 'metadata' element for details)
```

The “metadata” component of the client now holds 3 terms. Equivalently,
metadata can be added to an existing client with [the
`deposit_fill_metadata()`
function](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-depositsClient-deposit_fill_metadata):

``` r
cli <- depositsClient$new (service = "zenodo", sandbox = TRUE)
cli$deposit_fill_metadata (metadata)
```

Other ways of specifying and entering metadata are described in [the
introductory
vignette](https://docs.ropensci.org/deposits/articles/deposits.html). At
that stage, the metadata are only associated with the local client. The
following section describes how to use those metadata to initiate a
deposit on an external service.

### Create a new deposit

The metadata filled with the above steps can then be used to initiate a
new deposit on the associated server using [the `deposit_new()`
method](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-new-).

``` r
cli$deposit_new ()
#> ID of new deposit : 1065666
print (cli)
#> <deposits client>
#>  deposits service : zenodo
#>            sandbox: TRUE
#>          url_base : https://sandbox.zenodo.org/api/
#>  Current deposits : <none>
#>
#>  url_service : https://sandbox.zenodo.org/deposit/1065666
#>   deposit id : 1065666
#>     hostdata : list with 14  elements
#>     metadata : 3 terms (see 'metadata' element for details)
```

The client now includes several additional elements, notably a “deposit
id” (stored in `cli$id`) giving the unique identifier for the new
deposit, and a `hostdata` item with, in this case, 14 elements as
specified by the host service. The `url_service` is the URL for the
newly-created deposit. (Viewing in a web browser will require logging in
for all private and sandbox deposits).

### Uploading (and downloading) files

A deposit is really about data, not just metadata. Data can be uploaded
to a deposit with [the `deposit_upload_file()`
method](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-upload-file-):

``` r
cli$deposit_upload_file (path = "<path>/<to>/my-data.dat")
```

Details of files associated with deposits are stored in a `data.frame`
held as `cli$hostdata$files` which after running that command will
include a row with an item whose “filename” will be “my-data.dat”.

``` r
cli$hostdata$files
#>                           checksum    filename filesize
#> 1 5955bb96a8fee3bc89549bde9ef9b470 my-data.dat      829
#>                                     id
#> 1 618ae9b9-af48-4b86-aa37-7b4e767dccb7
#>                                                 links.download
#> 1 https://sandbox.zenodo.org/api/files/<file-hash>/my-data.dat
#>                                                                links.self
#> 1 https://sandbox.zenodo.org/api/deposit/depositions/1065666/files/<hash>
```

The `deposit_download_file()` function does the reverse:

``` r
path <- cli$deposit_download_file ("my-data.dat")
```

Files are by default downloaded to the current working directory, or
elsewhere specified by an additional `path` parameter. Unlike most
methods for deposit clients, which invisibly return an updated version
of the client, the `deposit_download_file()` returns the full local path
to the downloaded file.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors`
package](https://github.com/ropenscilabs/allcontributors) following the
[all-contributors](https://allcontributors.org) specification.
Contributions of any kind are welcome!

### Code

<table>
<tr>
<td align="center">
<a href="https://github.com/mpadge">
<img src="https://avatars.githubusercontent.com/u/6697851?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropenscilabs/deposits/commits?author=mpadge">mpadge</a>
</td>
</tr>
</table>

### Issues

<table>
<tr>
<td align="center">
<a href="https://github.com/collinschwantes">
<img src="https://avatars.githubusercontent.com/u/6107885?u=414bad1b3dd9499b3dc60180fc411f1fb18de24e&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropenscilabs/deposits/issues?q=is%3Aissue+author%3Acollinschwantes">collinschwantes</a>
</td>
<td align="center">
<a href="https://github.com/Bisaloo">
<img src="https://avatars.githubusercontent.com/u/10783929?u=38e3754466eaa200e20f0609709467b6331cdfbe&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropenscilabs/deposits/issues?q=is%3Aissue+author%3ABisaloo">Bisaloo</a>
</td>
<td align="center">
<a href="https://github.com/maelle">
<img src="https://avatars.githubusercontent.com/u/8360597?u=824f03caa87c92420352e3dd9a05470320a67412&v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropenscilabs/deposits/issues?q=is%3Aissue+author%3Amaelle">maelle</a>
</td>
<td align="center">
<a href="https://github.com/joelnitta">
<img src="https://avatars.githubusercontent.com/u/13459362?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropenscilabs/deposits/issues?q=is%3Aissue+author%3Ajoelnitta">joelnitta</a>
</td>
<td align="center">
<a href="https://github.com/noamross">
<img src="https://avatars.githubusercontent.com/u/571752?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropenscilabs/deposits/issues?q=is%3Aissue+author%3Anoamross">noamross</a>
</td>
</tr>
</table>
<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
