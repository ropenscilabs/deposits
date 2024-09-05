# deposits

<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/deposits/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/deposits/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropenscilabs/deposits/branch/main/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/deposits)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The deposits R package is a universal client for depositing and
accessing research data in a variety of online deposition services.
Currently supported services are [zenodo](https://zenodo.org) and
[figshare](https://figshare.com). These two systems have fundamentally
different interfaces (“API”s, or Application Programming Interfaces),
and access to these and indeed all deposition services has traditionally
been enabled through individual software clients. The deposits package
aims to be a universal client offering access to a variety of deposition
services, without users having to know any specific details of the APIs
for each service.

The deposits package works seamlessly with [the “frictionless” data
workflow](https://frictionlessdata.io/), to enable unified documentation
of all aspects of datasets in one place. Instructions for installing and
setting up the package are in the [installation
vignette](https://docs.ropensci.org/deposits/articles/install-setup.html).
This README gives a brief overview of package usage, with more detail in
the [introductory
vignette](https://docs.ropensci.org/deposits/articles/deposits.html).
For those who prefer to jump straight in to a workflow, there is also a
deposits [workflow
vignette](https://docs.ropensci.org/deposits/articles/workflow.html)
demonstrating a full workflow from data generation and documentation to
publication.

## Installation

The package can be installed by enabling the "ropenscilabs" repository from
[r-universe](https://ropenscilabs.r-universe.dev):

``` r
options (repos = c (
    ropenscilabs = "https://ropenscilabs.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
))
```

The `install.packages()` command will then install the development version.
Alternatively, the package can be installed with:

``` r
# install.packages("remotes")
remotes::install_github ("mpadge/deposits")
```

The package can then be loaded the usual way:
``` r
library (deposits)
```

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

Prior to describing the deposits workflow, it is useful to understand
the [“frictionless” data workflow](https://frictionlessdata.io/), which
provides a standardised way to document the contents and structure of
data files through a separate metadata file. The deposits package links
directly with [rOpenSci’s `frictionless`
package](https://docs.ropensci.org/frictionless) for writing, editing,
and reading frictionless metadata files.

The deposits package nevertheless aims to make this integration as
seamless and painless as possible. No knowledge of the frictionless
workflow is necessary, and the deposits workflow should “just work” in
many cases, appropriately constructing frictionless metadata files and
uploading them to deposits services. It is of course also possible to
use the deposits package without frictionless metadata.

## The deposits workflow

We now provide a brief overview of the deposits workflow. A more
complete description is given in the [main package
vignette](https://docs.ropensci.org/deposits/articles/deposits.html),
and demonstrated step-by-step in [the workflow
vignette](https://docs.ropensci.org/deposits/articles/workflow.html).
The deposits package uses [the `R6`
package](https://github.com/r-lib/R6) to create [a
`depositsClient`](https://docs.ropensci.org/deposits/reference/depositsClient.html)
used to call all methods needed in a deposits workflow. A [separate
vignette](https://docs.ropensci.org/deposits/articles/deposits-R6.html)
describes the `R6` system for those unfamiliar with it.

A typical deposits workflow, and associated methods, involves the
following steps:

1.  Create a new deposits client
2.  Fill the client with metadata, either as part of Step 1 through
    using the [`new()`
    method](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-new-),
    or afterward by using the
    [`deposit_fill_metadata()`](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-depositsClient-deposit_fill_metadata)
    method;
3.  Create a new deposit on the nominated service using the
    [`deposit_new()`](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-new-)
    method; and
4.  Upload files for the deposit using the
    [`deposit_upload_file()`](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-upload-file-)
    method.

The following sections describe each of these steps in more detail.

### Step 1: Create a new client

All deposits operations start with a client constructed with the `new()`
function:

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

The upper section of information shown by calling `print (cli)` to print
the client describes general information, and aspects of the specified
service, and the profile of the user (identified through the
locally-stored token, as described in [the “Installation and Setup”
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
#>    - deposit_add_resource
#>    - deposit_delete
#>    - deposit_delete_file
#>    - deposit_download_file
#>    - deposit_embargo
#>    - deposit_fill_metadata
#>    - deposit_new
#>    - deposit_prereserve_doi
#>    - deposit_publish
#>    - deposit_retrieve
#>    - deposit_service
#>    - deposit_update
#>    - deposit_upload_file
#>    - deposit_version
#>    - deposits_list
#>    - deposits_methods
#>    - deposits_search
#>
#>  see `?depositsClient` for full details of all methods.
```

All of the methods listed by
[`cli$deposits_methods()`](thos://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposits-methods-)
then work similarly, by calling `cli$<method>(<parameters>)`. Note that
R6 functions are called directly on the client, with the object itself
(`cli`) updated by the call. For example, [the `deposit_fill_metadata()`
method](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-fill-metadata-)
described below can be used to specify metadata for a deposit. This
works by simply calling,

``` r
cli$deposit_fill_metadata (metadata)
```

The `cli` object is then updated by this call, without needing to be
assigned to a return value (so there is no need to use
`cli <- cli$<method>`).

### Step 2: Fill client with metadata

The *metadata* of a deposit describe the nature and properties of the
data being deposited. A deposit is first created by uploading metadata
to a deposits service; only then can actual data be uploaded and
associated with the deposit described by the metadata. The deposits
package works with two main types of metadata:

1.  Metadata which describe a deposit and associated properties, such as
    author names and affiliations, deposit titles and descriptions,
    dates, keywords, links to other deposits or publications, and many
    other terms.
2.  Frictionless metadata which describe the actual contents of the data
    to be deposited. These kinds of metadata are (optionally) generated
    and (always) handled here by [the `frictionless`
    package](https://docs.ropensci.org/frictionless).

(Note that there are actually three main types of metadata, as described
in the [introductory
vignette](https://docs.ropensci.org/deposits/articles/deposits.html),
but the third may generally be ignored, and is intended as “read-only”
metadata provided by host services.) As explained at the outset, the
deposits package can be used without knowing or understanding anything
about [the `frictionless`
package](https://docs.ropensci.org/frictionless) or [frictionless
workflows](https://frictionlessdata.io). Most of this present section
therefore concerns the first of these two kinds of metadata, referred to
throughout all package documentation as “metadata”. Brief demonstrations
are also given of the second kind, to illustrate how the frictionless
workflow integrates with the general deposits workflow. These kinds of
metadata are always referred to as “frictionless metadata.”

A separate vignette describes in detail [how to specify and include
metadata in a deposits
client](https://docs.ropensci.org/deposits/articles/metadata.html). This
section briefly demonstrates the process. An example of deposits
metadata is:

``` r
metadata <- list (
    title = "New Title",
    abstract = "This is the abstract",
    creator = list (list (name = "A. Person"), list (name = "B. Person"))
)
```

The “creator” field is a list-of-lists, to allow individual creator
entries to have multiple fields in addition to “name”. These data can be
used in construction of a new client by passing a `metadata` argument:

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

At that stage, the metadata are only associated with the local client.
The following section describes how to use those metadata to initiate a
deposit on an external service.

### Step 3: Create a new deposit

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
#>     metadata : 4 terms (see 'metadata' element for details)
```

The client now includes several additional elements, notably a “deposit
id” (stored in `cli$id`) giving the unique identifier for the new
deposit, and a “hostdata” item with, in this case, 14 elements as
specified by the host service. The `url_service` is the URL for the
newly-created deposit. (Viewing in a web browser will require logging in
for all private and sandbox deposits). The “metadata” item also includes
an additional “identifier” element containing a pre-reserved DOI
provided by the deposits service.

### Step 4: Upload (or download) files

A deposit is really about data, not just metadata. Data can be uploaded
to a deposit with [the `deposit_upload_file()`
method](https://docs.ropensci.org/deposits/reference/depositsClient.html#method-deposit-upload-file-):

``` r
cli$deposit_upload_file (path = "<path>/<to>/my-data.dat")
```

Details of files associated with deposits are stored in a `data.frame`
stored as part of the “hostdata” of a deposits client, in the
`cli$hostdata$files` item. Prior to uploading, `cli$hostdata$files` will
be empty, but after uploading it will be a `data.frame` including one
row with an item whose “filename” will be “my-data.dat”.

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
cli$deposit_download_file ("my-data.dat")
#> '/<loca>/<directory>/my-data.dat'
```

Files are by default downloaded to the current working directory, or
elsewhere specified by an additional `path` parameter. (Note that this
method returns the full local path to the downloaded file, unlike most
methods for deposit clients, which invisibly return an updated version
of the client.)

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
<td align="center">
<a href="https://github.com/peterdesmet">
<img src="https://avatars.githubusercontent.com/u/600993?v=4" width="100px;" alt=""/>
</a><br>
<a href="https://github.com/ropenscilabs/deposits/issues?q=is%3Aissue+author%3Apeterdesmet">peterdesmet</a>
</td>
</tr>
</table>
<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
