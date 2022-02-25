# deposits

<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/deposits/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/deposits/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/ropenscilabs/deposits/branch/main/graph/badge.svg)](https://codecov.io/gh/ropenscilabs/deposits)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The `deposits` package is a universal client for depositing and
accessing research data anywhere. Currently supported services are
[zenodo](https://zenodo.org) and [figshare](https://figshare.com).

## Installation

The package can be installed by enabling the [“ropensci”
r-universe](https://ropensci.r-universe.dev), and using
`install.package()`:

``` r
options (repos = c (
    ropensci = 'https://ropensci.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))
install.package ("deposits")
```

Alternatively, the package can be installed directly from GitHub with
the following command:

``` r
remotes::install_github ("mpadge/deposits")
```

The package can then be loaded for use with:

``` r
library (deposits)
```

## Data Repositories

The list of data repositories currently supported is accessible by the
`deposits_services()` function:

``` r
deposits_services ()
```

    ##             name                           docs                    api_base_url
    ## 1         zenodo https://developers.zenodo.org/         https://zenodo.org/api/
    ## 2 zenodo-sandbox https://developers.zenodo.org/ https://sandbox.zenodo.org/api/
    ## 3       figshare     https://docs.figshare.com/    https://api.figshare.com/v2/

[`zenodo`](https://zenodo.org) offers a “sandbox” environment for
testing, which offers an ideal environment for testing the functionality
of this package.

## API tokens

All services require users to create an account and then to generate API
(“Application Programming Interface”) tokens. Click on the following
links to generate tokens, also listed as sequences of menu items used to
reach token settings:

  - [zenodo/account/settings/applications/tokens/new](https://zenodo.org/account/settings/applications/tokens/new/)
  - [zenodo-sandbox/account/settings/applications/tokens/new](https://sandbox.zenodo.org/account/settings/applications/tokens/new/),
  - [figshare/account/applications](https://figshare.com/account/applications).

It is not necessary to create or register applications on any of these
services; this package uses personal tokens only. The tokens need to be
stored as local environment variables the names of which must include
the names of the respective services, as defined by the “name” column
returned from `deposits_service()`, as shown above. This can be done as
in the following example:

``` r
Sys.setenv ("ZENODO_SANDBOX_TOKEN" = "<my-token")
```

Alternatively, these tokens can be stored in a `~/.Renviron` file, where
they will be automatically loaded into every R session.

## Functionality

The `deposits` package extends from [an `R6`
client](https://github.com/r-lib/R6) offering a variety of methods for
performing actions on deposits services. Details of the methods can be
seen in the [help file for the
`depositsClient`](https://docs.ropensci.org/deposits/reference/depositsClient.html).
All `deposits` operations start with a client constructed with the `new`
function, for which the first parameter must be given to specify the
service with which the client will connect:

``` r
cli <- depositsClient$new ("zenodo")
cli
```

    ## <deposits client>
    ##     name: zenodo
    ##  sandbox: FALSE
    ##     url : https://zenodo.org/api/
    ## metadata: <none>

``` r
cli <- depositsClient$new ("zenodo", sandbox = TRUE)
cli
```

    ## <deposits client>
    ##     name: zenodo
    ##  sandbox: TRUE
    ##     url : https://sandbox.zenodo.org/api/
    ## metadata: <none>

A `deposits` client offers the following additional functions, listed
here with default values for all parameters:

1.  `ping()` to verify Authorization tokens;
2.  `list_deposits()` to do just that;
3.  `fill_metadata(meta)` to fill a `deposits` client with metadata (see
    Metadata vignette for details);
4.  `new_deposit()` to create a new deposit using the metadata inserted
    with `fill_metadata()`;
5.  `delete_deposit(deposit_id)` to delete a nominated deposit;
6.  `upload_file(deposit_id, path)` to upload a local file to a
    specified deposit;
7.  `retrieve_deposit(deposit_id)` to retrieve metadata for a nominated
    deposit; and
8.  `download_file(deposit_id, filename)` to download nominated
    `filename` from specified deposit.

The introductory vignette demonstrates all of these functions, while the
“Metadata” vignette describes processes to specify deposit metadata with
a `deposits` client.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.
