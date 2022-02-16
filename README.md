# deposits

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/deposits/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/deposits/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/mpadge/deposits/branch/main/graph/badge.svg)](https://codecov.io/gh/mpadge/deposits)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

The `deposits` package aims to be a universal client for depositing
research data anywhere. It is not currently in a useable state.

## Installation

The package can be installed with the following command:

``` r
remotes::install_github ("mpadge/deposits")
```

and then loaded for use with:

``` r
library (deposits)
```

## Data Repositories

The list of data repositories currently supported is accessible by the
`deposits_services()` function:

``` r
deposits_services ()
```

    ##       name                           docs                 api_base_url
    ## 1   zenodo https://developers.zenodo.org/      https://zenodo.org/api/
    ## 2 figshare     https://docs.figshare.com/ https://api.figshare.com/v2/

## API Keys

All services require API (“Application Programming Interface”) keys to
be stored as local environment variables the names of which must include
the names of the respective services, as defined by the “name” column
returned from `deposits_service()`, as shown above. This can be done as
in the following example:

``` r
Sys.setenv ("ZENODO_TOKEN" = "<my-token")
```

Alternatively, these tokens can be stored in a `~/.Renviron` file, where
they will be automatically loaded into every R session.

## Functionality

The only functionality currently provided is authentication of the
locally stored token, demonstrated by the following code:

``` r
services <- deposits_services ()
for (s in services$name) {
    cli <- depositsClient$new(
      name = s
    )
    message (s, ": ", cli$ping()
}
```

    ## zenodo: TRUE

    ## figshare: TRUE
