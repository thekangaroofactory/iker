# iker

Iker is a data scribe.\
In ancient civilizations (such as Egypt), a scribe used to be a professional copyist who hand-wrote manuscripts and official documents.

The purpose of the package is to externalize data reading and writing so that applications do not have to manage the I/O layer.

It offers general connectors (file, API) as well as specific implementations (Supabase, GitHub).

<!-- badges: start -->

[![R-CMD-check](https://github.com/thekangaroofactory/iker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thekangaroofactory/iker/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/thekangaroofactory/iker/graph/badge.svg)](https://app.codecov.io/gh/thekangaroofactory/iker) [![R-universe](https://thekangaroofactory.r-universe.dev/iker/badges/version)](https://thekangaroofactory.r-universe.dev/iker/badges/version)

<!-- badges: end -->

## Installation

Install the package as follows:

### From R-universe

``` r
install.packages('iker', repos = c('https://thekangaroofactory.r-universe.dev', 'https://cloud.r-project.org'))
```

### From GitHub

``` r
pak::pkg_install("thekangaroofactory/iker")
```
