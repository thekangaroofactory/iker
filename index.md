# iker

Iker is a data scribe.  
In ancient civilizations (such as Egypt), a scribe used to be a
professional copyist who hand-wrote manuscripts and official documents.

The purpose of the package is to externalize data reading and writing so
that applications do not have to manage the I/O layer.

It offers general connectors (file, API) as well as specific
implementations (Supabase, GitHub).

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
