
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rreptiledb

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/matthewlewis896/rreptiledb.svg?branch=master)](https://travis-ci.com/matthewlewis896/rreptiledb)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/matthewlewis896/rreptiledb?branch=master&svg=true)](https://ci.appveyor.com/project/matthewlewis896/rreptiledb)
[![CRAN
status](https://www.r-pkg.org/badges/version/rreptiledb)](https://CRAN.R-project.org/package=rreptiledb)
<!-- badges: end -->

rreptiledb allows you to search and download data hosted on the [Reptile
Database](\url%7Bhttp://www.reptile-database.org/%7D) through R.

## Installation

Install from [GitHub](https://github.com/matthewlewis896/rreptiledb)
with:

``` r
pak::pkg_install("matthewlewis896/rreptiledb")
```

## Use

Load the package using:

``` r
library(rreptiledb)
```

Download the latest species checklist on the reptile database with:

``` r
sp_list <- rd_species()
```

Fetch data for any species using `rd_fetch()`:

``` r
sp <- rd_fetch(binomial = "Boa constrictor")
```

This returns a `list` with named items as they appear on the Reptile
Database. For instance:

``` r
print(sp$Higher_Taxa)
[1] "Boidae (Boinae), Henophidia, Alethinophidia, Serpentes, Squamata (snakes)"

print(sp$Subspecies)
[1] "Boa constrictor constrictor LINNAEUS 1758"     "Boa constrictor longicauda PRICE & RUSSO 1991"
[3] "Boa constrictor occidentalis PHILIPPI 1873"    "Boa constrictor ortonii COPE 1877" 
```
