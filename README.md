
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wk

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/paleolimbot/wk/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/wk/actions)
[![Codecov test
coverage](https://codecov.io/gh/paleolimbot/wk/branch/master/graph/badge.svg)](https://codecov.io/gh/paleolimbot/wk?branch=master)
<!-- badges: end -->

The goal of wk is to provide lightweight R and C++ infrastructure for
packages to use well-known formats (well-known binary and well-known
text) as input and/or output without requiring external software.
Well-known binary is very fast to read and write, whereas well-known
text is human-readable and human-writable. Together, these formats allow
for efficient interchange between software packages (WKB), and highly
readable tests and examples (WKT).

## Installation

You can install the released version of wk from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("wk")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/wk")
```

If you can load the package, you’re good to go\!

``` r
library(wk)
```

## Basic vector classes for WKT and WKB

Use `wkt()` to mark a character vector as containing well-known text, or
`wkb()` to mark a vector as well-known binary. These have some basic
vector features built in, which means you can subset, repeat,
concatenate, and put these objects in a data frame or tibble. These come
with built-in `format()` and `print()` methods.

``` r
wkt("POINT (30 10)")
#> <wk_wkt[1]>
#> [1] POINT (30 10)
as_wkb(wkt("POINT (30 10)"))
#> <wk_wkb[1]>
#> [1] <POINT (30 10)>
```

## Well-known R objects

The wk package experimentally generates (and parses) a plain R object
format, which is needed because well-known binary can’t natively
represent the empty point and reading/writing well-known text is too
slow. The format of the `wksxp()` object is designed to be as close as
possible to well-known text and well-known binary to make the
translation code as clean as possible.

``` r
wkt_translate_wksxp("POINT (30 10)")
#> [[1]]
#>      [,1] [,2]
#> [1,]   30   10
#> attr(,"class")
#> [1] "wk_point"
```

## wkutils

To keep the footprint (i.e., compile time) of this package as slim as
possible, utilities to work with WKT, WKB, and well-known R objects are
located in the [wkutils
package](https://paleolimbot.github.io/wkutils/). One of the main
drawbacks to passing around geometries in WKB is that the format is
opaque to R users, who need coordinates as R objects rather than binary
vectors. The wkutils package provides `wk*_meta()` and `wk*_coords()`
functions (among others) to extract usable coordinates and feature meta.

``` r
wkutils::wkt_coords("POINT ZM (1 2 3 4)")
#> # A tibble: 1 x 7
#>   feature_id part_id ring_id     x     y     z     m
#>        <int>   <int>   <int> <dbl> <dbl> <dbl> <dbl>
#> 1          1       1       0     1     2     3     4
wkutils::wkt_meta("POINT ZM (1 2 3 4)")
#> # A tibble: 1 x 7
#>   feature_id part_id type_id  size  srid has_z has_m
#>        <int>   <int>   <int> <int> <int> <lgl> <lgl>
#> 1          1       1       1     1    NA TRUE  TRUE
wkutils::wkt_ranges("POINT ZM (1 2 3 4)")
#> # A tibble: 1 x 8
#>    xmin  ymin  zmin  mmin  xmax  ymax  zmax  mmax
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1     2     3     4     1     2     3     4
wkutils::coords_point_translate_wkt(1, 2, 3, 4)
#> [1] "POINT ZM (1 2 3 4)"
```
