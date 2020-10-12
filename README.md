
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
located in the [wkutils package](https://paleolimbot.github.io/wkutils).
One of the main drawbacks to passing around geometries in WKB is that
the format is opaque to R users, who need coordinates as R objects
rather than binary vectors. The wkutils package provides `wk*_meta()`
and `wk*_coords()` functions (among others) to extract usable
coordinates and feature meta.

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

## Dependencies

The wk package imports [Rcpp](https://cran.r-project.org/package=Rcpp).

## Using the C++ headers

The wk package takes an event-based approach to parsing inspired by the
event-based SAX XML parser. This makes the readers and writers highly
re-usable\! This system is class-based, so you will have to make your
own subclass of `WKGeometryHandler` and wire it up to a `WKReader` to do
anything useful.

``` cpp
// If you're writing code in a package, you'll also
// have to put 'wk' in your `LinkingTo:` description field
// [[Rcpp::depends(wk)]]

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
#include "wk/wkt-reader.hpp"
using namespace Rcpp;

class CustomHandler: public WKGeometryHandler {
public:
  
  void nextFeatureStart(size_t featureId) {
    Rcout << "Do something before feature " << featureId << "\n";
  }
  
  void nextFeatureEnd(size_t featureId) {
    Rcout << "Do something after feature " << featureId << "\n";
  }
};

// [[Rcpp::export]]
void wkt_read_custom(CharacterVector wkt) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  
  CustomHandler handler;
  reader.setHandler(&handler);
  
  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }
}
```

On our example point, this prints the following:

``` r
wkt_read_custom("POINT (30 10)")
#> Do something before feature 0
#> Do something after feature 0
```

The full handler interface includes methods for the start and end of
features, geometries (which may be nested), linear rings, coordinates,
and parse errors. You can preview what will get called for a given
geometry using `wkutils::wkb|wkt_debug()` functions.

``` r
library(wkutils) # remotes::install_github("paleolimbot/wkutils")
#> Warning: package 'wkutils' was built under R version 4.0.2
wkt_debug("POINT (30 10)")
#> nextFeatureStart(0)
#>     nextGeometryStart(POINT [1], WKReader::PART_ID_NONE)
#>         nextCoordinate(POINT [1], WKCoord(x = 30, y = 10), 0)
#>     nextGeometryEnd(POINT [1], WKReader::PART_ID_NONE)
#> nextFeatureEnd(0)
```

## Performance

This package was designed to stand alone and be flexible, but also
happens to be really fast for some common operations.

Read WKB + Write WKB:

``` r
bench::mark(
  wk = wk:::wksxp_translate_wkb(wk:::wkb_translate_wksxp(nc_wkb)),
  sf = sf:::CPL_read_wkb(sf:::CPL_write_wkb(nc_sfc, EWKB = TRUE), EWKB = TRUE),
  check = FALSE
)
#> # A tibble: 2 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk            252µs    313µs     2958.   110.1KB     18.8
#> 2 sf            420µs    504µs     1500.    99.1KB     12.5
```

Read WKB + Write WKT:

``` r
bench::mark(
  wk = wk:::wkb_translate_wkt(nc_wkb),
  sf = sf:::st_as_text.sfc(sf:::st_as_sfc.WKB(nc_WKB, EWKB = TRUE)),
  check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> # A tibble: 2 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk           3.02ms   4.29ms    214.      3.32KB      0  
#> 2 sf         230.83ms 241.29ms      4.15  566.67KB     19.3
```

Read WKT + Write WKB:

``` r
bench::mark(
  wk = wk:::wkt_translate_wkb(nc_wkt),
  sf = sf:::CPL_write_wkb(sf:::st_as_sfc.character(nc_wkt), EWKB = TRUE),
  check = FALSE
)
#> # A tibble: 2 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk            2.1ms   2.35ms      396.    49.5KB     0   
#> 2 sf           3.42ms   4.45ms      198.   185.7KB     6.60
```

Read WKT + Write WKT:

``` r
bench::mark(
  wk = wk::wksxp_translate_wkt(wk::wkt_translate_wksxp(nc_wkt)),
  sf = sf:::st_as_text.sfc(sf:::st_as_sfc.character(nc_wkt)),
  check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is disabled.
#> # A tibble: 2 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk           5.23ms   7.02ms    137.      63.8KB     1.98
#> 2 sf         235.94ms 253.21ms      4.03   229.8KB    17.5
```
