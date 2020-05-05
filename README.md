
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

Want to return an object that any spatial package can read? Use `wkt()`
to mark a character vector as containing well-known text, or `wkb()` to
mark a vector as well-known binary. These have some basic vector
features built in, which means you can subset, repeat, concatenate, and
put these objects in a data frame or tibble. These come with built-in
`format()` and `print()` methods.

``` r
wkt("POINT (30 10)")
#> <wk_wkt[1]>
#> [1] POINT (30 10)
as_wkb(wkt("POINT (30 10)"))
#> <wk_wkb[1]>
#> [1] 01 01 00 00 00
```

## Extract coordinates and meta information

One of the main drawbacks to passing around geomtries in WKB is that the
format is opaque to R users, who need coordinates as R object rather
than binary vectors. In addition to `print()` and `plot()` methods for
`wkb()` vectors, the `wk*_meta()` and `wk*_coords()` functions provide
usable coordinates and feature meta.

``` r
wkt_coords("POINT ZM (1 2 3 4)")
#>   feature_id nest_id part_id ring_id coord_id x y z m
#> 1          1       0      NA      NA        1 1 2 3 4
wkt_meta("POINT ZM (1 2 3 4)")
#>   feature_id nest_id part_id type_id size srid has_z has_m
#> 1          1       0      NA       1    1   NA  TRUE  TRUE
```

## Well-known R objects?

The wk package experimentally generates (and parses) well-known “s”
expressions (the C name for R objects). This is similar to the format
that [sf](https://r-spatial.github.io/sf) uses.

``` r
wkt_translate_wksxp("POINT (30 10)")
#> [[1]]
#>      [,1] [,2]
#> [1,]   30   10
#> attr(,"class")
#> [1] "wk_point"
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
#include "wk/rcpp-io.h"
#include "wk/wkt-reader.h"
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
geometry using `wkb|wkt_debug()` functions.

``` r
wkt_debug("POINT (30 10)")
#> nextFeatureStart(0)
#>     nextGeometryStart(POINT [1], WKReader::PART_ID_NONE)
#>         nextCoordinate(POINT [1], WKCoord(x = 30, y = 10), 0)
#>     nextGeometryEnd(POINT [1], WKReader::PART_ID_NONE)
#> nextFeatureEnd(0)
```

## Performance

This package is mostly designed for no system dependencies and
flexibility, but also happens to be really fast for some common
operations.

Read WKB + Write WKB:

``` r
bench::mark(
  wk = wk:::wkb_translate_wkb(nc_wkb),
  geos_c = geovctrs:::geovctrs_cpp_convert(nc_wkb, wkb_ptype),
  sf = sf:::CPL_read_wkb(sf:::CPL_write_wkb(nc_sfc, EWKB = TRUE), EWKB = TRUE),
  wkb = wkb::readWKB(wkb::writeWKB(nc_sp)),
  check = FALSE
)
#> # A tibble: 4 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk          122.9µs  146.8µs    6501.    68.43KB     6.64
#> 2 geos_c      525.2µs  575.6µs    1682.    50.21KB     0   
#> 3 sf          386.6µs  440.8µs    2218.    99.57KB     9.21
#> 4 wkb          52.5ms   52.5ms      18.8    5.23MB    44.0
```

Read WKB + Write WKT:

``` r
bench::mark(
  wk = wk:::wkb_translate_wkt(nc_wkb),
  geos_c = geovctrs:::geovctrs_cpp_convert(nc_wkb, wkt_ptype),
  sf = sf:::st_as_text.sfc(sf:::st_as_sfc.WKB(nc_WKB, EWKB = TRUE)),
  wellknown = lapply(nc_wkb, wellknown::wkb_wkt),
  check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 4 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk           9.34ms   9.95ms     99.5     4.56MB     0   
#> 2 geos_c        3.9ms   4.37ms    225.      3.32KB     0   
#> 3 sf         172.16ms 173.37ms      5.73  541.35KB    11.5 
#> 4 wellknown   24.72ms  26.73ms     37.1     3.42MB     1.95
```

Read WKT + Write WKB:

``` r
bench::mark(
  wk = wk:::wkt_translate_wkb(nc_wkt),
  geos_c = geovctrs:::geovctrs_cpp_convert(nc_wkt, wkb_ptype),
  sf = sf:::CPL_write_wkb(sf:::st_as_sfc.character(nc_wkt), EWKB = TRUE),
  wellknown = lapply(nc_wkt, wellknown::wkt_wkb),
  check = FALSE
)
#> # A tibble: 4 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk           1.96ms   2.15ms     456.    53.58KB     0   
#> 2 geos_c       2.44ms   2.75ms     359.    49.48KB     2.11
#> 3 sf           3.31ms   3.71ms     263.   186.48KB     2.05
#> 4 wellknown   46.55ms  47.98ms      20.7    1.31MB     4.61
```

Read WKT + Write WKT:

``` r
bench::mark(
  wk = wk:::wkt_translate_wkt(nc_wkt),
  geos_c = geovctrs:::geovctrs_cpp_convert(nc_wkt, wkt_ptype),
  sf = sf:::st_as_text.sfc(sf:::st_as_sfc.character(nc_wkt)),
  check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk          11.11ms  12.25ms     81.8     4.51MB      0  
#> 2 geos_c       6.08ms   6.55ms    150.      3.32KB      0  
#> 3 sf         178.97ms 180.83ms      5.51  260.61KB     11.0
```

Generate coordinates:

``` r
bench::mark(
  wk_wkb = wk::wkb_coords(rep(nc_wkb, 10)),
  sfheaders = sfheaders::sfc_to_df(rep(nc_sfc, 10)),
  sf = sf::st_coordinates(rep(nc_sfc, 10)),
  check = FALSE
)
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk_wkb       1.82ms   2.35ms     424.     1.38MB     20.6
#> 2 sfheaders   12.31ms  13.14ms      74.5    5.44MB     20.9
#> 3 sf           29.6ms     30ms      32.8    5.61MB     19.7
```
