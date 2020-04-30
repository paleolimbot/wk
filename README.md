
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wk

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/paleolimbot/wk/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/wk/actions)
<!-- badges: end -->

The goal of wk is to provide lightweight R and C++ infrastsructure for
packages to use well-known formats (well-known binary and well-known
text) as input and/or output without requiring external software.

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

## Translate between WKB and WKT

This package mostly provides C++ headers to comitting to any particular
in-memory format, but it contains enough R code to test that the C++
headers will actually work. As such, you can translate between
well-known binary (a `list()` of `raw()` vectors) and well-known text
(character vector):

``` r
point_wkb <- as.raw(
  c(0x00, # 0x00 = big endian
    0x00, 0x00, 0x00, 0x01, # 0x00000001 = POINT
    0x40, 0x3e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, # x coordinate (double)
    0x40, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  # y coordinate (double)
  )
)

wkb_translate_wkt(list(point_wkb))
#> [1] "POINT (30 10)"
```

## Validate WKB and WKT

It is often useful to attempt parsing a geometry to see if it will
succeed. The “problems” functions parse well-known geometry vectors,
returning a character vector of parsing problems (or `NA` if there were
none).

``` r
wkb_problems(list(point_wkb))
#> [1] NA

point_bad_type <- point_wkb
point_bad_type[5] <- as.raw(0xff)
wkb_problems(list(point_bad_type))
#> [1] "Unrecognized geometry type in WKBReader::readGeometry(): 255"
```

## Using the C++ headers

The wk package takes an event-based approach to parsing inspired by the
event-based SAX XML parser. This allows the package to avoid inventing
an in-memory format, since it never has to store geometries in memory.
This is class-based, so you will have to make your own subclass of
`WKGeometryHandler` and wire it up to a `WKReader` to do anything
useful.

``` cpp
// If you're writing code in a package, you'll also
// have to put 'wk' in your `LinkingTo:` description field
// [[Rcpp::depends(wk)]]

#include <Rcpp.h>
#include "wk/io-rcpp.h"
#include "wk/wkb-reader.h"
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
void wkb_read_custom(List wkb) {
  WKRawVectorListProvider reader(wkb);
  CustomHandler handler;
  WKBReader wkbReader(reader, handler);
  
  while (wkbReader.hasNextFeature()) {
    wkbReader.iterateFeature();
  }
}
```

On our example point, this prints the following:

``` r
wkb_read_custom(list(point_wkb))
#> Do something before feature 0
#> Do something after feature 0
```

The full handler interface includes methods for the start and end of
features, geometries (which may be nested), linear rings, coordinates,
and parse errors. You can preview what will get called for a given
geometry using `wkb|wkt_debug()` functions.

``` r
wkb_debug(list(point_wkb))
#> nextFeatureStart(0)
#>     nextGeometryStart(POINT [1], WKReader::PART_ID_NONE)
#>         nextCoordinate(POINT [1], WKCoord(x = 30, y = 10), 0)
#>     nextGeometryEnd(POINT [1], WKReader::PART_ID_NONE)
#> nextFeatureEnd(0)
```

## Performance

This package is mostly designed for no system dependencies and
flexibility, but also happens to be reasonably fast.

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
#> 1 wk            125µs  148.9µs    6296.    73.95KB     7.61
#> 2 geos_c      523.1µs  578.8µs    1642.    50.21KB     2.09
#> 3 sf          401.2µs  469.6µs    2030.    99.57KB     6.70
#> 4 wkb          54.7ms   55.3ms      18.1    5.23MB    63.3
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
#> 1 wk            9.8ms  10.32ms     95.4     4.54MB     0   
#> 2 geos_c       4.26ms   4.58ms    213.      3.32KB     0   
#> 3 sf         182.61ms 184.05ms      5.41  541.35KB    10.8 
#> 4 wellknown   26.96ms  28.27ms     33.9     3.42MB     1.99
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
#> 1 wk           2.04ms   2.25ms     434.    67.66KB     0   
#> 2 geos_c       2.66ms    2.9ms     329.    49.48KB     0   
#> 3 sf           3.78ms   4.83ms     207.   186.48KB     4.26
#> 4 wellknown   48.89ms  51.24ms      19.6    1.31MB     2.18
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
#> 1 wk          11.82ms  12.88ms     77.2     4.51MB     0   
#> 2 geos_c       6.37ms   6.91ms    143.      3.32KB     0   
#> 3 sf         196.01ms 203.06ms      4.86  249.56KB     8.10
```
