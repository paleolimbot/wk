
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

The goal of wk is to provide lightweight R and C++ infrastsructure for
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
#> 1 wk            122µs    151µs    6337.    73.95KB     7.61
#> 2 geos_c      500.7µs  573.8µs    1680.    50.21KB     2.09
#> 3 sf            396µs  463.1µs    2080.    99.57KB     6.65
#> 4 wkb          53.5ms   53.6ms      18.7    5.23MB    65.4
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
#> 1 wk           9.18ms  10.08ms     98.4     4.54MB     0   
#> 2 geos_c       3.99ms   4.49ms    219.      3.32KB     0   
#> 3 sf         173.76ms 174.87ms      5.67  541.35KB    11.3 
#> 4 wellknown   24.92ms  27.36ms     35.1     3.42MB     3.90
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
#> 1 wk            1.9ms   2.13ms     454.    67.66KB     0   
#> 2 geos_c       2.63ms   2.82ms     345.    49.48KB     0   
#> 3 sf           3.47ms   3.88ms     252.   186.48KB     2.08
#> 4 wellknown   46.79ms  47.87ms      20.6    1.31MB     5.16
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
#> 1 wk          11.32ms  12.22ms     81.1     4.51MB      0  
#> 2 geos_c       6.35ms   6.74ms    146.      3.32KB      0  
#> 3 sf          182.4ms 184.02ms      5.43  250.35KB     10.9
```
