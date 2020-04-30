
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

## Translate between WKB and WKT

This package mostly provides C++ headers to avoid committing to any
particular in-memory format, but it contains enough R code to test that
the C++ headers will actually work. As such, you can translate between
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
#> 1 wk          116.4µs  147.8µs    6522.    73.92KB     7.56
#> 2 geos_c      508.6µs  574.9µs    1675.    50.21KB     2.09
#> 3 sf          403.9µs  493.7µs    1872.    99.57KB     6.72
#> 4 wkb          54.7ms   56.5ms      17.7    5.23MB    61.9
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
#> 1 wk           9.13ms     10ms     99.3     4.54MB     0   
#> 2 geos_c       4.16ms   4.81ms    198.      3.32KB     0   
#> 3 sf         185.69ms 187.88ms      5.11  541.35KB    10.2 
#> 4 wellknown   24.93ms  26.99ms     34.5     3.42MB     1.92
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
#> 1 wk            1.9ms   2.13ms     451.    67.65KB     0   
#> 2 geos_c       2.45ms    2.7ms     362.    49.48KB     0   
#> 3 sf           3.29ms   3.67ms     266.   186.48KB     4.26
#> 4 wellknown   46.47ms  47.17ms      21.1    1.31MB     4.68
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
#> 1 wk          10.86ms  12.05ms     83.1     4.51MB      0  
#> 2 geos_c       6.15ms   6.89ms    135.      3.32KB      0  
#> 3 sf          181.1ms 189.29ms      5.21  248.71KB     10.4
```
