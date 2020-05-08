
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

## Extract coordinates and meta information

One of the main drawbacks to passing around geomtries in WKB is that the
format is opaque to R users, who need coordinates as R object rather
than binary vectors. In addition to `print()` and `plot()` methods for
`wkb()` vectors, the `wk*_meta()` and `wk*_coords()` functions provide
usable coordinates and feature meta.

``` r
wkt_coords("POINT ZM (1 2 3 4)")
#>   feature_id part_id ring_id x y z m
#> 1          1       1       0 1 2 3 4
wkt_meta("POINT ZM (1 2 3 4)")
#>   feature_id part_id type_id size srid has_z has_m n_coords
#> 1          1       1       1    1   NA  TRUE  TRUE        1
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

This package was designed to stand alone and be flexible, but also
happens to be really fast for some common operations.

Read WKB + Write WKB:

``` r
bench::mark(
  wk = wk:::wksxp_translate_wkb(wk:::wkb_translate_wksxp(nc_wkb)),
  geos_c = geovctrs:::geovctrs_cpp_convert(nc_wkb, wkb_ptype),
  sf = sf:::CPL_read_wkb(sf:::CPL_write_wkb(nc_sfc, EWKB = TRUE), EWKB = TRUE),
  check = FALSE
)
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk            273µs    333µs     2966.   114.2KB    18.1 
#> 2 geos_c        498µs    551µs     1780.    53.5KB     2.03
#> 3 sf            370µs    413µs     2338.    99.8KB    16.6
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
#> 1 wk           3.03ms   3.22ms    308.      3.32KB     0   
#> 2 geos_c          4ms   4.36ms    228.      3.32KB     0   
#> 3 sf         180.12ms  184.4ms      5.28  569.81KB    21.1 
#> 4 wellknown   25.42ms  28.76ms     33.4     3.41MB     5.89
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
#> 1 wk           1.89ms   2.02ms     492.    53.58KB     0   
#> 2 geos_c        2.5ms   2.73ms     362.    49.48KB     0   
#> 3 sf           3.32ms   3.58ms     274.   186.48KB     6.41
#> 4 wellknown   42.73ms  45.88ms      21.9    1.31MB    12.5
```

Read WKT + Write WKT:

``` r
bench::mark(
  wk = wk::wksxp_translate_wkt(wk::wkt_translate_wksxp(nc_wkt)),
  geos_c = geovctrs:::geovctrs_cpp_convert(nc_wkt, wkt_ptype),
  sf = sf:::st_as_text.sfc(sf:::st_as_sfc.character(nc_wkt)),
  check = FALSE
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk           5.03ms   5.49ms    178.     63.77KB      0  
#> 2 geos_c       5.99ms   6.34ms    156.      3.32KB      0  
#> 3 sf         188.41ms 190.96ms      5.23  230.73KB     20.9
```

Generate coordinates:

``` r
bench::mark(
  wk_wkb = wk::wksxp_coords(nc_sxp),
  sfheaders = sfheaders::sfc_to_df(nc_sfc),
  sf = sf::st_coordinates(nc_sfc),
  check = FALSE
)
#> # A tibble: 3 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk_wkb     160.84µs  185.1µs     5010.     131KB     29.9
#> 2 sfheaders  502.16µs 544.86µs     1772.     612KB     56.1
#> 3 sf           2.13ms   2.32ms      426.     606KB     33.7
```

Send polygons to a graphics device (note that the graphics device is the
main holdup in real life):

``` r
devoid::void_dev()
wksxp_plot_new(nc_sxp)

bench::mark(
  wk_wkb = wk::wksxp_draw_polypath(nc_sxp),
  sf = sf:::plot.sfc_MULTIPOLYGON(nc_sfc, add = TRUE),
  check = FALSE
)
#> # A tibble: 2 x 6
#>   expression      min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 wk_wkb      312.7µs  336.4µs     2769.     358KB     18.2
#> 2 sf           3.25ms   3.44ms      283.     241KB     20.7
dev.off()
#> quartz_off_screen 
#>                 2
```
