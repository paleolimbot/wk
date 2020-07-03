
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

## Extract coordinates and meta information

One of the main drawbacks to passing around geometries in WKB is that
the format is opaque to R users, who need coordinates as R objects
rather than binary vectors. In addition to `print()` methods for `wkb()`
vectors, the `wk*_meta()` and `wk*_coords()` functions provide usable
coordinates and feature meta.

``` r
wkt_coords("POINT ZM (1 2 3 4)")
#>   feature_id part_id ring_id x y z m
#> 1          1       1       0 1 2 3 4
wkt_meta("POINT ZM (1 2 3 4)")
#>   feature_id part_id type_id size srid has_z has_m n_coords
#> 1          1       1       1    1   NA  TRUE  TRUE        1
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
#> 
#> Attaching package: 'wkutils'
#> The following objects are masked from 'package:wk':
#> 
#>     wkb_debug, wksxp_debug, wkt_debug, wkt_streamer_debug
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
#> 1 wk            294µs    346µs     2870.   114.2KB     18.4
#> 2 sf            435µs    477µs     2015.    99.8KB     11.2
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
#> 1 wk           3.02ms   3.43ms    288.      3.32KB      0  
#> 2 sf         203.37ms  208.8ms      4.82  566.66KB     17.7
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
#> 1 wk           1.89ms   2.06ms      475.    53.6KB     0   
#> 2 sf           3.44ms   3.97ms      246.   185.7KB     4.21
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
#> 1 wk           5.03ms   5.83ms    173.      63.8KB      0  
#> 2 sf         206.91ms 212.52ms      4.71   235.7KB     17.3
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
#> 1 wk_wkb     183.32µs  211.7µs     4490.     131KB     22.5
#> 2 sfheaders  607.04µs  707.8µs     1372.     627KB     39.1
#> 3 sf           2.52ms    2.8ms      353.     507KB     26.2
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
#> 1 wk_wkb     332.59µs 364.17µs     2524.     358KB     15.8
#> 2 sf           3.09ms   3.54ms      271.     243KB     17.9
dev.off()
#> quartz_off_screen 
#>                 2
```
