
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
  WKRawVectorListReader reader(wkb);
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
and parse errors.

``` cpp
class WKGeometryHandler {
public:

  virtual void nextFeatureStart(size_t featureId) {

  }

  virtual void nextFeatureEnd(size_t featureId) {

  }

  virtual void nextNull(size_t featureId) {

  }

  virtual void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {

  }

  virtual void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {

  }

  virtual void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {

  }

  virtual void nextLinearRingEnd(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {

  }

  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {

  }

  virtual bool nextError(WKParseException& error, size_t featureId) {
    return false;
  }

};
```

## Work in progress

This package is under heavy construction and may change at any time.
