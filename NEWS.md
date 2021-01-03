# wk (development version)

* Removed `wksxp()` in favour of improved `sfc()` support 
  (#21).

# wk 0.3.3

* Fixed WKB import of ZM geometries that do not use EWKB.
* Added `xy()`, `xyz()`, `xym()` and `xyzm()` classes
  to efficiently store point geometries.
* Added the `rct()` vector class to efficiently store
  two-dimensional rectangles.
* Fixed the CRAN check  failure caused by a circular
  dependency with  the wkutils package.
* Added S3 methods to coerce sf objects to and from
  `wkt()`, `wkb()` and `wksxp()`.

# wk 0.3.2

* Fixed EWKB output for collections and multi-geometries
  that included SRID (#3).
* Fixed CRAN check errors related to exception handling on
  MacOS/R 3.6.2.

# wk 0.3.1

* Added a `NEWS.md` file to track changes to the package.
