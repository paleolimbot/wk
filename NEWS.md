# wk 0.4.1

* Fix LTO and MacOS 3.6.2 check errors (#61).

# wk 0.4.0

* Removed `wksxp()` in favour of improved `sf::st_sfc()` support 
  (#21).
* Rewrite existing readers, writers, and handlers, using 
  a new C API (#13).
* Use new C API in favour of header-only approach for all
  wk functions (#19, #22).
* Use cpp11 to manage safe use of callables that may longjmp 
  from C++.
* Vector classes now propagate `attr(, "crs")`, and check
  that operations that involve more than one vector have
  compatable CRS objects as determined by `wk_crs_equal()`.
* Added an R-level framework for other packages to implement
  wk readers and handlers: `wk_handle()`, `wk_translate()`,
  and `wk_writer()` (#37).
* Added a native reader and writer for `sf::st_sfc()` objects
  and implemented R-level generics for sfc, sfg, sf, and bbox
  objects (#28, #29, #38, #45).
* Implement `crc()` vector class to represent circles (#40).
* Added a 2D cartesian bounding box handler (`wk_bbox()`) (#42).
* Refactored unit tests reflecting use of the new API and
  for improved test coverage (#44, #45, #46).
* Added `wk_meta()`, `wk_vector_meta()`, and `wk_count()` to 
  inspect properties of vectors (#53).
* Modified all internal handlers such that they work with vectors
  of unknown length (#54).

# wk 0.3.4

* Fixed reference to `wkutils::plot.wk_wksxp()`, which
  no longer exists.

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
