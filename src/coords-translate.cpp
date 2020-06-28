
#include <Rcpp.h>
#include "wk/rcpp-translate.hpp"
#include "wk/rcpp-coord-reader.hpp"
using namespace Rcpp;


// [[Rcpp::export]]
CharacterVector cpp_coords_point_translate_wkt(NumericVector x, NumericVector y,
                                               NumericVector z, NumericVector m,
                                               int precision, bool trim) {
  WKRcppPointCoordProvider provider(x, y, z, m);
  WKRcppPointCoordReader reader(provider);
  return wk::rcpp_translate_wkt(reader, precision, trim);
}

// [[Rcpp::export]]
List cpp_coords_point_translate_wkb(NumericVector x, NumericVector y,
                                    NumericVector z, NumericVector m,
                                    int endian, int bufferSize) {
  WKRcppPointCoordProvider provider(x, y, z, m);
  WKRcppPointCoordReader reader(provider);
  return wk::rcpp_translate_wkb(reader, endian, bufferSize);
}

// [[Rcpp::export]]
List cpp_coords_point_translate_wksxp(NumericVector x, NumericVector y,
                                      NumericVector z, NumericVector m) {
  WKRcppPointCoordProvider provider(x, y, z, m);
  WKRcppPointCoordReader reader(provider);
  return wk::rcpp_translate_wksxp(reader);
}

// [[Rcpp::export]]
CharacterVector cpp_coords_linestring_translate_wkt(NumericVector x, NumericVector y,
                                                    NumericVector z, NumericVector m,
                                                    IntegerVector featureId,
                                                    int precision, bool trim) {
  WKRcppLinestringCoordProvider provider(x, y, z, m, featureId);
  WKRcppLinestringCoordReader reader(provider);
  return wk::rcpp_translate_wkt(reader, precision, trim);
}

// [[Rcpp::export]]
List cpp_coords_linestring_translate_wkb(NumericVector x, NumericVector y,
                                         NumericVector z, NumericVector m,
                                         IntegerVector featureId,
                                         int endian, int bufferSize) {
  WKRcppLinestringCoordProvider provider(x, y, z, m, featureId);
  WKRcppLinestringCoordReader reader(provider);
  return wk::rcpp_translate_wkb(reader, endian, bufferSize);
}

// [[Rcpp::export]]
List cpp_coords_linestring_translate_wksxp(NumericVector x, NumericVector y,
                                           NumericVector z, NumericVector m,
                                           IntegerVector featureId) {
  WKRcppLinestringCoordProvider provider(x, y, z, m, featureId);
  WKRcppLinestringCoordReader reader(provider);
  return wk::rcpp_translate_wksxp(reader);
}

// [[Rcpp::export]]
CharacterVector cpp_coords_polygon_translate_wkt(NumericVector x, NumericVector y,
                                                 NumericVector z, NumericVector m,
                                                 IntegerVector featureId, IntegerVector ringId,
                                                 int precision, bool trim) {
  WKRcppPolygonCoordProvider provider(x, y, z, m, featureId, ringId);
  WKRcppPolygonCoordReader reader(provider);
  return wk::rcpp_translate_wkt(reader, precision, trim);
}

// [[Rcpp::export]]
List cpp_coords_polygon_translate_wkb(NumericVector x, NumericVector y,
                                      NumericVector z, NumericVector m,
                                      IntegerVector featureId, IntegerVector ringId,
                                      int endian, int bufferSize) {
  WKRcppPolygonCoordProvider provider(x, y, z, m, featureId, ringId);
  WKRcppPolygonCoordReader reader(provider);
  return wk::rcpp_translate_wkb(reader, endian, bufferSize);
}

// [[Rcpp::export]]
List cpp_coords_polygon_translate_wksxp(NumericVector x, NumericVector y,
                                        NumericVector z, NumericVector m,
                                        IntegerVector featureId, IntegerVector ringId) {
  WKRcppPolygonCoordProvider provider(x, y, z, m, featureId, ringId);
  WKRcppPolygonCoordReader reader(provider);
  return wk::rcpp_translate_wksxp(reader);
}
