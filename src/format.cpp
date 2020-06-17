
#include "wk/geometry-formatter.hpp"

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
#include "wk/rcpp-sexp-reader.hpp"
using namespace Rcpp;

Rcpp::CharacterVector cpp_format_base(WKReader& reader, int maxCoords) {
  WKCharacterVectorExporter exporter(reader.nFeatures());
  WKGeometryFormatter formatter(exporter, maxCoords);
  reader.setHandler(&formatter);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return exporter.output;
}

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_format_wkb(Rcpp::List wkb, int maxCoords) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_format_base(reader, maxCoords);
}

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_format_wkt(CharacterVector wkt, int maxCoords) {
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return cpp_format_base(reader, maxCoords);
}

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_format_wksxp(List wksxp, int maxCoords) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return cpp_format_base(reader, maxCoords);
}
