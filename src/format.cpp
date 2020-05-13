
#include "wk/formatter.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
#include "wk/sexp-reader.h"
using namespace Rcpp;

Rcpp::CharacterVector cpp_format_base(WKReader& reader, int maxCoords) {
  WKCharacterVectorExporter exporter(reader.nFeatures());
  WKFormatter formatter(exporter, maxCoords);
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
  WKSEXPProvider provider(wksxp);
  WKSEXPReader reader(provider);
  return cpp_format_base(reader, maxCoords);
}
