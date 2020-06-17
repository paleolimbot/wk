
#include "wk/geometry-debug-handler.hpp"
#include "wk/wkb-reader.hpp"
#include "wk/wkt-reader.hpp"

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
#include "wk/rcpp-sexp-reader.hpp"
using namespace Rcpp;

void cpp_debug_base(WKReader& reader) {
  WKGeometryDebugHandler handler(Rcout);
  reader.setHandler(&handler);

  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }
}

// [[Rcpp::export]]
void cpp_debug_wkb(List wkb) {
  WKRawVectorListProvider input(wkb);
  WKBReader reader(input);
  cpp_debug_base(reader);
}

// [[Rcpp::export]]
void cpp_debug_wkt(CharacterVector input) {
  WKCharacterVectorProvider provider(input);
  WKTReader reader(provider);
  cpp_debug_base(reader);
}

// [[Rcpp::export]]
void cpp_debug_wkt_streamer(CharacterVector input) {
  WKCharacterVectorProvider provider(input);
  WKTStreamer reader(provider);
  cpp_debug_base(reader);
}

// [[Rcpp::export]]
void cpp_debug_wksxp(List input) {
  WKRcppSEXPProvider provider(input);
  WKRcppSEXPReader reader(provider);
  cpp_debug_base(reader);
}
