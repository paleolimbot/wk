
#include "wk/geometry-debug-handler.h"
#include "wk/wkb-reader.h"
#include "wk/wkt-reader.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
#include "wk/sexp-reader.h"
using namespace Rcpp;

// [[Rcpp::export]]
void cpp_debug_wkb(List wkb) {
  WKRawVectorListProvider input(wkb);
  WKGeometryDebugHandler handler(Rcout);
  WKBReader reader(input);
  reader.setHandler(&handler);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }
}

void cpp_debug_base(WKReader& reader) {
  WKGeometryDebugHandler handler(Rcout);
  reader.setHandler(&handler);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }
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
  WKSEXPProvider provider(input);
  WKSEXPReader reader(provider);
  cpp_debug_base(reader);
}
