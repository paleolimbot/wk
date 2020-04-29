
#include "wk/geometry-debug-handler.h"
#include "wk/wkb-reader.h"
#include "wk/wkt-reader.h"
#include "wk/io-rcpp.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void cpp_debug_wkb(List wkb) {
  WKRawVectorListReader input(wkb);
  WKGeometryDebugHandler handler(Rcout);
  WKBReader reader(input, handler);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }
}

// [[Rcpp::export]]
void cpp_debug_wkt(CharacterVector input) {
  std::string str_in = as<std::string>(input[0]);
  WKGeometryDebugHandler handler(Rcout);
  WKTReader reader(handler);
  reader.read(str_in);
}
