#include <Rcpp.h>
#include "wkheaders/translator.h"
#include "wkheaders/io-rcpp.h"
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector cpp_translate_wkb_wkt(List x) {
  CharacterVector out(x.size());
  WKBWKTTranslator iter(new WKRawVectorListReader(x));

  R_xlen_t i = 0;
  while (iter.hasNextFeature()) {
    out[i] = iter.translateFeature();
    i++;
  }

  return out;
}
