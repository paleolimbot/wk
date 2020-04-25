
#include <sstream>
#include <Rcpp.h>
#include "wkheaders/translator.h"
#include "wkheaders/io-rcpp.h"

using namespace Rcpp;

class RcppWKBWKTTranslator: public WKBWKTTranslator {
public:
  Rcpp::CharacterVector output;
  std::stringstream& stream;

  RcppWKBWKTTranslator(List input, std::stringstream& stream): 
    WKBWKTTranslator(new WKRawVectorListReader(input), stream), 
    output(input.size()), stream(stream) {}

  void nextNull(size_t featureId) {
    output[featureId] = NA_STRING;
  }

  void nextFeature(size_t featureId) {
    stream.str("");
    stream.clear();
    WKBWKTTranslator::nextFeature(featureId);
    output[featureId] = stream.str();
  }
};


// [[Rcpp::export]]
CharacterVector cpp_translate_wkb_wkt(List x) {
  std::stringstream stream;
  RcppWKBWKTTranslator iter(x, stream);

  while (iter.hasNextFeature()) {
    iter.iterate();
  }

  return iter.output;
}
