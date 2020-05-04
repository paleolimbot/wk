
#include <Rcpp.h>
#include "wk/io-rcpp.h"
#include "wk/geometry-handler.h"
#include "wk/wkb-reader.h"
#include "wk/wkt-streamer.h"
using namespace Rcpp;

class WKValidator: public WKGeometryHandler {
public:
  CharacterVector output;

  WKValidator(size_t size): output(size) {}

  virtual bool nextError(WKParseException& error, size_t featureId) {
    this->output[featureId] = error.what();
    return true;
  }

  virtual void nextFeatureEnd(size_t featureId) {
    this->output[featureId] = NA_STRING;
  }
};

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_problems_wkb(Rcpp::List wkb) {
  WKValidator validator(wkb.size());

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  reader.setHandler(&validator);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return validator.output;
}

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_problems_wkt(CharacterVector wkt) {
  WKValidator validator(wkt.size());

  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  reader.setHandler(&validator);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return validator.output;
}
