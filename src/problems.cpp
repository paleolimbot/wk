
#include "wk/geometry-handler.h"
#include "wk/wkb-reader.h"
#include "wk/wkt-streamer.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
#include "wk/rcpp-sexp-reader.h"
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

Rcpp::CharacterVector cpp_problems_base(WKReader& reader) {
  WKValidator validator(reader.nFeatures());
  reader.setHandler(&validator);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return validator.output;
}

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_problems_wkb(Rcpp::List wkb) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_problems_base(reader);
}

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_problems_wkt(CharacterVector wkt) {
  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return cpp_problems_base(reader);
}

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_problems_wksxp(List wksxp) {
  WKSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return cpp_problems_base(reader);
}
