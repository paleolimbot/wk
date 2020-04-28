
#include <Rcpp.h>
#include "wk/io-rcpp.h"
#include "wk/wkb-reader.h"
using namespace Rcpp;

class RcppWKBValidator: WKBReader {
public:
  Rcpp::CharacterVector output;

  RcppWKBValidator(WKRawVectorListReader& reader): WKBReader(reader) {
    this->output = CharacterVector(reader.nFeatures());
  }

  // expose these as the public interface
  virtual bool hasNextFeature() {
    return WKBReader::hasNextFeature();
  }

  virtual void iterateFeature() {
    WKBReader::iterateFeature();
  }

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
  WKRawVectorListReader reader(wkb);
  RcppWKBValidator validator(reader);

  while (validator.hasNextFeature()) {
    validator.iterateFeature();
  }

  return validator.output;
}
