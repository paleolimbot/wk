
#include <Rcpp.h>
#include "wkheaders/io-rcpp.h"
#include "wkheaders/wkb-iterator.h"
using namespace Rcpp;

class RcppWKBValidator: WKBIterator {
public:
  Rcpp::CharacterVector output;

  RcppWKBValidator(Rcpp::List input): WKBIterator(new WKRawVectorListReader(input)) {
    this->output = CharacterVector(input.size());
  }

  // expose these as the public interface
  virtual bool hasNextFeature() {
    return WKBIterator::hasNextFeature();
  }

  virtual void iterateFeature() {
    WKBIterator::iterateFeature();
  }

  virtual void nextFeature(size_t featureId) {
    try {
      WKBIterator::nextFeature(featureId);
      this->output[featureId] = NA_STRING;
    } catch(std::exception& e) {
      this->output[featureId] = e.what();
    }
  }
};

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_problems_wkb(Rcpp::List wkb) {
  RcppWKBValidator validator(wkb);

  while (validator.hasNextFeature()) {
    validator.iterateFeature();
  }

  return validator.output;
}
