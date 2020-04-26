
#include <sstream>
#include <Rcpp.h>
#include "wkheaders/translator.h"
#include "wkheaders/wkb-wkb-translator.h"
#include "wkheaders/io-rcpp.h"

using namespace Rcpp;

class RcppWKBWKTTranslator: public WKBWKTTranslator {
public:
  Rcpp::CharacterVector output;
  std::stringstream stream;
  bool nextIsNull;

  RcppWKBWKTTranslator(Rcpp::List input): 
    WKBWKTTranslator(new WKRawVectorListReader(input), stream), output(input.size()) {
    // set default formatting
    this->ensureClassicLocale();
    this->setRoundingPrecision(16);
    this->setTrim(true);
  }

  void nextFeature(size_t featureId) {
    this->stream.str("");
    this->stream.clear();
    this->nextIsNull = false;
    WKBWKTTranslator::nextFeature(featureId);
    if (this->nextIsNull) {
      this->output[featureId] = NA_STRING;
    } else {
      this->output[featureId] = stream.str();
    }
  }

  void nextNull(size_t featureId) {
    this->nextIsNull = true;
  }
};

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_translate_wkb_wkt(Rcpp::List wkb, int precision, bool trim) {
  RcppWKBWKTTranslator iter(wkb);
  iter.setRoundingPrecision(precision);
  iter.setTrim(trim);
  
  while (iter.hasNextFeature()) {
    iter.iterateFeature();
  }

  return iter.output;
}



class RcppWKBWKBTranslator: public WKBWKBTranslator {
public:
  RcppWKBWKBTranslator(Rcpp::List input, WKRawVectorListWriter& writer): 
    WKBWKBTranslator(new WKRawVectorListReader(input), new WKBWriter(writer)) {

  }
};

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wkb(Rcpp::List wkb, int endian, int bufferSize) {
  WKRawVectorListWriter writer(wkb.size());
  writer.setBufferSize(bufferSize);
  RcppWKBWKBTranslator iter(wkb, writer);
  iter.setEndian(endian);

  while (iter.hasNextFeature()) {
    iter.iterateFeature();
  }

  return writer.output;
}

