
#include <sstream>
#include <Rcpp.h>
#include "wk/wkb-wkt-translator.h"
#include "wk/wkb-wkb-translator.h"
#include "wk/io-rcpp.h"

using namespace Rcpp;

class RcppWKBWKTTranslator: public WKBWKTTranslator {
public:
  Rcpp::CharacterVector output;
  std::stringstream stream;
  bool nextIsNull;

  RcppWKBWKTTranslator(BinaryReader& reader):
    WKBWKTTranslator(reader, stream), output(reader.nFeatures()) {
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
Rcpp::CharacterVector cpp_translate_wkb_wkt(Rcpp::List wkb, int includeZ, int includeM,
                                            int includeSRID, int precision, bool trim) {
  WKRawVectorListReader reader(wkb);
  RcppWKBWKTTranslator translator(reader);

  translator.setIncludeZ(includeZ);
  translator.setIncludeM(includeM);
  translator.setIncludeSRID(includeSRID);
  translator.setRoundingPrecision(precision);
  translator.setTrim(trim);

  while (translator.hasNextFeature()) {
    translator.iterateFeature();
  }

  return translator.output;
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wkb(Rcpp::List wkb, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {
  WKRawVectorListReader reader(wkb);
  WKRawVectorListWriter writer(wkb.size());
  writer.setBufferSize(bufferSize);

  WKBWKBTranslator translator(reader, writer);

  translator.setIncludeZ(includeZ);
  translator.setIncludeM(includeM);
  translator.setIncludeSRID(includeSRID);
  translator.setEndian(endian);

  while (translator.hasNextFeature()) {
    translator.iterateFeature();
  }

  return writer.output;
}