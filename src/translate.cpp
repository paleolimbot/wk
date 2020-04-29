
#include <sstream>
#include <Rcpp.h>
#include "wk/wkt-writer.h"
#include "wk/wkb-wkb-translator.h"
#include "wk/io-rcpp.h"

using namespace Rcpp;

class RcppWKBWKTWriter: public WKTWriter {
public:
  Rcpp::CharacterVector output;
  std::stringstream stream;
  bool nextIsNull;

  RcppWKBWKTWriter(WKBytesProvider& reader):
    WKTWriter(reader, stream), output(reader.nFeatures()) {
    // set default formatting
    this->ensureClassicLocale();
    this->setRoundingPrecision(16);
    this->setTrim(true);
  }

  void nextFeatureStart(size_t featureId) {
    this->stream.str("");
    this->stream.clear();
    this->nextIsNull = false;
  }

  void nextFeatureEnd(size_t featureId) {
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
  WKRawVectorListProvider reader(wkb);
  RcppWKBWKTWriter translator(reader);

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
  WKRawVectorListProvider reader(wkb);
  WKRawVectorListExporter writer(wkb.size());
  writer.setBufferSize(bufferSize);

  WKBWKBWriter translator(reader, writer);

  translator.setIncludeZ(includeZ);
  translator.setIncludeM(includeM);
  translator.setIncludeSRID(includeSRID);
  translator.setEndian(endian);

  while (translator.hasNextFeature()) {
    translator.iterateFeature();
  }

  return writer.output;
}
