
#include <sstream>
#include "wk/wkt-writer.h"
#include "wk/wkt-reader.h"
#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"
#include "wk/sexp-writer.h"

#include <Rcpp.h>
#include "wk/io-rcpp.h"
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_translate_wkb_wkt(Rcpp::List wkb, int includeZ, int includeM,
                                            int includeSRID, int precision, bool trim) {
  WKRawVectorListProvider provider(wkb);
  WKCharacterVectorExporter exporter(provider.nFeatures());

  WKTWriter writer(exporter);
  WKBReader reader(provider, writer);

  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wkb(Rcpp::List wkb, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKRawVectorListProvider provider(wkb);
  WKRawVectorListExporter exporter(provider.nFeatures());

  WKBWriter writer(exporter);
  WKBReader reader(provider, writer);

  exporter.setBufferSize(bufferSize);
  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);
  writer.setEndian(endian);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}

// [[Rcpp::export]]
CharacterVector cpp_translate_wkt_wkt(CharacterVector wkt, int includeZ, int includeM,
                                      int includeSRID, int precision, bool trim) {

  WKCharacterVectorProvider provider(wkt);
  WKCharacterVectorExporter exporter(provider.nFeatures());

  WKTWriter writer(exporter);
  WKTStreamer reader(provider, writer);

  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkt_wkb(CharacterVector wkt, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKCharacterVectorProvider provider(wkt);
  WKRawVectorListExporter exporter(provider.nFeatures());

  WKBWriter writer(exporter);
  WKTReader reader(provider, writer);

  exporter.setBufferSize(bufferSize);
  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);
  writer.setEndian(endian);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}


// [[Rcpp::export]]
Rcpp::List cpp_translate_wkt_wk_sexp(CharacterVector wkt, int includeZ, int includeM,
                                     int includeSRID) {

  WKCharacterVectorProvider provider(wkt);
  WKSEXPExporter exporter(provider.nFeatures());

  WKSEXPWriter writer(exporter);
  WKTReader reader(provider, writer);

  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}
