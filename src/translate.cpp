
#include <sstream>
#include "wk/wkt-writer.h"
#include "wk/wkt-reader.h"
#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
#include "wk/sexp-writer.h"
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_translate_wkb_wkt(Rcpp::List wkb, int includeZ, int includeM,
                                            int includeSRID, int precision, bool trim) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);

  WKCharacterVectorExporter exporter(provider.nFeatures());
  WKTWriter writer(exporter);
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);

  reader.setHandler(&writer);

  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wkb(Rcpp::List wkb, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);

  WKRawVectorListExporter exporter(provider.nFeatures());
  exporter.setBufferSize(bufferSize);
  WKBWriter writer(exporter);

  reader.setHandler(&writer);

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
  WKTStreamer reader(provider);

  WKCharacterVectorExporter exporter(provider.nFeatures());
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);
  WKTWriter writer(exporter);

  reader.setHandler(&writer);

  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkt_wkb(CharacterVector wkt, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);

  WKRawVectorListExporter exporter(provider.nFeatures());
  exporter.setBufferSize(bufferSize);
  WKBWriter writer(exporter);
  writer.setEndian(endian);

  reader.setHandler(&writer);

  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}


// [[Rcpp::export]]
Rcpp::List cpp_translate_wkt_wksexp(CharacterVector wkt, int includeZ, int includeM,
                                     int includeSRID) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);

  WKSEXPExporter exporter(provider.nFeatures());
  WKSEXPWriter writer(exporter);

  reader.setHandler(&writer);

  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }

  return exporter.output;
}
