
#include <sstream>
#include "wk/wkt-writer.h"
#include "wk/wkt-reader.h"
#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
#include "wk/sexp-writer.h"
using namespace Rcpp;

void cpp_translate_base(WKReader& reader, WKWriter& writer,
                        int includeZ, int includeM, int includeSRID) {
  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);

  reader.setHandler(&writer);

  while (reader.hasNextFeature()) {
    reader.iterateFeature();
  }
}

Rcpp::CharacterVector cpp_translate_wkt_base(WKReader& reader,
                                             int includeZ, int includeM, int includeSRID,
                                             int precision, bool trim) {
  WKCharacterVectorExporter exporter(reader.nFeatures());
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);
  WKTWriter writer(exporter);

  cpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

Rcpp::List cpp_translate_wkb_base(WKReader& reader,
                                  int includeZ, int includeM, int includeSRID,
                                  int endian, int bufferSize) {
  WKRawVectorListExporter exporter(reader.nFeatures());
  exporter.setBufferSize(bufferSize);
  WKBWriter writer(exporter);
  writer.setEndian(endian);

  cpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}


// [[Rcpp::export]]
Rcpp::CharacterVector cpp_translate_wkb_wkt(Rcpp::List wkb, int includeZ, int includeM,
                                            int includeSRID, int precision, bool trim) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_translate_wkt_base(reader, includeZ, includeM, includeSRID, precision, trim);
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wkb(Rcpp::List wkb, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);

  return cpp_translate_wkb_base(reader, includeZ, includeM, includeSRID, endian, bufferSize);
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
