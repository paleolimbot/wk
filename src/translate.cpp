
#include <sstream>
#include "wk/wkt-writer.h"
#include "wk/wkt-reader.h"
#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
#include "wk/sexp-writer.h"
#include "wk/sexp-reader.h"
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

Rcpp::CharacterVector cpp_translate_base_wkt(WKReader& reader,
                                             int includeZ, int includeM, int includeSRID,
                                             int precision, bool trim) {
  WKCharacterVectorExporter exporter(reader.nFeatures());
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);
  WKTWriter writer(exporter);

  cpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

Rcpp::List cpp_translate_base_wkb(WKReader& reader,
                                  int includeZ, int includeM, int includeSRID,
                                  int endian, int bufferSize) {
  WKRawVectorListExporter exporter(reader.nFeatures());
  exporter.setBufferSize(bufferSize);
  WKBWriter writer(exporter);
  writer.setEndian(endian);

  cpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

Rcpp::List cpp_translate_base_wksxp(WKReader& reader,
                                     int includeZ, int includeM, int includeSRID) {
  WKSEXPExporter exporter(reader.nFeatures());
  WKSEXPWriter writer(exporter);

  cpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}


// [[Rcpp::export]]
Rcpp::CharacterVector cpp_translate_wkb_wkt(Rcpp::List wkb, int includeZ, int includeM,
                                            int includeSRID, int precision, bool trim) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_translate_base_wkt(reader, includeZ, includeM, includeSRID, precision, trim);
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wkb(Rcpp::List wkb, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_translate_base_wkb(reader, includeZ, includeM, includeSRID, endian, bufferSize);
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wksxp(Rcpp::List wkb, int includeZ, int includeM,
                                    int includeSRID) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_translate_base_wksxp(reader, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
CharacterVector cpp_translate_wkt_wkt(CharacterVector wkt, int includeZ, int includeM,
                                      int includeSRID, int precision, bool trim) {

  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return cpp_translate_base_wkt(reader, includeZ, includeM, includeSRID, precision, trim);
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkt_wkb(CharacterVector wkt, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return cpp_translate_base_wkb(reader, includeZ, includeM, includeSRID, endian, bufferSize);
}


// [[Rcpp::export]]
Rcpp::List cpp_translate_wkt_wksxp(CharacterVector wkt, int includeZ, int includeM,
                                     int includeSRID) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return cpp_translate_base_wksxp(reader, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
CharacterVector cpp_translate_wksxp_wkt(List wksexp, int includeZ, int includeM,
                                         int includeSRID, int precision, bool trim) {
  WKSEXPProvider provider(wksexp);
  WKSEXPReader reader(provider);
  return cpp_translate_base_wkt(reader, includeZ, includeM, includeSRID, precision, trim);
}
