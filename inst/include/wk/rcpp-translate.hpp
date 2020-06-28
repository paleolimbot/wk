
#ifndef WK_RCPP_TRANSLATE_H
#define WK_RCPP_TRANSLATE_H

#include "wk/wkt-writer.hpp"
#include "wk/wkt-reader.hpp"
#include "wk/wkb-writer.hpp"
#include "wk/wkb-reader.hpp"

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
#include "wk/rcpp-sexp-writer.hpp"
#include "wk/rcpp-sexp-reader.hpp"

namespace wk {

inline void rcpp_translate_base(WKReader& reader, WKWriter& writer,
                                int includeZ = NA_INTEGER, int includeM = NA_INTEGER,
                                int includeSRID = NA_INTEGER) {
  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);

  reader.setHandler(&writer);

  while (reader.hasNextFeature()) {
    Rcpp::checkUserInterrupt();
    reader.iterateFeature();
  }
}

inline Rcpp::List rcpp_translate_wkb(WKReader& reader,
                                    int endian, int bufferSize = 2048,
                                    int includeZ = NA_INTEGER, int includeM = NA_INTEGER,
                                    int includeSRID = NA_INTEGER) {
  WKRawVectorListExporter exporter(reader.nFeatures());
  exporter.setBufferSize(bufferSize);
  WKBWriter writer(exporter);
  writer.setEndian(endian);

  rcpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}


inline Rcpp::CharacterVector rcpp_translate_wkt(WKReader& reader,
                                                int precision = 16, bool trim = true,
                                                int includeZ = NA_INTEGER, int includeM = NA_INTEGER,
                                                int includeSRID = NA_INTEGER) {
  WKCharacterVectorExporter exporter(reader.nFeatures());
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);
  WKTWriter writer(exporter);

  rcpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

inline Rcpp::List rcpp_translate_wksxp(WKReader& reader,
                                       int includeZ = NA_INTEGER, int includeM = NA_INTEGER,
                                       int includeSRID = NA_INTEGER) {
  WKRcppSEXPExporter exporter(reader.nFeatures());
  WKRcppSEXPWriter writer(exporter);

  rcpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

} // namespace wk

#endif
