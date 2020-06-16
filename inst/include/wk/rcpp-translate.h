
#ifndef WK_RCPP_TRANSLATE_H
#define WK_RCPP_TRANSLATE_H

#include "wk/wkt-writer.h"
#include "wk/wkt-reader.h"
#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
#include "wk/rcpp-sexp-writer.h"
#include "wk/rcpp-sexp-reader.h"

#ifndef WK_ENDIAN_DEFAULT
#define WK_ENDIAN_DEFAULT 1
#endif

namespace wk {

inline void translate_base(WKReader& reader, WKWriter& writer,
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

inline Rcpp::List translate_wkb(WKReader& reader,
                                int includeZ = NA_INTEGER, int includeM = NA_INTEGER,
                                int includeSRID = NA_INTEGER,
                                int endian = WK_ENDIAN_DEFAULT, int bufferSize = 2048) {
  WKRawVectorListExporter exporter(reader.nFeatures());
  exporter.setBufferSize(bufferSize);
  WKBWriter writer(exporter);
  writer.setEndian(endian);

  translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}


inline Rcpp::CharacterVector translate_wkt(WKReader& reader,
                                           int includeZ = NA_INTEGER, int includeM = NA_INTEGER,
                                           int includeSRID = NA_INTEGER,
                                           int precision = 16, bool trim = true) {
  WKCharacterVectorExporter exporter(reader.nFeatures());
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);
  WKTWriter writer(exporter);

  translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

inline Rcpp::List translate_wksxp(WKReader& reader,
                                  int includeZ = NA_INTEGER, int includeM = NA_INTEGER,
                                  int includeSRID = NA_INTEGER) {
  WKSEXPExporter exporter(reader.nFeatures());
  WKRcppSEXPWriter writer(exporter);

  translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

} // namespace wk

#endif
