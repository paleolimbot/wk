
#include <Rcpp.h>
#include "wk/rcpp-translate.h"
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_translate_wkb_wkt(Rcpp::List wkb, int includeZ, int includeM,
                                            int includeSRID, int precision, bool trim) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::translate_wkt(reader, includeZ, includeM, includeSRID, precision, trim);
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wkb(Rcpp::List wkb, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::translate_wkb(reader, includeZ, includeM, includeSRID, endian, bufferSize);
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkb_wksxp(Rcpp::List wkb, int includeZ, int includeM,
                                   int includeSRID) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::translate_wksxp(reader, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
CharacterVector cpp_translate_wkt_wkt(CharacterVector wkt, int includeZ, int includeM,
                                      int includeSRID, int precision, bool trim) {

  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return wk::translate_wkt(reader, includeZ, includeM, includeSRID, precision, trim);
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkt_wkb(CharacterVector wkt, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return wk::translate_wkb(reader, includeZ, includeM, includeSRID, endian, bufferSize);
}

// [[Rcpp::export]]
Rcpp::List cpp_translate_wkt_wksxp(CharacterVector wkt, int includeZ, int includeM,
                                     int includeSRID) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return wk::translate_wksxp(reader, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
CharacterVector cpp_translate_wksxp_wkt(List wksexp, int includeZ, int includeM,
                                         int includeSRID, int precision, bool trim) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::translate_wkt(reader, includeZ, includeM, includeSRID, precision, trim);
}

// [[Rcpp::export]]
List cpp_translate_wksxp_wkb(List wksexp, int includeZ, int includeM,
                              int includeSRID, int endian, int bufferSize) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::translate_wkb(reader, includeZ, includeM, includeSRID, endian, bufferSize);
}

// [[Rcpp::export]]
List cpp_translate_wksxp_wksxp(List wksexp, int includeZ, int includeM, int includeSRID) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::translate_wksxp(reader, includeZ, includeM, includeSRID);
}
