
#include <Rcpp.h>
#include "wk/rcpp-translate.hpp"
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_wkb_translate_wkt(Rcpp::List wkb, int includeZ, int includeM,
                                            int includeSRID, int precision, bool trim) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::translate_wkt(reader, precision, trim, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
Rcpp::List cpp_wkb_translate_wkb(Rcpp::List wkb, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::translate_wkb(reader, endian, bufferSize, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
Rcpp::List cpp_wkb_translate_wksxp(Rcpp::List wkb, int includeZ, int includeM,
                                   int includeSRID) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::translate_wksxp(reader, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
CharacterVector cpp_wkt_translate_wkt(CharacterVector wkt, int includeZ, int includeM,
                                      int includeSRID, int precision, bool trim) {

  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return wk::translate_wkt(reader, precision, trim, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
Rcpp::List cpp_wkt_translate_wkb(CharacterVector wkt, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return wk::translate_wkb(reader, endian, bufferSize, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
Rcpp::List cpp_wkt_translate_wksxp(CharacterVector wkt, int includeZ, int includeM,
                                   int includeSRID) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return wk::translate_wksxp(reader, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
CharacterVector cpp_wksxp_translate_wkt(List wksexp, int includeZ, int includeM,
                                        int includeSRID, int precision, bool trim) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::translate_wkt(reader, precision, trim, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
List cpp_wksxp_translate_wkb(List wksexp, int includeZ, int includeM,
                             int includeSRID, int endian, int bufferSize) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::translate_wkb(reader, endian, bufferSize, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
List cpp_wksxp_translate_wksxp(List wksexp, int includeZ, int includeM, int includeSRID) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::translate_wksxp(reader, includeZ, includeM, includeSRID);
}
