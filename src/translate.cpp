
#include <Rcpp.h>
#include "wk/rcpp-translate.hpp"
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::CharacterVector cpp_wkb_translate_wkt(Rcpp::List wkb, int includeZ, int includeM,
                                            int includeSRID, int precision, bool trim) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::rcpp_translate_wkt(reader, precision, trim, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
Rcpp::List cpp_wkb_translate_wkb(Rcpp::List wkb, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::rcpp_translate_wkb(reader, endian, bufferSize, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
Rcpp::List cpp_wkb_translate_wksxp(Rcpp::List wkb, int includeZ, int includeM,
                                   int includeSRID) {

  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::rcpp_translate_wksxp(reader, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
CharacterVector cpp_wkt_translate_wkt(CharacterVector wkt, int includeZ, int includeM,
                                      int includeSRID, int precision, bool trim) {

  WKCharacterVectorProvider provider(wkt);
  WKTStreamer reader(provider);
  return wk::rcpp_translate_wkt(reader, precision, trim, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
Rcpp::List cpp_wkt_translate_wkb(CharacterVector wkt, int includeZ, int includeM,
                                 int includeSRID, int endian, int bufferSize) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return wk::rcpp_translate_wkb(reader, endian, bufferSize, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
Rcpp::List cpp_wkt_translate_wksxp(CharacterVector wkt, int includeZ, int includeM,
                                   int includeSRID) {

  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return wk::rcpp_translate_wksxp(reader, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
CharacterVector cpp_wksxp_translate_wkt(List wksexp, int includeZ, int includeM,
                                        int includeSRID, int precision, bool trim) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::rcpp_translate_wkt(reader, precision, trim, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
List cpp_wksxp_translate_wkb(List wksexp, int includeZ, int includeM,
                             int includeSRID, int endian, int bufferSize) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::rcpp_translate_wkb(reader, endian, bufferSize, includeZ, includeM, includeSRID);
}

// [[Rcpp::export]]
List cpp_wksxp_translate_wksxp(List wksexp, int includeZ, int includeM, int includeSRID) {
  WKRcppSEXPProvider provider(wksexp);
  WKRcppSEXPReader reader(provider);
  return wk::rcpp_translate_wksxp(reader, includeZ, includeM, includeSRID);
}

// -------- XY -----------

// [[Rcpp::export]]
CharacterVector cpp_translate_xyzm_wkt(List xy, int precision, int trim) {
  RcppWKFieldsProvider provider(xy);
  RcppXYZMReader reader(provider);
  return wk::rcpp_translate_wkt(reader, precision, trim, 2, 2, 0);
}

// [[Rcpp::export]]
List cpp_translate_xyzm_wkb(List xy, int endian, int bufferSize) {
  RcppWKFieldsProvider provider(xy);
  RcppXYZMReader reader(provider);
  return wk::rcpp_translate_wkb(reader, endian, bufferSize, 2, 2, 0);
}

// [[Rcpp::export]]
List cpp_translate_xyzm_wksxp(List xy) {
  RcppWKFieldsProvider provider(xy);
  RcppXYZMReader reader(provider);
  return wk::rcpp_translate_wksxp(reader, 2, 2, 0);
}

// [[Rcpp::export]]
List cpp_translate_wkt_xyzm(CharacterVector wkt) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return wk::rcpp_translate_xyzm(reader);
}

// [[Rcpp::export]]
List cpp_translate_wkb_xyzm(List wkb) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return wk::rcpp_translate_xyzm(reader);
}

// [[Rcpp::export]]
List cpp_translate_wksxp_xyzm(List wksxp) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return wk::rcpp_translate_xyzm(reader);
}

// -------- rct ---------

// [[Rcpp::export]]
CharacterVector cpp_translate_rct_wkt(List rct, int precision, int trim) {
  RcppWKFieldsProvider provider(rct);
  RcppWKRctReader reader(provider);
  return wk::rcpp_translate_wkt(reader, precision, trim, 0, 0, 0);
}

// [[Rcpp::export]]
List cpp_translate_rct_wkb(List rct, int endian, int bufferSize) {
  RcppWKFieldsProvider provider(rct);
  RcppWKRctReader reader(provider);
  return wk::rcpp_translate_wkb(reader, endian, bufferSize, 0, 0, 0);
}

// [[Rcpp::export]]
List cpp_translate_rct_wksxp(List rct) {
  RcppWKFieldsProvider provider(rct);
  RcppWKRctReader reader(provider);
  return wk::rcpp_translate_wksxp(reader, 0, 0, 0);
}
