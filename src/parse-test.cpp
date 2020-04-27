
#include <Rcpp.h>
#include "wk/formatter.h"
#include "wk/string-tokenizer.h"
using namespace Rcpp;

class ParseException: public std::runtime_error {
public:

  ParseException(const char* expected, std::string found):
    std::runtime_error(Formatter() << "Expected " << expected <<
      " but found " << found) {}
};

// [[Rcpp::export]]
void parse_test(CharacterVector input) {
  String str_in = input[0];
  Rcout << "Input: " << std::string(str_in);

  // std::stringstream stream(str_in);
  // std::string geometryInfo;
  // std::getline(stream, geometryInfo, '(');
  // if (stream.eof()) {
  //   throw ParseException("(", "end of input");
  // }
  //
  // Rcout << "Geometry Info: " << geometryInfo << "\n";
  //
  //
  // std::string almostEndWhitespace;
  // std::getline(stream, almostEndWhitespace, ')');
  // if (stream.eof()) {
  //   throw ParseException("')'", "end of input");
  // }

  // TODO check that almostEndWhitespace is actually whitespace
  // TODO check that everyting before eof is whitespace

  // char* cstr = strdup(str_in.get_cstring());
  // char* ptr;
  //
  // ptr = strtok(cstr, "\t\r\n (");
  //
  // int simpleGeometryType;
  //
  // if (strcmp(ptr, "POINT") == 0) {
  //   simpleGeometryType = WKGeometryType::Point;
  // } else if (strcmp(ptr, "LINESTRING") == 0) {
  //   simpleGeometryType = WKGeometryType::LineString;
  // } else if (strcmp(ptr, "POLYGON") == 0) {
  //   simpleGeometryType = WKGeometryType::Polygon;
  // } else if (strcmp(ptr, "MULTIPOINT") == 0) {
  //   simpleGeometryType = WKGeometryType::MultiPoint;
  // } else if (strcmp(ptr, "MULTILINESTRING") == 0) {
  //   simpleGeometryType = WKGeometryType::MultiLineString;
  // } else if (strcmp(ptr, "MULTIPOLYGON") == 0) {
  //   simpleGeometryType = WKGeometryType::MultiPolygon;
  // } else if (strcmp(ptr, "GEOMETRYCOLLECTION") == 0) {
  //   simpleGeometryType = WKGeometryType::GeometryCollection;
  // } else {
  //   throw ParseException("a simple geometry type", ptr);
  // }
  //
  // Rcout << "Found geometry type: " << simpleGeometryType << "\n";
  //
  // WKCoord coord;
  // ptr = strtok(NULL, "(");
  // ptr = strtok(NULL, "0123456789-");
  //
  // Rcout << "current ptr: '" << ptr  << "'";
  // std::free(cstr);
}

