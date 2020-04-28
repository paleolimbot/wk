
#include <Rcpp.h>
#include "wk/formatter.h"
#include "wk/wkt-reader.h"
using namespace Rcpp;

class ParseException: public std::runtime_error {
public:

  ParseException(const char* expected, std::string found):
    std::runtime_error(Formatter() << "Expected " << expected <<
      " but found " << found) {}
};

// [[Rcpp::export]]
void parse_test(CharacterVector input) {
  std::string str_in = as<std::string>(input[0]);
  Rcout << "Input: " << str_in << "\n";
  WKGeometryHandler handler;
  WKTReader reader(handler);
  reader.read(str_in);
}
