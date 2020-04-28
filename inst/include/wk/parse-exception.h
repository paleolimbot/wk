
#ifndef WK_PARSE_EXCEPTION_H
#define WK_PARSE_EXCEPTION_H

class WKParseException: public std::runtime_error {
public:
  WKParseException(std::string message): std::runtime_error(message) {}
};

#endif
