
#ifndef WK_IO_STRING_H
#define WK_IO_STRING_H

#include <locale>
#include <sstream>
#include "wk/io.h"

class WKStringProvider: public WKProvider {
public:

};

class WKStringExporter: public WKExporter {
public:
  WKStringExporter(size_t size): WKExporter(size) {}
  virtual void writeString(std::string value) = 0;
  virtual void writeConstChar(const char* value) = 0;
  virtual void writeDouble(double value) = 0;
  virtual void writeUint32(uint32_t value) = 0;
};

class WKStringStreamExporter: public WKStringExporter {
public:
  WKStringStreamExporter(size_t size): WKStringExporter(size) {
    this->stream.imbue(std::locale::classic());
  }

  void writeString(std::string value) {
    this->stream << value;
  }

  void writeConstChar(const char* value) {
    this->stream << value;
  }

  void writeDouble(double value) {
    this->stream << value;
  }

  void writeUint32(uint32_t value) {
    this->stream <<  value;
  }

protected:
  std::stringstream stream;
};

#endif
