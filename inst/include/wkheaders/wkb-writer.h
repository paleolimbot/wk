
#ifndef WKHEADERS_WKB_WRITER_H
#define WKHEADERS_WKB_WRITER_H

#include <memory>
#include "wkheaders/geometry-type.h"
#include "wkheaders/wk-coord.h"
#include "wkheaders/io-utils.h"

class WKBWriter {
public:

  WKBWriter(BinaryWriter* writer) {
    this->swapEndian = false;
    this->writer = std::unique_ptr<BinaryWriter>(writer);
  }

  void setEndian(unsigned char endian) {
    this->endian = endian;
    this->swapEndian = IOUtils::nativeEndian() != endian;
  }

  size_t writeEndian() {
    return this->writeChar(this->endian);
  }

  size_t writeCoord(WKCoord coord) {
    size_t bytesWritten = 0;
    for (size_t i=0; i < coord.size(); i++) {
       bytesWritten += this->writeDouble(coord[i]);
    }
    return bytesWritten;
  }

  size_t writeChar(unsigned char value) {
    return this->writer->writeCharRaw(value);
  }

  size_t writeDouble(double value) {
    if (this->swapEndian) {
      this->writer->writeDoubleRaw(IOUtils::swapEndian<double>(value));
    } else {
      this->writer->writeDoubleRaw(value);
    }
    return sizeof(double);
  }

  size_t writeUint32(uint32_t value) {
    if (this->swapEndian) {
      this->writer->writeDoubleRaw(IOUtils::swapEndian<uint32_t>(value));
    } else {
      this->writer->writeDoubleRaw(value);
    }
    return sizeof(uint32_t);
  }

private:
  bool swapEndian;
  unsigned char endian;
  std::unique_ptr<BinaryWriter> writer;
};

#endif