
#ifndef WK_IO_RCPP_H
#define WK_IO_RCPP_H

#include <Rcpp.h>
#include "wk/parse-exception.h"
#include "wk/io-bytes.h"

using namespace Rcpp;

class WKRawVectorListReader: public WKBytesProvider {
public:

  WKRawVectorListReader(List container) {
    this->container = container;
    this->index = -1;
    this->featureNull = true;
    this->offset = 0;
  }

  unsigned char readCharRaw() {
    return readBinary<unsigned char>();
  }

  double readDoubleRaw() {
    return readBinary<double>();
  }

  uint32_t readUint32Raw() {
    return readBinary<uint32_t>();
  }

  bool seekNextFeature() {
    this->index += 1;
    if (this->index >= this->container.size()) {
      return false;
    }

    SEXP item = this->container[this->index];

    if (item == R_NilValue) {
      this->featureNull = true;
      this->data = RawVector::create();
    } else {
      this->featureNull = false;
      this->data = (RawVector)item;
    }

    this->offset = 0;
    return true;
  }

  bool featureIsNull() {
    return this->featureNull;
  }

  size_t nFeatures() {
    return container.size();
  }

private:
  List container;
  R_xlen_t index;
  RawVector data;
  R_xlen_t offset;
  bool featureNull;

  template<typename T>
  T readBinary() {
    // Rcout << "Reading " << sizeof(T) << " starting at " << this->offset << "\n";
    if ((this->offset + sizeof(T)) > this->data.size()) {
      throw WKParseException("Reached end of RawVector input");
    }

    T dst;
    memcpy(&dst, &(this->data[this->offset]), sizeof(T));
    this->offset += sizeof(T);
    return dst;
  }
};

class WKRawVectorListWriter: public WKBytesExporter {
public:
  List output;
  RawVector buffer;
  bool prevIsNull;

  R_xlen_t index;
  R_xlen_t offset;

  WKRawVectorListWriter(size_t size): WKBytesExporter(size) {
    this->prevIsNull = false;
    this->index = -1;
    this->offset = 0;
    output = List(size);
    this->setBufferSize(2048);
  }

  void setBufferSize(R_xlen_t bufferSize) {
    if (bufferSize <= 0) {
      throw std::runtime_error("Attempt to set zero or negative buffer size");
    }

    RawVector newBuffer = RawVector(bufferSize);
    this->buffer = newBuffer;
  }

  void extendBufferSize(R_xlen_t bufferSize) {
    if (bufferSize < this->buffer.size()) {
      throw std::runtime_error("Attempt to shrink RawVector buffer size");
    }

    RawVector newBuffer = RawVector(bufferSize);
    for (R_xlen_t i=0; i < this->offset; i++) {
      newBuffer[i] = this->buffer[i];
    }

    this->buffer = newBuffer;
  }

  bool seekNextFeature() {
    if (this->index == -1) {
      this->index += 1;
      return true;
    }

    if (this->prevIsNull) {
      this->output[this->index] = R_NilValue;
      this->prevIsNull = false;
    } else if (this->offset == 0) {
      this->output[this->index] = RawVector::create();
    } else {
      this->output[this->index] = this->buffer[Range(0, this->offset - 1)];
    }

    this->index += 1;
    this->offset = 0;

    return this->index >= this->output.size();
  }

  void writeNull() {
    this->prevIsNull = true;
  }

  size_t writeCharRaw(unsigned char value) {
    return this->writeBinary<unsigned char>(value);
  }

  size_t writeDoubleRaw(double value) {
    return this->writeBinary<double>(value);
  }

  size_t writeUint32Raw(uint32_t value) {
    return this->writeBinary<uint32_t>(value);
  }

  template<typename T>
  size_t writeBinary(T value) {
    // Rcout << "Writing " << sizeof(T) << "(" << value << ") starting at " << this->offset << "\n";
    while ((this->offset + sizeof(T)) > this->buffer.size()) {
      // we're going to need a bigger boat
      this->extendBufferSize(this->buffer.size() * 2);
    }

    memcpy(&(this->buffer[this->offset]), &value, sizeof(T));
    this->offset += sizeof(T);
    return sizeof(T);
  }
};

#endif
