
#ifndef WKHEADERS_IO_RCPP_H
#define WKHEADERS_IO_RCPP_H

#include <Rcpp.h>
#include "wkheaders/io-utils.h"

using namespace Rcpp;

class WKRawVectorListReader: public BinaryReader {
public:

  WKRawVectorListReader(List container) {
    this->container = container;
    this->index = -1;
    this->featureNull = true;
    this->offset = 0;
  }

protected:
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
      stop("Reached end of RawVector input");
    }

    T dst;
    memcpy(&dst, &(this->data[this->offset]), sizeof(T));
    this->offset += sizeof(T);
    return dst;
  }
};

class WKRawVectorListWriter: BinaryWriter {
public:
  List output;
  RawVector buffer;
  bool prevIsNull;

  R_xlen_t index;
  R_xlen_t offset;

  WKRawVectorListWriter(size_t size): BinaryWriter(size) {
    this->prevIsNull = false;
    this->index = 0;
    this->offset = 0;
    output = List(size);
    this->buffer = RawVector(2048);
  }

  bool seekNextFeature() {
    if (this->prevIsNull) {
      this->output[this->index] = R_NilValue;
      this->prevIsNull = false;
    } else {
      this->output[this->index] = this->buffer[Range(0, this->offset)];
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
    if ((this->offset + sizeof(T)) > this->buffer.size()) {
      stop("Reached end of RawVector buffer");
    }

    memcpy(&(this->buffer[this->offset]), &value, sizeof(T));
    this->offset += sizeof(T);
    return sizeof(T);
  }
};

#endif
