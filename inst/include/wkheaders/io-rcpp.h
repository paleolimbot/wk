
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

#endif
