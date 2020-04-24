
#ifndef WKHEADERS_IO_GEOMETRY_H
#define WKHEADERS_IO_GEOMETRY_H

#include <cstring>
#include <memory>
#include "wkheaders/geometry.h"
#include "wkheaders/io-utils.h"

class WKBinaryWriter {
public:
  virtual void write(char value) = 0;
  virtual void write(double value) = 0;
  virtual void write(uint32_t value) =  0;

  template <int nOrdinates>
  size_t write(Coord<nOrdinates>* value) {
    for (int i=0; i < nOrdinates; i++) {
      this->write(value->ordinates[i]);
    }

    return sizeof(double) * value->nDims;
  }

  template <int nOrdinates>
  size_t write(LinearRing<nOrdinates>* value) {
    size_t bytesWritten = 0;
    for (size_t i=0; i < value->points.size(); i++) {
      bytesWritten += this->write<nOrdinates>(value->points[i]);
    }
    return bytesWritten;
  }


};

class WKBinaryReader {

public:
  bool swapEndian;

  WKBinaryReader() {
    this->swapEndian = false;
  }

  void nextGeometry() {
    unsigned char endian = this->readChar();
    this->swapEndian = ((int)endian != (int)IOUtils::nativeEndian());
    uint32_t wkbType = this->readUint32();

    switch (wkbType) {
    case 1:
      this->nextPoint();
      break;
    
    default:
      std::cout << "Bad wkb type: " << wkbType << "\n";
      throw std::exception();
    }
  }

  void nextPoint() {
    double x = this->readDouble();
    double y = this->readDouble();
    nextXY(x, y, 0);
  }

  void nextXY(double x, double y, uint32_t i) {

  }

  WKGeometry* readGeometry() {
    unsigned char endian = this->readChar();
    this->swapEndian = ((int)endian != (int)IOUtils::nativeEndian());
    uint32_t wkbType = this->readUint32();

    switch (wkbType) {
    case 1:
      return this->readPoint();
    
    default:
      std::cout << "Bad wkb type: " << wkbType << "\n";
      throw std::exception();
    }
  }

  WKPoint* readPoint() {
    return new WKPoint(this->readCoord<2>());
  }

  template<int nOrdinates>
  Coord<nOrdinates> readCoord() {
    Coord<nOrdinates> out;
    for (int i=0; i < nOrdinates; i++) {
      out.ordinates[i] = this->readDouble();
    }

    return out;
  }

  template<int nOrdinates>
  LinearRing<nOrdinates> readLinearRing() {
    uint32_t nPoints = this->readUint32();
    LinearRing<nOrdinates> out = LinearRing<nOrdinates>(nPoints);

    for (size_t i=0; i < nPoints; i++) {
      out.points[i] = this->readCoord<nOrdinates>();
    }

    return out;
  }

  // these are not virtual, shouldn't be overridden
  unsigned char readChar() {
    return this->readCharRaw();
  }

  double readDouble() {
    if (this->swapEndian) {
      return IOUtils::swapEndian<double>(this->readDoubleRaw());
    } else
      return this->readDoubleRaw();
  }

  double readUint32() {
    if (this->swapEndian) {
      return IOUtils::swapEndian<uint32_t>(this->readUint32Raw());
    } else
      return this->readUint32Raw();
  }

// must be overwritten
protected:
  virtual unsigned char readCharRaw() = 0;
  virtual double readDoubleRaw() = 0;
  virtual uint32_t readUint32Raw() = 0;
};

#endif
