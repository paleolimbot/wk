
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
  WKBinaryReader() {
    this->swapEndian = false;
  }

  std::unique_ptr<WKGeometry> readGeometry() {
    char endian = this->doReadChar();
    this->swapEndian = ((int)endian != (int)IOUtils::nativeEndian());
    uint32_t wkbType = this->doReadUint32();

    WKGeometry* out;

    switch (wkbType) {
    case 1:
      out = this->readPoint();
      break;
    default:
      std::cout << "Bad wkb type: " << wkbType << "\n";
      throw std::exception();
    }

    return std::unique_ptr<WKGeometry>(out);
  }

protected:
  virtual unsigned char readChar() = 0;
  virtual double readDouble() = 0;
  virtual uint32_t readUint32() = 0;

private:
  WKPoint* readPoint() {
    Coord<2> point = this->readCoord<2>();
    return new WKPoint(point);
  }

  bool swapEndian;

  unsigned char doReadChar() {
    return this->readChar();
  }

  double doReadDouble() {
    if (this->swapEndian) {
      return IOUtils::swapEndian<double>(this->readDouble());
    } else
      return this->readDouble();
  }

  double doReadUint32() {
    if (this->swapEndian) {
      return IOUtils::swapEndian<uint32_t>(this->readUint32());
    } else
      return this->readUint32();
  }

  template<int nOrdinates>
  Coord<nOrdinates> readCoord() {
    Coord<nOrdinates> out;
    for (int i=0; i< nOrdinates; i++) {
      out.ordinates[i] = this->doReadDouble();
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
};

#endif
