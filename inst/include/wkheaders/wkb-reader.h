
#ifndef WKHEADERS_WKB_READER_H
#define WKHEADERS_WKB_READER_H

#include "wkb-iterator.h"
#include "geometry.h"

class WKBReader: WKBIterator {
WKGeometry* readGeometry() {
    unsigned char endian = this->readChar();
    this->swapEndian = ((int)endian != (int)IOUtils::nativeEndian());
    uint32_t wkbType = this->readUint32();

    switch (wkbType) {
    case 1:
      return this->readPoint();
    
    default:
      throw std::runtime_error("Unknown geometry type");
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
};

#endif
