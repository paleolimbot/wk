
#include <cstring>
#include <memory>
#include "wkheaders/geometry.h"

#include <endian>
#if __BYTE_ORDER == __LITTLE_ENDIAN
#define WKB_PLATFORM_ENDIAN 0x01
#elif __BYTE_ORDER == __BIG_ENDIAN
#define WKB_PLATFORM_ENDIAN 0x00
#else
#error "System endianess not defined"
#endif

class WKBinaryWriter {
public:
  virtual void write(char value) = 0;
  virtual void write(double value, char endian) = 0;
  virtual void write(uint32_t value, char endian) =  0;

  template <int nOrdinates>
  size_t write(Coord<nOrdinates>* value, char endian) {
    for (int i=0; i < nOrdinates; i++) {
      this->write(value->ordinates[i]);
    }

    return sizeof(double) * value->nDims;
  }

  template <int nOrdinates>
  size_t write(LinearRing<nOrdinates>* value, char endian) {
    size_t bytesWritten = 0;
    for (size_t i=0; i < value->points.size(); i++) {
      bytesWritten += this->write<nOrdinates>(value->points[i]);
    }
    return bytesWritten;
  }


};

class WKBinaryReader {

public:
  std::unique_ptr<WKGeometry> readGeometry() {
    char endian = this->readChar();
    uint32_t wkbType = this->readUint32(endian);

    switch (wkbType) {
    case 1:
      return this->readPoint(endian);
    }
  }

protected:
  virtual char readChar() = 0;
  virtual double readDouble(char endian) = 0;
  virtual uint32_t readUint32(char endian) = 0;

private:
  template<int nOrdinates>
  Coord<nOrdinates> readCoord(char endian) {
    Coord<nOrdinates> out;
    for (int i=0; i< nOrdinates; i++) {
      out.ordinates[i] = this->readDouble(endian);
    }

    return out;
  }

  template<int nOrdinates>
  LinearRing<nOrdinates> readLinearRing(char endian) {
    uint32_t nPoints = this->readUint32(endian);
    LinearRing<nOrdinates> out = LinearRing<nOrdinates>(nPoints);

    for (size_t i=0; i < nPoints; i++) {
      out.points[i] = this->readCoord<nOrdinates>(endian);
    }

    return out;
  }

  std::unique_ptr<WKGeometry> readPoint(char endian) {
    Coord<2> point = this->readCoord<2>(endian);
    std::unique_ptr<WKPoint> out = std::unique_ptr<WKPoint>(new WKPoint(point));
    return out;
  }
};
