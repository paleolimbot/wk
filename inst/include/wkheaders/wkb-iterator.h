
#ifndef WKHEADERS_WKB_ITERATOR_H
#define WKHEADERS_WKB_ITERATOR_H

#include <memory>
#include "wkheaders/geometry-type.h"
#include "wkheaders/io-utils.h"

class WKBIterator {

public:
  const static int RECURSION_LEVEL_INVALID = -1;
  const static uint32_t PART_ID_INVALID = UINT32_MAX;
  const static uint32_t COORD_ID_INVALID = UINT32_MAX;
  const static uint32_t SRID_INVALID = UINT32_MAX;
  const static unsigned char ENDIAN_INVALID = 0xff;

  int recursionLevel;
  uint32_t partId;
  uint32_t coordId;

  bool swapEndian;
  unsigned char endian;
  EWKBGeometryType ewkbType;
  uint32_t srid;
  double x;
  double y;
  double z;
  double m;

  WKBIterator(BinaryReader* reader) {
    this->reader = std::unique_ptr<BinaryReader>(reader);
    this->swapEndian = false;
    this->recursionLevel = RECURSION_LEVEL_INVALID;
    this->partId = PART_ID_INVALID;
    this->coordId = COORD_ID_INVALID;
    this->srid = SRID_INVALID;
    this->endian = ENDIAN_INVALID;
  }

  virtual bool nextFeature() {
    if (this->reader->seekNextFeature()) {
      this->recursionLevel = 0;
      this->partId = PART_ID_INVALID;
      this->coordId = COORD_ID_INVALID;
      this->endian = ENDIAN_INVALID;
      this->srid = SRID_INVALID;
      this->nextGeometry();
      return true;
    } else {
      return false;
    }
  }

  virtual void nextGeometry() {
    this->endian = this->readChar();
    this->swapEndian = ((int)endian != (int)IOUtils::nativeEndian());
    this->ewkbType = EWKBGeometryType::get(this->readUint32());
    if (ewkbType.hasSRID) {
      this->srid = this->readUint32();
    }

    switch (ewkbType.geometryType) {
    case GeometryType::Point:
      this->nextPoint(ewkbType);
      break;
    
    default:
      throw std::runtime_error(Formatter() << "Unrecognized geometry type: " << ewkbType.geometryType);
    }
  }

  virtual void nextPoint(EWKBGeometryType ewkbType) {
    this->x = this->readDouble();
    this->y = this->readDouble();

    if (ewkbType.hasZ || ewkbType.hasM) {
      this->z = this->readDouble();
      this->m = this->readDouble();
      this->nextXYZM(this->x, this->y, this->z, this->m);
    } else if (ewkbType.hasZ) {
      this->z = this->readDouble();
      this->nextXYZ(this->x, this->y, this->z);
    } else if (ewkbType.hasM) {
      this->m = this->readDouble();
      this->nextXYM(this->x, this->y, this->m);
    } else {
      this->nextXY(this->x, this->y);
    }

    if (ewkbType.hasZ) {

    }
    nextXY(x, y);
  }

  virtual void nextXY(double x, double y) {

  }

  virtual void nextXYZ(double x, double y, double z) {

  }

  virtual void nextXYM(double x, double y, double m) {

  }

  virtual void nextXYZM(double x, double y, double z, double m) {

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

private:
  std::unique_ptr<BinaryReader> reader;

  unsigned char readCharRaw() {
    return this->reader->readCharRaw();
  }

  double readDoubleRaw() {
    return this->reader->readDoubleRaw();
  }

  uint32_t readUint32Raw() {
    return this->reader->readUint32Raw();
  }

  bool seekNextFeature() {
    return this->reader->seekNextFeature();
  }
};

#endif
