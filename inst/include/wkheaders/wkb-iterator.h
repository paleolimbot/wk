
#ifndef WKHEADERS_WKB_ITERATOR_H
#define WKHEADERS_WKB_ITERATOR_H

#include <memory>
#include <cmath>
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
  GeometryType geometryType;
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

  virtual void nextFeature() {
    this->reader->seekNextFeature();
    this->recursionLevel = 0;
    this->partId = PART_ID_INVALID;
    this->coordId = COORD_ID_INVALID;
    this->endian = ENDIAN_INVALID;
    this->srid = SRID_INVALID;
    this->readGeometry();
  }

  virtual void readGeometry() {
    this->endian = this->readChar();
    this->swapEndian = ((int)endian != (int)IOUtils::nativeEndian());
    this->nextEndian(this->endian);

    this->geometryType = GeometryType::get(this->readUint32());
    this->nextGeometryType(this->geometryType);

    if (geometryType.hasSRID) {
      this->srid = this->readUint32();
      this->nextSRID(this->srid);
    }

    switch (geometryType.simpleGeometryType) {
    case SimpleGeometryType::Point:
      this->nextGeometry(this->geometryType, 1);
      break;
    case SimpleGeometryType::LineString:
    case SimpleGeometryType::Polygon:
      this->nextGeometry(this->geometryType, this->readUint32());
      break;
    case SimpleGeometryType::MultiPoint:
    case SimpleGeometryType::MultiLineString:
    case SimpleGeometryType::MultiPolygon:
      this->nextMultiGeometry(this->geometryType, this->readUint32());
      break;
    case SimpleGeometryType::GeometryCollection:
      this->nextCollection(this->geometryType, this->readUint32());
      break;
    default:
      throw std::runtime_error(
          Formatter() << 
            "Unrecognized geometry type in WKBIterator::readGeometry(): " << 
            geometryType.simpleGeometryType
      );
    }
  }

  virtual void nextEmpty(GeometryType geometryType) {

  }

  virtual void nextGeometry(GeometryType geometryType, uint32_t size) {
    switch (geometryType.simpleGeometryType) {
    case SimpleGeometryType::Point:
      this->nextPoint(geometryType);
      break;

    default:
      throw std::runtime_error(
          Formatter() << 
            "Unrecognized geometry type: " << 
            geometryType.simpleGeometryType
      );
    }
  }

  virtual void nextMultiGeometry(GeometryType geometryType, uint32_t size) {

  }

  virtual void nextCollection(GeometryType geometryType, uint32_t size) {

  }

  virtual void nextPoint(GeometryType geometryType) {
    this->readPoint(geometryType);
  }

  virtual void readPoint(GeometryType geometryType) {
    this->x = this->readDouble();
    this->y = this->readDouble();

    if (geometryType.hasZ && geometryType.hasM) {
      this->z = this->readDouble();
      this->m = this->readDouble();
      this->nextXYZM(this->x, this->y, this->z, this->m);

    } else if (geometryType.hasZ) {
      this->z = this->readDouble();
      this->nextXYZ(this->x, this->y, this->z);

    } else if (geometryType.hasM) {
      this->m = this->readDouble();
      this->nextXYM(this->x, this->y, this->m);

    } else {
      this->nextXY(this->x, this->y);
    }
  }

  virtual void nextEndian(unsigned char endian) {

  }

  virtual void nextGeometryType(GeometryType geometryType) {
      
  }

  virtual void nextSRID(uint32_t srid) {

  }

  virtual void nextXY(double x, double y) {

  }

  virtual void nextXYZ(double x, double y, double z) {
    this->nextXY(x, y);
  }

  virtual void nextXYM(double x, double y, double m) {
    this->nextXY(x, y);
  }

  virtual void nextXYZM(double x, double y, double z, double m) {
    this->nextXYZ(x, y, z);
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
