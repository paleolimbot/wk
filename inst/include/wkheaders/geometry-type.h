
#ifndef WKHEADERS_GEOMETRY_TYPE_H
#define WKHEADERS_GEOMETRY_TYPE_H

#include <cstdint>
#include <string>
#include "exception.h"

// https://github.com/postgis/postgis/blob/2.1.0/doc/ZMSgeoms.txt
// https://github.com/r-spatial/sf/blob/master/src/wkb.cpp

enum SimpleGeometryType {
  Invalid = 0,
  Point = 1,
  LineString = 2,
  Polygon = 3,
  MultiPoint = 4,
  MultiLineString = 5,
  MultiPolygon = 6,
  GeometryCollection = 7
};

#define EWKB_Z_BIT    0x80000000
#define EWKB_M_BIT    0x40000000
#define EWKB_SRID_BIT 0x20000000

class GeometryType {
public:
  uint32_t simpleGeometryType;
  bool hasZ;
  bool hasM;
  bool hasSRID;

  GeometryType() {
    this->simpleGeometryType = SimpleGeometryType::Invalid;
    this->hasZ = false;
    this->hasM = false;
    this->hasSRID = false;
  }

  GeometryType(uint32_t ewkbType) {
    this->simpleGeometryType = ewkbType & 0x000000ff;
    this->hasZ = ewkbType & EWKB_Z_BIT;
		this->hasM = ewkbType & EWKB_M_BIT;
		this->hasSRID = ewkbType & EWKB_SRID_BIT;
    // checks the simple geometry type
    this->wktType();
  }

  GeometryType(int simpleGeometryType, bool hasZ, bool hasM, bool hasSRID) {
    this->simpleGeometryType = simpleGeometryType;
    this->hasZ = hasZ;
    this->hasM = hasM;
    this->hasSRID = hasSRID;
  }

  uint32_t wkbType() {
    uint32_t out = this->simpleGeometryType;
    if (this->hasZ) out |= EWKB_Z_BIT;
    if (this->hasM) out |= EWKB_M_BIT;
    if (this->hasSRID) out |= EWKB_SRID_BIT;
    return out;
  }

  std::string wktType() {
    Formatter f;
    f << wktSimpleGeometryType(this->simpleGeometryType);

    if (this->hasZ || this->hasM) {
      f << " ";
    }
    if (this->hasZ) {
      f << "Z";
    }

    if (this->hasM) {
      f << "M";
    }

    return f;
  }

  static const char* wktSimpleGeometryType(uint32_t simpleGeometryType) {
    switch (simpleGeometryType) {
    case SimpleGeometryType::Point:
      return "POINT";
    case SimpleGeometryType::LineString:
      return "LINESTRING";
    case SimpleGeometryType::Polygon:
      return "POLYGON";
    case SimpleGeometryType::MultiPoint:
      return "MULTIPOINT";
    case SimpleGeometryType::MultiLineString:
      return "MULTILINESTRING";
    case SimpleGeometryType::MultiPolygon:
      return "MULTIPOLYGON";
    case SimpleGeometryType::GeometryCollection:
      return "GEOMETRYCOLLECTION";
    default:
      throw std::runtime_error(
        Formatter() << "GeometryType::wktType(): invalid type: " << simpleGeometryType
      );
    }
  }
};

#endif
