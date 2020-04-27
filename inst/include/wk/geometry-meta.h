
#ifndef WK_GEOMETRY_TYPE_H
#define WK_GEOMETRY_TYPE_H

#include <cstdint>
#include <string>
#include "formatter.h"

// https://github.com/postgis/postgis/blob/2.1.0/doc/ZMSgeoms.txt
// https://github.com/r-spatial/sf/blob/master/src/wkb.cpp

enum WKGeometryType {
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

class WKGeometryMeta {
public:
  uint32_t simpleGeometryType;
  bool hasZ;
  bool hasM;
  bool hasSRID;
  uint32_t ewkbType;

  WKGeometryMeta():
    simpleGeometryType(WKGeometryType::Invalid),
    hasZ(false),
    hasM(false),
    hasSRID(false),
    ewkbType(0) {}

  WKGeometryMeta(uint32_t ewkbType):
    simpleGeometryType(ewkbType & 0x000000ff),
    hasZ(ewkbType & EWKB_Z_BIT),
    hasM(ewkbType & EWKB_M_BIT),
    hasSRID(ewkbType & EWKB_SRID_BIT),
    ewkbType(ewkbType) {}

  WKGeometryMeta(int simpleGeometryType, bool hasZ, bool hasM, bool hasSRID):
    simpleGeometryType(simpleGeometryType),
    hasZ(hasZ),
    hasM(hasM),
    hasSRID(hasSRID),
    ewkbType(calcEWKBType(simpleGeometryType, hasZ, hasM, hasSRID)) {}

  std::string wktType() const {
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

private:
  static uint32_t calcEWKBType(int simpleGeometryType, bool hasZ, bool hasM, bool hasSRID) {
    uint32_t out = simpleGeometryType;
    if (hasZ) out |= EWKB_Z_BIT;
    if (hasM) out |= EWKB_M_BIT;
    if (hasSRID) out |= EWKB_SRID_BIT;
    return out;
  }

  static const char* wktSimpleGeometryType(uint32_t simpleGeometryType) {
    switch (simpleGeometryType) {
    case WKGeometryType::Point:
      return "POINT";
    case WKGeometryType::LineString:
      return "LINESTRING";
    case WKGeometryType::Polygon:
      return "POLYGON";
    case WKGeometryType::MultiPoint:
      return "MULTIPOINT";
    case WKGeometryType::MultiLineString:
      return "MULTILINESTRING";
    case WKGeometryType::MultiPolygon:
      return "MULTIPOLYGON";
    case WKGeometryType::GeometryCollection:
      return "GEOMETRYCOLLECTION";
    default:
      throw std::runtime_error(
        Formatter() <<
          "invalid type in WKGeometryMeta::wktSimpleGeometryType(): " <<
          simpleGeometryType
      );
    }
  }
};

#endif