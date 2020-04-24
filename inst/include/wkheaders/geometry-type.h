
#ifndef WKHEADERS_GEOMETRY_TYPE_H
#define WKHEADERS_GEOMETRY_TYPE_H


#include <cstdint>
#include <string>
#include "exception.h"

// https://github.com/postgis/postgis/blob/2.1.0/doc/ZMSgeoms.txt

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

class GeometryType {
public:
  int simpleGeometryType;
  bool hasZ;
  bool hasM;
  bool hasSRID;

  GeometryType() {
    this->simpleGeometryType = SimpleGeometryType::Invalid;
    this->hasZ = false;
    this->hasM = false;
    this->hasSRID = false;
  }

  GeometryType(uint32_t simpleGeometryType, bool hasZ, bool hasM, bool hasSRID) {
    this->simpleGeometryType = simpleGeometryType;
    this->hasZ = hasZ;
    this->hasM = hasM;
    this->hasSRID = hasSRID;
  }

  uint32_t wkbType() {
    uint32_t out = this->simpleGeometryType;
    if (this->hasZ) out += 0x80000000;
    if (this->hasM) out += 0x40000000;
    if (this->hasSRID) out += 0x20000000;
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

  static const char* wktSimpleGeometryType(int simpleGeometryType) {
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
      throw std::runtime_error("GeometryType::wktType(): invalid type");
    }
  }

  static GeometryType getXY(int simpleGeometryType) {
    return GeometryType(simpleGeometryType, false, false, false);
  }

  static GeometryType getZ(int simpleGeometryType) {
    return GeometryType(simpleGeometryType, true, false, false);
  }

  static GeometryType getZM(int simpleGeometryType) {
    return GeometryType(simpleGeometryType, true, true, false);
  }

  static GeometryType getZMS(int simpleGeometryType) {
    return GeometryType(simpleGeometryType, true, true, true);
  }

  static GeometryType getZS(int simpleGeometryType) {
    return GeometryType(simpleGeometryType, true, false, true);
  }

  static GeometryType getM(int simpleGeometryType) {
    return GeometryType(simpleGeometryType, false, true, false);
  }

  static GeometryType getMS(int simpleGeometryType) {
    return GeometryType(simpleGeometryType, false, true, true);
  }

  static GeometryType getS(int simpleGeometryType) {
    return GeometryType(simpleGeometryType, false, false, true);
  }

  static GeometryType get(uint32_t wkbType) {
    switch (wkbType) {
    // basic geometries
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
      return GeometryType(wkbType, false, false, false);

    // Z
    case 0x80000001:
    case 0x80000002:
    case 0x80000003:
    case 0x80000004:
    case 0x80000005:
    case 0x80000006:
    case 0x80000007:
      return GeometryType(wkbType - 0x80000000, true, false, false);

    // | 0x40000000 M
    case 0x40000001:
    case 0x40000002:
    case 0x40000003:
    case 0x40000004:
    case 0x40000005:
    case 0x40000006:
    case 0x40000007:
      return GeometryType(wkbType - 0x40000000, false, true, false);

    // | 0x40000000 | 0x80000000 ZM
    case 0xC0000001:
    case 0xC0000002:
    case 0xC0000003:
    case 0xC0000004:
    case 0xC0000005:
    case 0xC0000006:
    case 0xC0000007:
      return GeometryType(wkbType - 0xC0000000, true, true, false);

    // | 0x20000000 S
    case 0x20000001:
    case 0x20000002:
    case 0x20000003:
    case 0x20000004:
    case 0x20000005:
    case 0x20000006:
    case 0x20000007:
      return GeometryType(wkbType - 0x20000000, false, false, true);

    // | 0x20000000 | 0x80000000 ZS
    case 0xA0000001:
    case 0xA0000002:
    case 0xA0000003:
    case 0xA0000004:
    case 0xA0000005:
    case 0xA0000006:
    case 0xA0000007:
      return GeometryType(wkbType - 0xA0000000, true, false, true);

    // | 0x20000000 | 0x40000000 MS
    case 0x60000001:
    case 0x60000002:
    case 0x60000003:
    case 0x60000004:
    case 0x60000005:
    case 0x60000006:
    case 0x60000007:
      return GeometryType(wkbType - 0x60000000, false, true, true);

    // | 0x20000000 | 0x40000000 | 0x80000000 ZMS
    case 0xE0000001:
    case 0xE0000002:
    case 0xE0000003:
    case 0xE0000004:
    case 0xE0000005:
    case 0xE0000006:
    case 0xE0000007:
      return GeometryType(wkbType - 0xE0000000, true, true, true);

    default:
      throw std::runtime_error(Formatter() << "Unrecognized EWKB geometry type: " << wkbType);
    }
  }
};

#endif
