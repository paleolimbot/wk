
#ifndef WK_GEOMETRY_DEBUG_HANDLER_H
#define WK_GEOMETRY_DEBUG_HANDLER_H

#include "wk/coord.h"
#include "wk/geometry-handler.h"
#include "wk/parse-exception.h"
#include "wk/geometry-meta.h"
#include "wk/geometry-debug-handler.h"

class WKGeometryDebugHandler: public WKGeometryHandler {
public:
  WKGeometryDebugHandler(std::ostream& out): out(out) {}

  virtual void nextFeatureStart(size_t featureId) {
    out << "nextFeatureStart(" << featureId <<  ")\n";
  }

  virtual void nextFeatureEnd(size_t featureId) {
    out << "nextFeatureEnd(" << featureId <<  ")\n";
  }

  virtual void nextNull(size_t featureId) {
    out << "nextNull(" << featureId <<  ")\n";
  }

  virtual void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    out << "nextGeometryStart(";
    this->writeMeta(meta);
    out << ", ";
    this->writeMaybeUnknown(partId, "WKBReader.PART_ID_INVALID");
    out << ")\n";
  }

  virtual void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    out << "nextGeometryEnd(";
    this->writeMeta(meta);
    out << ", ";
    this->writeMaybeUnknown(partId, "WKBReader.PART_ID_INVALID");
    out << ")\n";
  }

  virtual void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    out << "nextLinearRingStart(";
    this->writeMeta(meta);
    out << ", ";
    this->writeMaybeUnknown(size, "WKBReader.SIZE_UNKNOWN");
    out << ", " << ringId << ")\n";
  }

  virtual void nextLinearRingEnd(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    out << "nextLinearRingEnd(";
    this->writeMeta(meta);
    out << ", ";
    this->writeMaybeUnknown(size, "WKBReader.SIZE_UNKNOWN");
    out << ", " << ringId << ")\n";
  }

  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    out << "nextCoordinate(";
    this->writeMeta(meta);
    out << ", " << "WKCoord(x = " << coord.x << ", y = " << coord.y;
    if (coord.hasZ) {
      out << ", z = " << coord.z;
    }

    if (coord.hasM) {
      out << ", m = " << coord.m;
    }

    out << "), " << coordId << ")\n";
  }

  virtual bool nextError(WKParseException& error, size_t featureId) {
    out << "nextError('" << error.what() << "', " << featureId << ")\n";
    return true;
  }

  virtual void writeMaybeUnknown(uint32_t value, const char* ifUnknown) {
    if (value == UINT32_MAX) {
      out << ifUnknown;
    } else {
      out << value;
    }
  }

  virtual void writeMeta(const WKGeometryMeta& meta) {
    this->writeGeometryType(meta.geometryType);
    if (meta.hasSRID) {
      out << " SRID=" << meta.srid;
    }

    if (meta.hasSize) {
      out << " [" << meta.size << "]";
    } else {
      out << " [unknown]";
    }
  }

  virtual void writeGeometryType(uint32_t simpleGeometryType) {
    switch (simpleGeometryType) {
    case WKGeometryType::Point:
      out << "POINT";
      break;
    case WKGeometryType::LineString:
      out << "LINESTRING";
      break;
    case WKGeometryType::Polygon:
      out << "POLYGON";
      break;
    case WKGeometryType::MultiPoint:
      out << "MULTIPOINT";
      break;
    case WKGeometryType::MultiLineString:
      out << "MULTILINESTRING";
      break;
    case WKGeometryType::MultiPolygon:
      out << "MULTIPOLYGON";
      break;
    case WKGeometryType::GeometryCollection:
      out << "GEOMETRYCOLLECTION";
      break;
    default:
      out << "Unknown Type (" << simpleGeometryType << ")";
      break;
    }
  }

protected:
  std::ostream& out;
};

#endif
