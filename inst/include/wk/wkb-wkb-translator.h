
#ifndef WK_WKB_WKB_TRANSLATOR
#define WK_WKB_WKB_TRANSLATOR

#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"

class WKBWKBTranslator: WKBReader {
public:
  WKBWKBTranslator(BinaryReader& reader, BinaryWriter& writer): WKBReader(reader), writer(writer) {

  }

  // expose these as the public interface
  virtual bool hasNextFeature() {
    this->writer.seekNextFeature();
    return WKBReader::hasNextFeature();
  }

  virtual void iterateFeature() {
    WKBReader::iterateFeature();
  }

  void setEndian(unsigned char endian) {
    this->writer.setEndian(endian);
  }

  void setIncludeSRID(int includeSRID) {
    this->includeSRID = includeSRID;
  }

  void setIncludeZ(int includeZ) {
    this->includeZ = includeZ;
  }

  void setIncludeM(int includeM) {
    this->includeM = includeM;
  }

protected:
  WKBWriter writer;
  size_t featureId;
  GeometryType newGeometryType;

  virtual void nextFeature(size_t featureId) {
    this->featureId = featureId;
    WKBReader::nextFeature(featureId);
  }

  virtual void nextNull(size_t featureId) {
    this->writer.writeNull();
  }

  void nextGeometryType(GeometryType geometryType, uint32_t partId) {
    // make a new geometry type based on the creation options
    this->newGeometryType = this->getNewGeometryType(geometryType);

    // write endian and new geometry type
    this->writer.writeEndian();
    this->writer.writeUint32(this->newGeometryType.ewkbType);
  }

  void nextSRID(const GeometryType geometryType, uint32_t partId, uint32_t srid) {
    if (this->newGeometryType.hasSRID) {
      this->writer.writeUint32(srid);
    }
  }

  void nextGeometry(const GeometryType geometryType, uint32_t partId, uint32_t size) {
    // only points aren't followed by a uint32 with the number of [rings, coords, geometries]
    if (geometryType.simpleGeometryType != SimpleGeometryType::Point) {
      this->writer.writeUint32(size);
    }

    WKBReader::nextGeometry(geometryType, partId, size);
  }

  void nextLinearRing(const GeometryType geometryType, uint32_t ringId, uint32_t size) {
    this->writer.writeUint32(size);
    WKBReader::nextLinearRing(geometryType, ringId, size);
  }

  void nextCoordinate(const WKCoord coord, uint32_t coordId) {
    this->writer.writeDouble(coord.x);
    this->writer.writeDouble(coord.y);
    if (this->newGeometryType.hasZ && coord.hasZ) {
      this->writer.writeDouble(coord.z);
    }
    if (this->newGeometryType.hasM && coord.hasM) {
      this->writer.writeDouble(coord.m);
    }
  }

private:
  int includeSRID;
  int includeZ;
  int includeM;

  GeometryType getNewGeometryType(const GeometryType geometryType) {
    return GeometryType(
      geometryType.simpleGeometryType,
      this->actuallyIncludeZ(geometryType),
      this->actuallyIncludeM(geometryType),
      this->actuallyIncludeSRID(geometryType)
    );
  }

  bool actuallyIncludeSRID(const GeometryType geometryType) {
    return actuallyInclude(this->includeSRID, geometryType.hasSRID, "SRID");
  }

  bool actuallyIncludeZ(const GeometryType geometryType) {
    return actuallyInclude(this->includeZ, geometryType.hasZ, "Z");
  }

  bool actuallyIncludeM(const GeometryType geometryType) {
    return actuallyInclude(this->includeM, geometryType.hasM, "M");
  }

  bool actuallyInclude(int flag, bool hasValue, const char* label) {
    if (flag == 1 && !hasValue) {
      throw std::runtime_error(
        Formatter() << "Can't include " <<  label <<
          " values in a geometry for which" <<
          label << " values are not defined [feature " << this->featureId << "]"
      );
    }

    return flag && hasValue;
  }
};

#endif
