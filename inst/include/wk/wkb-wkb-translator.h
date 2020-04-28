
#ifndef WK_WKB_WKB_TRANSLATOR
#define WK_WKB_WKB_TRANSLATOR

#include "wk/translator.h"
#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"

class WKBWKBTranslator: WKBReader, public WKTranslator {
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

protected:
  WKBWriter writer;
  WKGeometryMeta newMeta;

  virtual void nextNull(size_t featureId) {
    this->writer.writeNull();
  }

  void nextGeometryStart(const WKGeometryMeta meta, uint32_t partId) {
    // make a new geometry type based on the creation options
    this->newMeta = this->getNewMeta(meta);

    this->writer.writeEndian();
    this->writer.writeUint32(this->newMeta.ewkbType);

    if (this->newMeta.hasSRID) this->writer.writeUint32(srid);
    if (this->newMeta.geometryType != WKGeometryType::Point) this->writer.writeUint32(meta.size);
  }

  void nextLinearRing(const WKGeometryMeta meta, uint32_t ringId, uint32_t size) {
    this->writer.writeUint32(size);
    WKBReader::nextLinearRing(meta, ringId, size);
  }

  void nextCoordinate(const WKCoord coord, uint32_t coordId) {
    this->writer.writeDouble(coord.x);
    this->writer.writeDouble(coord.y);
    if (this->newMeta.hasZ && coord.hasZ) {
      this->writer.writeDouble(coord.z);
    }
    if (this->newMeta.hasM && coord.hasM) {
      this->writer.writeDouble(coord.m);
    }
  }
};

#endif
