
#ifndef WK_WKB_WRITER
#define WK_WKB_WRITER

#include "wk/writer.h"
#include "wk/wkb-reader.h"

class WKBWKBWriter: WKGeometryHandler, public WKWriter {
public:
  WKBWKBWriter(WKBytesProvider& reader, WKBytesExporter& writer): reader(reader, *this), writer(writer) {

  }

  // expose these as the public interface
  virtual bool hasNextFeature() {
    this->writer.seekNextFeature();
    return reader.hasNextFeature();
  }

  virtual void iterateFeature() {
    reader.iterateFeature();
  }

  void setEndian(unsigned char endian) {
    this->endian = endian;
    this->swapEndian = WKBytesUtils::nativeEndian() != endian;
  }

protected:
  WKBReader reader;
  WKGeometryMeta newMeta;

  virtual void nextNull(size_t featureId) {
    this->writer.writeNull();
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    // make a new geometry type based on the creation options
    this->newMeta = this->getNewMeta(meta);

    this->writeEndian();
    this->writeUint32(this->newMeta.ewkbType);

    if (this->newMeta.hasSRID) this->writeUint32(this->newMeta.srid);
    if (this->newMeta.geometryType != WKGeometryType::Point) this->writeUint32(meta.size);
  }

  void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t ringId, uint32_t size) {
    this->writeUint32(size);
  }

  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->writeDouble(coord.x);
    this->writeDouble(coord.y);
    if (this->newMeta.hasZ && coord.hasZ) {
      this->writeDouble(coord.z);
    }
    if (this->newMeta.hasM && coord.hasM) {
      this->writeDouble(coord.m);
    }
  }

private:
  bool swapEndian;
  unsigned char endian;
  WKBytesExporter& writer;

  size_t writeEndian() {
    return this->writeChar(this->endian);
  }

  size_t writeCoord(WKCoord coord) {
    size_t bytesWritten = 0;
    for (size_t i=0; i < coord.size(); i++) {
      bytesWritten += this->writeDouble(coord[i]);
    }
    return bytesWritten;
  }

  size_t writeChar(unsigned char value) {
    return this->writer.writeCharRaw(value);
  }

  size_t writeDouble(double value) {
    if (this->swapEndian) {
      this->writer.writeDoubleRaw(WKBytesUtils::swapEndian<double>(value));
    } else {
      this->writer.writeDoubleRaw(value);
    }
    return sizeof(double);
  }

  size_t writeUint32(uint32_t value) {
    if (this->swapEndian) {
      this->writer.writeUint32Raw(WKBytesUtils::swapEndian<uint32_t>(value));
    } else {
      this->writer.writeUint32Raw(value);
    }
    return sizeof(uint32_t);
  }
};

#endif
