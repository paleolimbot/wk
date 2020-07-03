
#ifndef WK_FILTER_H
#define WK_FILTER_H

#include "wk/geometry-handler.hpp"

class WKFilter: public WKGeometryHandler {
public:
  WKFilter(WKGeometryHandler& handler): handler(handler) {}

  virtual void nextFeatureStart(size_t featureId) {
    this->handler.nextFeatureStart(featureId);
  }

  virtual void nextFeatureEnd(size_t featureId) {
    this->handler.nextFeatureEnd(featureId);
  }

  virtual void nextNull(size_t featureId) {
    this->handler.nextNull(featureId);
  }

  virtual void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->handler.nextGeometryStart(meta, partId);
  }

  virtual void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    this->handler.nextGeometryEnd(meta, partId);
  }

  virtual void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->handler.nextLinearRingStart(meta, size, ringId);
  }

  virtual void nextLinearRingEnd(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->handler.nextLinearRingEnd(meta, size, ringId);
  }

  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->handler.nextCoordinate(meta, coord, coordId);
  }

  virtual bool nextError(WKParseException& error, size_t featureId) {
    return this->handler.nextError(error, featureId);
  }

protected:
  WKGeometryHandler& handler;
};

class  WKMetaFilter: public WKFilter {
public:
  WKMetaFilter(WKGeometryHandler& handler): WKFilter(handler) {}

  virtual WKGeometryMeta newGeometryMeta(const WKGeometryMeta& meta, uint32_t partId) = 0;

  virtual void nextFeatureStart(size_t featureId) {
    this->metaReplacement.clear();
    this->handler.nextFeatureStart(featureId);
  }

  virtual void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->metaReplacement[&meta] = this->newGeometryMeta(meta, partId);
    this->handler.nextGeometryStart(this->metaReplacement[&meta], partId);
  }

  virtual void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    this->handler.nextGeometryEnd(this->metaReplacement[&meta], partId);
  }

  virtual void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->handler.nextLinearRingStart(this->metaReplacement[&meta], size, ringId);
  }

  virtual void nextLinearRingEnd(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->handler.nextLinearRingEnd(this->metaReplacement[&meta], size, ringId);
  }

  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->handler.nextCoordinate(this->metaReplacement[&meta], coord, coordId);
  }

private:
  // using a hash map to keep track of meta, because it's important to make sure that
  // identical meta objects (at the same address) are used for identical geometry
  // objects (used in s2 and elsewhere to help handle nested collections)
  std::unordered_map<const WKGeometryMeta*, WKGeometryMeta> metaReplacement;
};

#endif
