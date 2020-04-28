
#ifndef WK_TRANSLATOR
#define WK_TRANSLATOR

#include "wk/geometry-meta.h"

class WKTranslator {
public:
  // by default, leave everything as is!
  WKTranslator(): includeZ(2), includeM(2), includeSRID(2) {}

  // expose these as the public interface
  virtual bool hasNextFeature() = 0;
  virtual void iterateFeature() = 0;

  // creation optioins for all WKX formats
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
  int includeZ;
  int includeM;
  int includeSRID;

  WKGeometryMeta getNewMeta(const WKGeometryMeta meta) {
    return WKGeometryMeta(
      meta.geometryType,
      this->actuallyIncludeZ(meta),
      this->actuallyIncludeM(meta),
      this->actuallyIncludeSRID(meta)
    );
  }

  bool actuallyIncludeZ(const WKGeometryMeta meta) {
    return actuallyInclude(this->includeZ, meta.hasZ, "Z");
  }

  bool actuallyIncludeM(const WKGeometryMeta meta) {
    return actuallyInclude(this->includeM, meta.hasM, "M");
  }

  bool actuallyIncludeSRID(const WKGeometryMeta meta) {
    return actuallyInclude(this->includeSRID, meta.hasSRID, "SRID");
  }

  bool actuallyInclude(int flag, bool hasValue, const char* label) {
    if (flag == 1 && !hasValue) {
      throw std::runtime_error(
        Formatter() << "Can't include " <<  label <<
          " values in a geometry for which " <<
          label << " values are not defined"
      );
    }

    return flag && hasValue;
  }
};

#endif

