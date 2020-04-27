
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

  WKGeometryMeta getNewGeometryType(const WKGeometryMeta geometryType) {
    return WKGeometryMeta(
      geometryType.simpleGeometryType,
      this->actuallyIncludeZ(geometryType),
      this->actuallyIncludeM(geometryType),
      this->actuallyIncludeSRID(geometryType)
    );
  }

  bool actuallyIncludeZ(const WKGeometryMeta geometryType) {
    return actuallyInclude(this->includeZ, geometryType.hasZ, "Z");
  }

  bool actuallyIncludeM(const WKGeometryMeta geometryType) {
    return actuallyInclude(this->includeM, geometryType.hasM, "M");
  }

  bool actuallyIncludeSRID(const WKGeometryMeta geometryType) {
    return actuallyInclude(this->includeSRID, geometryType.hasSRID, "SRID");
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

