
#ifndef WK_TRANSLATOR
#define WK_TRANSLATOR

#include "wk/geometry-type.h"

class WKTranslator {
public:

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
  size_t featureId;
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

