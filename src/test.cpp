#include <Rcpp.h>
#include "wkheaders/geometry.h"
#include "wkheaders/io-geometry.h"
#include "wkheaders/io-utils.h"
using namespace Rcpp;

class RawVectorWKBinaryReader: public WKBinaryReader {
public:
  RawVector data;
  R_xlen_t offset;

  RawVectorWKBinaryReader(RawVector data) {
    this->data = data;
    this->offset = 0;
  }

protected:
  unsigned char readChar() {
    return readBinary<unsigned char>();
  }

  double readDouble() {
    return readBinary<double>();
  }

  uint32_t readUint32() {
    return readBinary<uint32_t>();
  }

private:
  template<typename T>
  T readBinary() {
    // Rcout << "Reading " << sizeof(T) << " starting at " << this->offset << "\n";
    if ((this->offset + sizeof(T)) > this->data.size()) {
      stop("Reached end of RawVector input (corrupt data)");
    }

    T dst;
    memcpy(&dst, &(this->data[this->offset]), sizeof(T));
    this->offset += sizeof(T);
    return dst;
  }
};

// [[Rcpp::export]]
void test_basic_reader(RawVector data) {
  RawVectorWKBinaryReader reader(data);
  std::unique_ptr<WKGeometry> geom = reader.readGeometry();
  WKPoint* pt = dynamic_cast<WKPoint*>(geom.get());
  Rcout << "POINT (" << pt->coord.ordinates[0] << " " << pt->coord.ordinates[1] << ")\n";
}
