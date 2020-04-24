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
    return read<unsigned char>();
  }

  double readDouble() {
    return read<double>();
  }

  uint32_t readUint32() {
    return read<uint32_t>();
  }

private:
  template <typename T>
  T read() {
    T value = readBinary<T>();
    if (this->swapEndian) {
      return IOUtils::swapEndian<T>(value);
    } else {
      return value;
    }
  }

  template<typename T>
  T readBinary() {
    std::cout << "Reading " << sizeof(T) << " starting at " << this->offset << ": ";
    if ((this->offset + sizeof(T)) > this->data.size()) {
      stop("Reached end of RawVector input (corrupt data)");
    }

    T dst;
    memcpy(&dst, &(this->data[this->offset]), sizeof(T));
    this->offset += sizeof(T);
    std::cout << dst << "\n";
    return dst;
  }
};

// [[Rcpp::export]]
void test(RawVector data) {

  RawVectorWKBinaryReader reader(data);
  reader.readGeometry();

  Rcout << "finished!\n";
}
