
#include <iostream>
#include <memory>
#include "wkheaders/io-geometry.h"

class InputStreamReader {
public:

  InputStreamReader(std::istream* input) {
    this->input = std::unique_ptr<std::istream>(input);
  }

protected:
  double readDouble(char endian) {
    return read<double>(endian);
  }

  uint32_t readUint32(char endian) {
    return read<uint32_t>(endian);
  }

private:
  std::unique_ptr<std::istream> input;

  template <typename T>
  T read(char endian) {
    T value = readBinary<T>();
    if (endian == WKB_PLATFORM_ENDIAN) {
      return value;
    } else {
      return swapEndian<T>(value);
    }
  }

  template<typename T>
  T readBinary() {
    char* buf[sizeof(T)];
    this->input->read(*buf, sizeof(T));
    T dst;
    memcpy(&dst, buf, sizeof(T));
    return dst;
  }

  // https://github.com/r-spatial/sf/blob/master/src/wkb.cpp
  // https://stackoverflow.com/questions/105252/how-do-i-convert-between-big-endian-and-little-endian-values-in-c
  template <typename T>
  static T swapEndian(T u) {
    union {
    T u;
    unsigned char u8[sizeof(T)];
  } source, dest;
    source.u = u;
    for (size_t k = 0; k < sizeof(T); k++)
      dest.u8[k] = source.u8[sizeof(T) - k - 1];
    return dest.u;
  }
};

