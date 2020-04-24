
#include <iostream>
#include <memory>
#include "wkheaders/io-geometry.h"
#include "wkheaders/io-utils.h"

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
      return IOUtils::swapEndian<T>(value);
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
};

