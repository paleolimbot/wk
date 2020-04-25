
#ifndef WKHEADERS_IO_UTILS_H
#define WKHEADERS_IO_UTILS_H

class IOUtils {
public:
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

  static char nativeEndian(void) {
    const int one = 1;
    unsigned char *cp = (unsigned char *) &one;
    return (char) *cp;
  }
};

class BinaryReader {
public:
  virtual unsigned char readCharRaw() = 0;
  virtual double readDoubleRaw() = 0;
  virtual uint32_t readUint32Raw() = 0;
  virtual bool seekNextFeature() = 0;
  virtual bool featureIsNull() = 0;
  virtual size_t nFeatures() = 0;

  virtual ~BinaryReader() {}
};

#endif
