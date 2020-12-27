
#include "cpp11.hpp"
#include "wk-v1.hpp"
#include <clocale>
#include <cstring>
#include <sstream>

#define HANDLE_OR_RETURN(expr)                                 \
  result = expr;                                               \
  if (result != WK_CONTINUE) return result

class WKV1ParseableStringException: public WKParseException {
public:
  WKV1ParseableStringException(std::string expected, std::string found, const char* src, size_t pos):
  WKParseException(makeError(expected, found, src, pos)),
  expected(expected), found(found), src(src), pos(pos) {}

  std::string expected;
  std::string found;
  std::string src;
  size_t pos;

  static std::string makeError(std::string expected, std::string found, const char* src, size_t pos) {
    std::stringstream stream;
    stream << "Expected " << expected << " but found " << found << " (:" << pos << ")";
    return stream.str().c_str();
  }
};

class WKV1ParseableString {
public:
  WKV1ParseableString(const char* str, const char* whitespace, const char* sep):
  str(str), length(strlen(str)), offset(0), whitespace(whitespace), sep(sep) {}

  const char* c_str() {
    return this->str;
  }

  // Change the position of the cursor
  size_t seek(size_t position) {
    if (position > this->length) {
      position = this->length;
    } else if (position < 0) {
      position = 0;
    }

    size_t delta = position - this->offset;
    this->offset = position;
    return delta;
  }

  void advance() {
    if (this->offset < this->length) {
      this->offset++;
    }
  }

  void advance(int n) {
    if ((this->offset + n) <= this->length) {
      this->offset += n;
    } else {
      this->offset = this->length;
    }
  }

  bool finished() {
    return this->offset >= this->length;
  }

  // Returns the character at the cursor and advances the cursor
  // by one
  char readChar() {
    char out = this->peekChar();
    this->advance();
    return out;
  }

  // Returns the character currently ahead of the cursor
  // without advancing the cursor (skips whitespace)
  char peekChar() {
    this->skipWhitespace();
    if (this->offset < this->length) {
      return this->str[this->offset];
    } else {
      return '\0';
    }
  }

  // Returns true if the next character is one of `chars`
  bool is(char c) {
    return c == this->peekChar();
  }

  // Returns true if the next character is one of `chars`
  bool isOneOf(const char* chars) {
    return strchr(chars, this->peekChar()) != nullptr;
  }

  // Returns true if the next character is most likely to be a number
  bool isNumber() {
    // complicated by nan and inf
    if (this->isOneOf("-nNiI")) {
      std::string text = this->peekUntilSep();
      try {
        std::stod(text);
        return true;
      } catch(std::exception& e) {
        return false;
      }
    } else {
      return this->isOneOf("-0123456789");
    }
  }

  // Returns true if the next character is a letter
  bool isLetter() {
    char found = this->peekChar();
    return (found >= 'a' && found <= 'z') || (found >= 'A' && found <= 'Z');
  }

  std::string assertWord() {
    std::string text = this->peekUntilSep();
    if (!this->isLetter()) {
      this->error("a word", quote(text));
    }

    this->advance(text.size());
    return text;
  }

  // Returns the integer currently ahead of the cursor,
  // throwing an exception if whatever is ahead of the
  // cursor cannot be parsed into an integer
  uint32_t assertInteger() {
    std::string text = this->peekUntilSep();
    try {
      uint32_t out = std::stoul(text);
      this->advance(text.size());
      return out;
    } catch (std::exception& e) {
      if (this->finished()) {
        this->error("an integer", "end of input");
      } else {
        this->error("an integer", quote(text));
      }
    }
  }

  // Returns the double currently ahead of the cursor,
  // throwing an exception if whatever is ahead of the
  // cursor cannot be parsed into a double. This will
  // accept "inf", "-inf", and "nan".
  double assertNumber() {
    std::string text = this->peekUntilSep();
    try {
      double out = std::stod(text);
      this->advance(text.size());
      return out;
    } catch (std::exception& e) {
      if (this->finished()) {
        this->error("a number", "end of input");
      } else {
        this->error("a number", quote(text));
      }
    }
  }

  // Asserts that the character at the cursor is whitespace, and
  // returns a std::string of whitespace characters, advancing the
  // cursor to the end of the whitespace.
  std::string assertWhitespace() {
    if (this->finished()) {
      this->error("whitespace", "end of input");
    }

    char found = this->str[this->offset];
    if (strchr(this->whitespace, found) == nullptr) {
      this->error("whitespace", quote(this->peekUntilSep()));
    }

    size_t offset0 = this->offset;
    size_t nWhitespaceChars = this->skipWhitespace();
    return std::string(&(this->str[offset0]), nWhitespaceChars);
  }

  void assert_(char c) {
    char found = this->peekChar();
    if (found != c) {
      this->error(quote(c), quote(found));
    }
    this->advance();
  }

  // Asserts the that the character at the cursor is one of `chars`
  // and advances the cursor by one (throwing an exception otherwise).
  char assertOneOf(const char* chars) {
    char found = this->peekChar();

    if ((strlen(chars) > 0) && this->finished()) {
      this->error(expectedFromChars(chars), "end of input");
    } else if (strchr(chars, found) == nullptr) {
      this->error(expectedFromChars(chars), quote(this->peekUntilSep()));
    }

    this->advance();
    return found;
  }

  // Asserts that the cursor is at the end of the input
  void assertFinished() {
    this->assertOneOf("");
  }

  // Returns the text between the cursor and the next separator,
  // which is defined to be whitespace or the following characters: =;,()
  // advancing the cursor. If we are at the end of the string, this will
  // return std::string("")
  std::string readUntilSep() {
    this->skipWhitespace();
    size_t wordLen = peekUntil(this->sep);
    bool finished = this->finished();
    if (wordLen == 0 && !finished) {
      wordLen = 1;
    }
    std::string out(&(this->str[this->offset]), wordLen);
    this->advance(wordLen);
    return out;
  }

  // Returns the text between the cursor and the next separator
  // (" \r\n\t,();=") without advancing the cursor.
  std::string peekUntilSep() {
    this->skipWhitespace();
    size_t wordLen = peekUntil(this->sep);
    if (wordLen == 0 && !this->finished()) {
      wordLen = 1;
    }
    return std::string(&(this->str[this->offset]), wordLen);
  }

  // Advances the cursor past any whitespace, returning the
  // number of characters skipped.
  size_t skipWhitespace() {
    return this->skipChars(this->whitespace);
  }

  // Skips all of the characters in `chars`, returning the number of
  // characters skipped.
  size_t skipChars(const char* chars) {
    size_t offset0 = this->offset;
    char c = this->str[this->offset];
    while ((c != '\0') && strchr(chars, c)) {
      this->offset++;
      if (this->offset >= this->length) {
        break;
      }

      c = this->str[this->offset];
    }

    return this->offset - offset0;
  }

  // Returns the number of characters until one of `chars` is encountered,
  // which may be 0.
  size_t peekUntil(const char* chars) {
    size_t offset0 = this->offset;
    size_t offseti = this->offset;
    char c = this->str[offseti];
    while ((c != '\0') && !strchr(chars, c)) {
      offseti++;
      if (offseti >= this->length) {
        break;
      }

      c = this->str[offseti];
    }

    return offseti - offset0;
  }

  [[ noreturn ]] void errorBefore(std::string expected, std::string found) {
    throw WKV1ParseableStringException(expected, quote(found), this->str, this->offset - found.size());
  }

  [[noreturn]] void error(std::string expected, std::string found) {
    throw WKV1ParseableStringException(expected, found, this->str, this->offset);
  }

  [[noreturn]] void error(std::string expected) {
    throw WKV1ParseableStringException(expected, quote(this->peekUntilSep()), this->str, this->offset);
  }

private:
  const char* str;
  size_t length;
  size_t offset;
  const char* whitespace;
  const char* sep;

  static std::string expectedFromChars(const char* chars) {
    size_t nChars = strlen(chars);
    if (nChars == 0) {
      return "end of input";
    } else if (nChars == 1) {
      return quote(chars);
    }

    std::stringstream stream;
    for (size_t i = 0; i < nChars; i++) {
      if (nChars > 2) {
        stream << ",";
      }
      if (i > 0) {
        stream << " or ";
      }
      stream << quote(chars[i]);
    }

    return stream.str();
  }

  static std::string quote(std::string input) {
    if (input.size() == 0) {
      return "end of input";
    } else {
      std::stringstream stream;
      stream << "'" << input << "'";
      return stream.str();
    }
  }

  static std::string quote(char input) {
    if (input == '\0') {
      return "end of input";
    } else {
      std::stringstream stream;
      stream << "'" << input << "'";
      return stream.str();
    }
  }
};


class WKTV1String: public WKV1ParseableString {
public:
  WKTV1String(const char* str): WKV1ParseableString(str, " \r\n\t", " \r\n\t,();=") {}

  WKGeometryMeta_t assertGeometryMeta() {
    WKGeometryMeta_t meta;
    WK_META_RESET(meta, WK_GEOMETRY);

    std::string geometryType = this->assertWord();

    if (geometryType == "SRID") {
      this->assert_('=');
      meta.srid = this->assertInteger();
      meta.hasSrid = true;
      this->assert_(';');
      geometryType = this->assertWord();
    }

    if (this->is('Z')) {
      this->assert_('Z');
      meta.hasZ = true;
    } else {
      meta.hasZ = false;
    }

    if (this->is('M')) {
      this->assert_('M');
      meta.hasM = true;
    } else {
      meta.hasM = false;
    }

    if (this->isEMPTY()) {
      meta.hasSize = true;
      meta.size = 0;
    }

    meta.geometryType = this->geometryTypeFromString(geometryType);
    return meta;
  }

  int geometryTypeFromString(std::string geometryType) {
    if (geometryType == "POINT") {
      return WK_POINT;
    } else if(geometryType == "LINESTRING") {
      return WK_LINESTRING;
    } else if(geometryType == "POLYGON") {
      return WK_POLYGON;
    } else if(geometryType == "MULTIPOINT") {
      return WK_MULTIPOINT;
    } else if(geometryType == "MULTILINESTRING") {
      return WK_MULTILINESTRING;
    } else if(geometryType == "MULTIPOLYGON") {
      return WK_MULTIPOLYGON;
    } else if(geometryType == "GEOMETRYCOLLECTION") {
      return WK_GEOMETRYCOLLECTION;
    } else {
      this->errorBefore("geometry type or 'SRID='", geometryType);
    }
  }

  bool isEMPTY() {
    return this->peekUntilSep() == "EMPTY";
  }

  bool assertEMPTYOrOpen() {
    if (this->isLetter()) {
      std::string word = this->assertWord();
      if (word != "EMPTY") {
        this->errorBefore("'(' or 'EMPTY'", word);
      }

      return true;
    } else if (this->is('(')) {
      this->assert_('(');
      return false;
    } else {
      this->error("'(' or 'EMPTY'");
    }
  }
};

class WKTStreamingHandler {
public:

  WKTStreamingHandler(WKHandler& handler): handler(handler) {
    // constructor and deleter set the thread locale while the object is in use
#ifdef _MSC_VER
    _configthreadlocale(_ENABLE_PER_THREAD_LOCALE);
#endif
    char* p = std::setlocale(LC_NUMERIC, nullptr);
    if(p != nullptr) {
      this->saved_locale = p;
    }
    std::setlocale(LC_NUMERIC, "C");
  }

  ~WKTStreamingHandler() {
    std::setlocale(LC_NUMERIC, saved_locale.c_str());
  }

  char readFeature(WKGeometryMeta_t* meta, SEXP item, R_xlen_t nFeatures, R_xlen_t featureId) {
    char result;
    HANDLE_OR_RETURN(this->handler.featureStart(meta, nFeatures, featureId));

    if (item == NA_STRING) {
      HANDLE_OR_RETURN(this->handler.nullFeature(meta, nFeatures, featureId));
    } else {
      WKTV1String s(CHAR(item));
      HANDLE_OR_RETURN(this->readGeometryWithType(s, WK_PART_ID_NONE));
      s.assertFinished();
    }

    return this->handler.featureEnd(meta, nFeatures, featureId);
  }

protected:

  char readGeometryWithType(WKTV1String& s, uint32_t partId) {
    WKGeometryMeta_t meta = s.assertGeometryMeta();
    char result;
    HANDLE_OR_RETURN(this->handler.geometryStart(&meta, WK_SIZE_UNKNOWN, partId));

    switch (meta.geometryType) {

    case WK_POINT:
      HANDLE_OR_RETURN(this->readPoint(s, &meta));
      break;

    case WK_LINESTRING:
      HANDLE_OR_RETURN(this->readLineString(s, &meta));
      break;

    case WK_POLYGON:
      HANDLE_OR_RETURN(this->readPolygon(s, &meta));
      break;

    case WK_MULTIPOINT:
      HANDLE_OR_RETURN(this->readMultiPoint(s, &meta));
      break;

    case WK_MULTILINESTRING:
      HANDLE_OR_RETURN(this->readMultiLineString(s, &meta));
      break;

    case WK_MULTIPOLYGON:
      HANDLE_OR_RETURN(this->readMultiPolygon(s, &meta));
      break;

    case WK_GEOMETRYCOLLECTION:
      HANDLE_OR_RETURN(this->readGeometryCollection(s, &meta));
      break;

    default:
      throw WKParseException("Unknown geometry type"); // # nocov
    }

    return this->handler.geometryEnd(&meta, WK_SIZE_UNKNOWN, partId);
  }

  char readPoint(WKTV1String& s, const WKGeometryMeta_t* meta) {
    if (!s.assertEMPTYOrOpen()) {
      char result;
      HANDLE_OR_RETURN(this->readPointCoordinate(s, meta));
      s.assert_(')');
    }

    return WK_CONTINUE;
  }

  char readLineString(WKTV1String& s, const WKGeometryMeta_t* meta) {
    return this->readCoordinates(s, meta);
  }

  char readPolygon(WKTV1String& s, const WKGeometryMeta_t* meta)  {
    return this->readLinearRings(s, meta);
  }

  char readMultiPoint(WKTV1String& s, const WKGeometryMeta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    WKGeometryMeta_t childMeta;
    uint32_t partId = 0;
    char result;

    if (s.isNumber()) { // (0 0, 1 1)
      do {
        childMeta = this->childMeta(s, meta, WK_POINT);

        HANDLE_OR_RETURN(this->handler.geometryStart(&childMeta, WK_SIZE_UNKNOWN, partId));

        if (s.isEMPTY()) {
          s.assertWord();
        } else {
          HANDLE_OR_RETURN(this->readPointCoordinate(s, &childMeta));
        }
        HANDLE_OR_RETURN(this->handler.geometryStart(&childMeta, WK_SIZE_UNKNOWN, partId));

        partId++;
      } while (s.assertOneOf(",)") != ')');

    } else { // ((0 0), (1 1))
      do {
        childMeta = this->childMeta(s, meta, WK_POINT);
        HANDLE_OR_RETURN(this->handler.geometryStart(&childMeta, WK_SIZE_UNKNOWN, partId));
        HANDLE_OR_RETURN(this->readPoint(s, &childMeta));
        HANDLE_OR_RETURN(this->handler.geometryEnd(&childMeta, WK_SIZE_UNKNOWN, partId));
        partId++;
      } while (s.assertOneOf(",)") != ')');
    }

    return WK_CONTINUE;
  }

  char readMultiLineString(WKTV1String& s, const WKGeometryMeta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    WKGeometryMeta_t childMeta;
    uint32_t partId = 0;
    char result;

    do {
      childMeta = this->childMeta(s, meta, WK_LINESTRING);
      HANDLE_OR_RETURN(this->handler.geometryStart(&childMeta, WK_SIZE_UNKNOWN, partId));
      HANDLE_OR_RETURN(this->readLineString(s, &childMeta));
      HANDLE_OR_RETURN(this->handler.geometryEnd(&childMeta, WK_SIZE_UNKNOWN, partId));

      partId++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  uint32_t readMultiPolygon(WKTV1String& s, const WKGeometryMeta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    WKGeometryMeta_t childMeta;
    uint32_t partId = 0;
    char result;

    do {
      childMeta = this->childMeta(s, meta, WK_POLYGON);
      HANDLE_OR_RETURN(this->handler.geometryStart(&childMeta, WK_SIZE_UNKNOWN, partId));
      HANDLE_OR_RETURN(this->readPolygon(s, &childMeta));
      HANDLE_OR_RETURN(this->handler.geometryEnd(&childMeta, WK_SIZE_UNKNOWN, partId));
      partId++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  char readGeometryCollection(WKTV1String& s, const WKGeometryMeta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    uint32_t partId = 0;
    char result;

    do {
      HANDLE_OR_RETURN(this->readGeometryWithType(s, partId));
      partId++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  uint32_t readLinearRings(WKTV1String& s, const WKGeometryMeta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    uint32_t ringId = 0;
    char result;

    do {
      HANDLE_OR_RETURN(this->handler.ringStart(meta, WK_SIZE_UNKNOWN, ringId));
      HANDLE_OR_RETURN(this->readCoordinates(s, meta));
      HANDLE_OR_RETURN(this->handler.ringEnd(meta, WK_SIZE_UNKNOWN, ringId));
      ringId++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  // Point coordinates are special in that there can only be one
  // coordinate (and reading more than one might cause errors since
  // writers are unlikely to expect a point geometry with many coordinates).
  // This assumes that `s` has already been checked for EMPTY or an opener
  // since this is different for POINT (...) and MULTIPOINT (.., ...)
  char readPointCoordinate(WKTV1String& s, const WKGeometryMeta_t* meta) {
    WKCoord_t coord;
    char result;
    int coordSize = 2;
    if (meta->hasZ) coordSize++;
    if (meta->hasM) coordSize++;

    this->readCoordinate(s, &coord, coordSize);
    HANDLE_OR_RETURN(handler.coord(meta, coord, 1, 0));
    return WK_CONTINUE;
  }

  char readCoordinates(WKTV1String& s, const WKGeometryMeta_t* meta) {
    WKCoord_t coord;
    int coordSize = 2;
    if (meta->hasZ) coordSize++;
    if (meta->hasM) coordSize++;

    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    uint32_t coordId = 0;
    char result;

    do {
      this->readCoordinate(s, &coord, coordSize);
      HANDLE_OR_RETURN(handler.coord(meta, coord, WK_SIZE_UNKNOWN, coordId));

      coordId++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  void readCoordinate(WKTV1String& s, WKCoord_t* coord, int coordSize) {
    coord->v[0] = s.assertNumber();
    for (size_t i = 1; i < coordSize; i++) {
      s.assertWhitespace();
      coord->v[i] = s.assertNumber();
    }
  }

  WKGeometryMeta_t childMeta(WKTV1String& s, const WKGeometryMeta_t* parent, int geometryType) {
    WKGeometryMeta_t childMeta;
    WK_META_RESET(childMeta, geometryType);
    childMeta.srid = parent->srid;
    childMeta.hasSrid = parent->hasSrid;

    childMeta.geometryType = geometryType;
    if (s.isEMPTY()) {
      childMeta.hasSize = true;
      childMeta.size = 0;
    } else {
      childMeta.hasSize = false;
      childMeta.size = WK_SIZE_UNKNOWN;
    }

    return childMeta;
  }

private:
  std::string saved_locale;
  WKHandler& handler;
};

[[cpp11::register]]
SEXP wk_cpp_handle_wkt(SEXP wkt, SEXP xptr) {
  R_xlen_t nFeatures = Rf_xlength(wkt);
  WKGeometryMeta_t globalMeta;
  WK_META_RESET(globalMeta, WK_GEOMETRY);
  WK_META_SET_SIZE(globalMeta, nFeatures);

  WKHandler_t* handler = (WKHandler_t*) R_ExternalPtrAddr(xptr);
  WKHandler cppHandler(handler);
  WKTStreamingHandler streamer(cppHandler);

  cppHandler.vectorStart(&globalMeta);

  for (R_xlen_t i = 0; i < nFeatures; i++) {
    try {
      char result = streamer.readFeature(&globalMeta, STRING_ELT(wkt, i), nFeatures, i);
      if (result == WK_ABORT) {
        break;
      }
    } catch (WKParseException& e) {
      if (cppHandler.error(i, e.code(), e.what()) != WK_CONTINUE) {
        break;
      }
    }
  }

  return cppHandler.vectorEnd(&globalMeta);
}
