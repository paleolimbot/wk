
#include "cpp11.hpp"
#include "wk-v1-reader.hpp"
#include <clocale>
#include <cstring>
#include <sstream>
#include <cstdlib>

#define HANDLE_OR_RETURN(expr)                                 \
  result = expr;                                               \
  if (result != WK_CONTINUE) return result

class BufferedParserException: public WKParseException {
public:
  BufferedParserException(std::string expected, std::string found, const char* src, int64_t pos):
  WKParseException(makeError(expected, found, src, pos)),
  expected(expected), found(found), src(src), pos(pos) {}

  std::string expected;
  std::string found;
  std::string src;
  int64_t pos;

  static std::string makeError(std::string expected, std::string found, const char* src, int64_t pos) {
    std::stringstream stream;
    stream << "Expected " << expected << " but found " << found << " (:" << pos << ")";
    return stream.str().c_str();
  }
};

class SimpleBufferSource {
public:
  SimpleBufferSource(): str(nullptr), size(0), offset(0) {}

  void set_buffer(const char* str, int64_t size) {
    this->str = str;
    this->size = size;
    this->offset = 0;
  }

  int64_t fill_buffer(char* buffer, int64_t max_size) {
    int64_t copy_size = std::min<int64_t>(this->size - this->offset, max_size);
    if (copy_size > 0) {
      memcpy(buffer, this->str + this->offset, copy_size);
      this->offset += copy_size;
      return copy_size;
    } else {
      return 0;
    }
  }

  std::string error_context(int64_t offset_from_offset) {
    std::stringstream stream;
    stream << " (:" << (this->offset + offset_from_offset) << ")";
    return stream.str();
  }

private:
  const char* str;
  int64_t size;
  int64_t offset;
};

class BufferedParser {
public:
  BufferedParser(int64_t buffer_length): str(nullptr), length(0), offset(0), 
    buffer_length(buffer_length), whitespace(" \r\n\t"), sep(" \r\n\t"), source(nullptr) {
    this->str = (char*) malloc(this->buffer_length);
    if (this->str == nullptr) {
      throw std::runtime_error("Failed to allocate BufferedParser buffer");
    }

    // constructor and deleter set the thread locale while the object is in use
    // for consistent parsing of numbers
#ifdef _MSC_VER
    _configthreadlocale(_ENABLE_PER_THREAD_LOCALE);
#endif
    char* p = std::setlocale(LC_NUMERIC, nullptr);
    if(p != nullptr) {
      this->saved_locale = p;
    }
    std::setlocale(LC_NUMERIC, "C");
  }

  BufferedParser(): BufferedParser(4096) {}

  ~BufferedParser() {
    std::setlocale(LC_NUMERIC, saved_locale.c_str());
    if (this->str != nullptr) {
      free(this->str);
    }
  }

  void setSource(SimpleBufferSource* source) {
    this->source = source;
    this->offset = 0;
    this->length = 0;
  }

  const char* setWhitespace(const char* whitespace) {
    const char* previous_whitespace = this->whitespace;
    this->whitespace = whitespace;
    return previous_whitespace;
  }

  const char* setSeparators(const char* separators) {
    const char* previous_sep = this->sep;
    this->sep = separators;
    return previous_sep;
  }

  int64_t charsLeftInBuffer() {
    return this->length - this->offset;
  }

  bool checkBuffer(int n_chars) {
    int64_t chars_to_keep = this->charsLeftInBuffer();
    if ((chars_to_keep - n_chars) >= 0) {
        return true;
    }

    if (chars_to_keep > 0) {
      memmove(this->str, this->str + this->offset, chars_to_keep);
    }

    int64_t new_chars;
    if (this->source == nullptr) {
      new_chars = 0;
    } else {
      new_chars = this->source->fill_buffer(this->str + chars_to_keep, this->buffer_length - chars_to_keep);
    }

    this->offset = 0;
    this->length = chars_to_keep + new_chars;
    return n_chars <= this->length;
  }

  bool finished() {
    return !(this->checkBuffer(1));
  }

  void advance() {
    if (this->checkBuffer(1)) {
      this->offset++;
    }
  }

  void advance(int n) {
    if (this->checkBuffer(n)) {
      this->offset += n;
    } else {
      this->offset = this->length;
    }
  }

  // Returns the character at the cursor and advances the cursor by one
  char readChar() {
    char out = this->peekChar();
    this->advance();
    return out;
  }

  // Returns the character currently ahead of the cursor without advancing the cursor (skips whitespace)
  char peekChar() {
    this->skipWhitespace();
    if (this->checkBuffer(1)) {
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
    if (this->isOneOf("-nNiI.")) {
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
  long assertInteger() {
    std::string text = this->peekUntilSep();
    const char* textPtr = text.c_str();
    char* endPtr;
    long out = std::strtol(textPtr, &endPtr, 10);
    if (endPtr != (textPtr + text.size())) {
      this->error("an integer", quote(text));
    }

    this->advance(text.size());
    return out;
  }

  // Returns the double currently ahead of the cursor,
  // throwing an exception if whatever is ahead of the
  // cursor cannot be parsed into a double. This will
  // accept "inf", "-inf", and "nan".
  double assertNumber() {
    if (this->finished()) {
      this->error("a number", "end of input");
    }

    std::string text = this->peekUntilSep();
    const char* textPtr = text.c_str();
    char* endPtr;
    double out = std::strtod(textPtr, &endPtr);
    if (endPtr != (textPtr + text.size())) {
      this->error("a number", quote(text));
    }

    this->advance(text.size());
    return out;
  }

  // Asserts that the character at the cursor is whitespace, and
  // returns a std::string of whitespace characters, advancing the
  // cursor to the end of the whitespace.
  std::string assertWhitespace() {
    if (!this->checkBuffer(1)) {
      this->error("whitespace", "end of input");
    }

    char found = this->str[this->offset];
    if (strchr(this->whitespace, found) == nullptr) {
      this->error("whitespace", quote(this->peekUntilSep()));
    }

    int64_t offset0 = this->offset;
    int64_t nWhitespaceChars = this->skipWhitespace();
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
    this->assert_('\0');
  }

  // Returns the text between the cursor and the next separator,
  // which is defined to be whitespace or the following characters: =;,()
  // advancing the cursor. If we are at the end of the string, this will
  // return std::string("")
  std::string readUntilSep() {
    this->skipWhitespace();
    int64_t wordLen = peekUntil(this->sep);
    bool finished = this->finished();
    if (wordLen == 0 && !finished) {
      wordLen = 1;
    }
    std::string out(this->str + this->offset, wordLen);
    this->advance(wordLen);
    return out;
  }

  // Returns the text between the cursor and the next separator without advancing the cursor.
  std::string peekUntilSep() {
    this->skipWhitespace();
    int64_t wordLen = peekUntil(this->sep);
    if (wordLen == 0) {
      return std::string("");
    } else {
      return std::string(this->str + this->offset, wordLen);
    }
  }

  // Advances the cursor past any whitespace, returning the number of characters skipped.
  int64_t skipWhitespace() {
    return this->skipChars(this->whitespace);
  }

  // Skips all of the characters in `chars`, returning the number of characters skipped.
  int64_t skipChars(const char* chars) {
    int64_t n_skipped = 0;
    bool found = false;

    while (!found && !this->finished()) {
      while (this->charsLeftInBuffer() > 0) {
        if (strchr(chars, this->str[this->offset])) {
          this->offset++;
          n_skipped++;
        } else {
          found = true;
          break;
        }
      }
    }
    
    return n_skipped;
  }

  // Returns the number of characters until one of `chars` is encountered,
  // which may be 0.
  int64_t peekUntil(const char* chars) {
    if (this->finished()) {
      return 0;
    }

    int64_t n_chars = -1;
    bool found = false;
    
    while (!found && ((this->offset + n_chars + 1) < this->length)) {
      while ((this->offset + n_chars + 1) < this->length) {
        n_chars++;
        if (strchr(chars, this->str[this->offset + n_chars])) {
          found = true;
          break;
        }
      }

      if (!found) {
        int64_t remaining_buffer_chars = this->buffer_length - n_chars;
        if (remaining_buffer_chars <= 0) {
          this->error("An item with length <= buffer_length", "end of buffer");
        }

        this->checkBuffer(remaining_buffer_chars);
      }
    }

    if (!found && (this->offset + n_chars + 1) == this->length) {
      n_chars++;
    }

    return n_chars;
  }

  [[ noreturn ]] void errorBefore(std::string expected, std::string found) {
    throw BufferedParserException(expected, quote(found), this->str, this->offset - found.size());
  }

  [[noreturn]] void error(std::string expected, std::string found) {
    throw BufferedParserException(expected, found, this->str, this->offset);
  }

  [[noreturn]] void error(std::string expected) {
    throw BufferedParserException(expected, quote(this->peekUntilSep()), this->str, this->offset);
  }

private:
  char* str;
  int64_t length;
  int64_t offset;
  int64_t buffer_length;
  const char* whitespace;
  const char* sep;
  SimpleBufferSource* source;
  std::string saved_locale;

  static std::string expectedFromChars(const char* chars) {
    int64_t nChars = strlen(chars);
    std::stringstream stream;
    for (int64_t i = 0; i < nChars; i++) {
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


class BufferedWKTParser: public BufferedParser {
public:
  BufferedWKTParser() {
    this->setSeparators(" \r\n\t,();=");
  }

  BufferedWKTParser(int64_t buffer_size): BufferedParser(buffer_size) {
    this->setSeparators(" \r\n\t,();=");
  }

  wk_meta_t assertGeometryMeta() {
    wk_meta_t meta;
    WK_META_RESET(meta, WK_GEOMETRY);

    std::string geometry_type = this->assertWord();

    if (geometry_type == "SRID") {
      this->assert_('=');
      meta.srid = this->assertInteger();
      this->assert_(';');
      geometry_type = this->assertWord();
    }

    if (this->is('Z')) {
      this->assert_('Z');
      meta.flags |= WK_FLAG_HAS_Z;
    }

    if (this->is('M')) {
      this->assert_('M');
      meta.flags |= WK_FLAG_HAS_M;
    }

    if (this->isEMPTY()) {
      meta.size = 0;
    }

    meta.geometry_type = this->geometry_typeFromString(geometry_type);
    return meta;
  }

  int geometry_typeFromString(std::string geometry_type) {
    if (geometry_type == "POINT") {
      return WK_POINT;
    } else if(geometry_type == "LINESTRING") {
      return WK_LINESTRING;
    } else if(geometry_type == "POLYGON") {
      return WK_POLYGON;
    } else if(geometry_type == "MULTIPOINT") {
      return WK_MULTIPOINT;
    } else if(geometry_type == "MULTILINESTRING") {
      return WK_MULTILINESTRING;
    } else if(geometry_type == "MULTIPOLYGON") {
      return WK_MULTIPOLYGON;
    } else if(geometry_type == "GEOMETRYCOLLECTION") {
      return WK_GEOMETRYCOLLECTION;
    } else {
      this->errorBefore("geometry type or 'SRID='", geometry_type);
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

class WKTStreamer {
public:

  WKTStreamer(WKHandlerXPtr& handler, int64_t buffer_size): handler(handler), s(buffer_size) {}

  int streamFeature(wk_vector_meta_t* meta, cpp11::r_string item, R_xlen_t feat_id) {
    int result;
    HANDLE_OR_RETURN(this->handler.feature_start(meta, feat_id));

    if (item == NA_STRING) {
      HANDLE_OR_RETURN(this->handler.null_feature());
    } else {
      const char* chars = CHAR(item);
      buffer.set_buffer(chars, strlen(chars));
      s.setSource(&(this->buffer));
      HANDLE_OR_RETURN(this->readGeometryWithType(WK_PART_ID_NONE));
      s.assertFinished();
    }

    return this->handler.feature_end(meta, feat_id);
  }

protected:

  int readGeometryWithType(uint32_t part_id) {
    wk_meta_t meta = s.assertGeometryMeta();
    int result;
    HANDLE_OR_RETURN(this->handler.geometry_start(&meta, part_id));

    switch (meta.geometry_type) {

    case WK_POINT:
      HANDLE_OR_RETURN(this->readPoint(&meta));
      break;

    case WK_LINESTRING:
      HANDLE_OR_RETURN(this->readLineString(&meta));
      break;

    case WK_POLYGON:
      HANDLE_OR_RETURN(this->readPolygon(&meta));
      break;

    case WK_MULTIPOINT:
      HANDLE_OR_RETURN(this->readMultiPoint(&meta));
      break;

    case WK_MULTILINESTRING:
      HANDLE_OR_RETURN(this->readMultiLineString(&meta));
      break;

    case WK_MULTIPOLYGON:
      HANDLE_OR_RETURN(this->readMultiPolygon(&meta));
      break;

    case WK_GEOMETRYCOLLECTION:
      HANDLE_OR_RETURN(this->readGeometryCollection(&meta));
      break;

    default:
      throw WKParseException("Unknown geometry type"); // # nocov
    }

    return this->handler.geometry_end(&meta, part_id);
  }

  int readPoint(const wk_meta_t* meta) {
    if (!s.assertEMPTYOrOpen()) {
      int result;
      HANDLE_OR_RETURN(this->readPointCoordinate(meta));
      s.assert_(')');
    }

    return WK_CONTINUE;
  }

  int readLineString(const wk_meta_t* meta) {
    return this->readCoordinates(meta);
  }

  int readPolygon(const wk_meta_t* meta)  {
    return this->readLinearRings(meta);
  }

  int readMultiPoint(const wk_meta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    wk_meta_t childMeta;
    uint32_t part_id = 0;
    int result;

    if (s.isNumber()) { // (0 0, 1 1)
      do {
        childMeta = this->childMeta(meta, WK_POINT);

        HANDLE_OR_RETURN(this->handler.geometry_start(&childMeta, part_id));

        if (s.isEMPTY()) {
          s.assertWord();
        } else {
          HANDLE_OR_RETURN(this->readPointCoordinate(&childMeta));
        }
        HANDLE_OR_RETURN(this->handler.geometry_end(&childMeta, part_id));

        part_id++;
      } while (s.assertOneOf(",)") != ')');

    } else { // ((0 0), (1 1))
      do {
        childMeta = this->childMeta(meta, WK_POINT);
        HANDLE_OR_RETURN(this->handler.geometry_start(&childMeta, part_id));
        HANDLE_OR_RETURN(this->readPoint(&childMeta));
        HANDLE_OR_RETURN(this->handler.geometry_end(&childMeta, part_id));
        part_id++;
      } while (s.assertOneOf(",)") != ')');
    }

    return WK_CONTINUE;
  }

  int readMultiLineString(const wk_meta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    wk_meta_t childMeta;
    uint32_t part_id = 0;
    int result;

    do {
      childMeta = this->childMeta(meta, WK_LINESTRING);
      HANDLE_OR_RETURN(this->handler.geometry_start(&childMeta, part_id));
      HANDLE_OR_RETURN(this->readLineString(&childMeta));
      HANDLE_OR_RETURN(this->handler.geometry_end(&childMeta, part_id));

      part_id++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  uint32_t readMultiPolygon(const wk_meta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    wk_meta_t childMeta;
    uint32_t part_id = 0;
    int result;

    do {
      childMeta = this->childMeta(meta, WK_POLYGON);
      HANDLE_OR_RETURN(this->handler.geometry_start(&childMeta, part_id));
      HANDLE_OR_RETURN(this->readPolygon(&childMeta));
      HANDLE_OR_RETURN(this->handler.geometry_end(&childMeta, part_id));
      part_id++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  int readGeometryCollection(const wk_meta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    uint32_t part_id = 0;
    int result;

    do {
      HANDLE_OR_RETURN(this->readGeometryWithType(part_id));
      part_id++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  uint32_t readLinearRings(const wk_meta_t* meta) {
    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    uint32_t ring_id = 0;
    int result;

    do {
      HANDLE_OR_RETURN(this->handler.ring_start(meta, WK_SIZE_UNKNOWN, ring_id));
      HANDLE_OR_RETURN(this->readCoordinates(meta));
      HANDLE_OR_RETURN(this->handler.ring_end(meta, WK_SIZE_UNKNOWN, ring_id));
      ring_id++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  // Point coordinates are special in that there can only be one
  // coordinate (and reading more than one might cause errors since
  // writers are unlikely to expect a point geometry with many coordinates).
  // This assumes that `s` has already been checked for EMPTY or an opener
  // since this is different for POINT (...) and MULTIPOINT (.., ...)
  int readPointCoordinate(const wk_meta_t* meta) {
    double coord[4];
    int result;
    int coordSize = 2;
    if (meta->flags & WK_FLAG_HAS_Z) coordSize++;
    if (meta->flags & WK_FLAG_HAS_M) coordSize++;

    this->readCoordinate(coord, coordSize);
    HANDLE_OR_RETURN(handler.coord(meta, coord, 0));
    return WK_CONTINUE;
  }

  int readCoordinates(const wk_meta_t* meta) {
    double coord[4];
    int coordSize = 2;
    if (meta->flags & WK_FLAG_HAS_Z) coordSize++;
    if (meta->flags & WK_FLAG_HAS_M) coordSize++;

    if (s.assertEMPTYOrOpen()) {
      return WK_CONTINUE;
    }

    uint32_t coord_id = 0;
    int result;

    do {
      this->readCoordinate(coord, coordSize);
      HANDLE_OR_RETURN(handler.coord(meta, coord, coord_id));

      coord_id++;
    } while (s.assertOneOf(",)") != ')');

    return WK_CONTINUE;
  }

  void readCoordinate(double* coord, int coordSize) {
    coord[0] = s.assertNumber();
    for (int i = 1; i < coordSize; i++) {
      s.assertWhitespace();
      coord[i] = s.assertNumber();
    }
  }

  wk_meta_t childMeta(const wk_meta_t* parent, int geometry_type) {
    wk_meta_t childMeta;
    WK_META_RESET(childMeta, geometry_type);
    childMeta.flags = parent->flags;
    childMeta.srid = parent->srid;

    childMeta.geometry_type = geometry_type;
    if (s.isEMPTY()) {
      childMeta.size = 0;
    } else {
      childMeta.size = WK_SIZE_UNKNOWN;
    }

    return childMeta;
  }

private:
  WKHandlerXPtr& handler;
  BufferedWKTParser s;
  SimpleBufferSource buffer;
};

[[cpp11::register]]
cpp11::sexp wk_cpp_handle_wkt(cpp11::strings wkt, cpp11::sexp xptr, int buffer_size, bool reveal_size) {
  R_xlen_t n_features = wkt.size();
  wk_vector_meta_t globalMeta;
  WK_VECTOR_META_RESET(globalMeta, WK_GEOMETRY);

  // this is needed to test that handlers function properly when
  // passed a vector of indeterminite length
  if (reveal_size) {
    globalMeta.size = n_features;
  }

  globalMeta.flags |= WK_FLAG_DIMS_UNKNOWN;

  WKHandlerXPtr cppHandler(xptr);
  WKTStreamer streamer(cppHandler, buffer_size);

  int result = cppHandler.vector_start(&globalMeta);

  if (result != WK_ABORT) {
    for (R_xlen_t i = 0; i < n_features; i++) {
      if (((i + 1) % 1000) == 0) cpp11::check_user_interrupt();

      try {
        if (streamer.streamFeature(&globalMeta, wkt[i], i) == WK_ABORT) {
          break;
        }
      } catch (WKParseException& e) {
        if (cppHandler.error(e.what()) == WK_ABORT) {
          break;
        }
      }
    }
  }

  return cppHandler.vector_end(&globalMeta);
}
