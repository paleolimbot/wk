
#include "cpp11/protect.hpp"
#include "cpp11/declarations.hpp"
#include "wk-v1.h"
#define FASTFLOAT_ASSERT(x) { if (!(x)) Rf_error("fastfloat assert failed"); }
#include "fast_float.h"
#include <clocale>
#include <cstring>
#include <sstream>
#include <cstdlib>

#define HANDLE_OR_RETURN(expr)                                 \
  result = expr;                                               \
  if (result != WK_CONTINUE) return result

#define HANDLE_CONTINUE_OR_BREAK(expr)                         \
  result = expr;                                               \
  if (result == WK_ABORT_FEATURE) continue; else if (result == WK_ABORT) break

class BufferedParserException: public std::runtime_error {
public:
  BufferedParserException(std::string expected, std::string found, std::string context):
  std::runtime_error(makeError(expected, found, context)),
    expected(expected), found(found), context(context) {}

  std::string expected;
  std::string found;
  std::string context;

  static std::string makeError(std::string expected, std::string found, std::string context = "") {
    std::stringstream stream;
    stream << "Expected " << expected << " but found " << found << context;
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

private:
  const char* str;
  int64_t size;
  int64_t offset;
};

template <class SourceType>
class BufferedParser {
public:
  BufferedParser(int64_t buffer_length): str(nullptr), length(0), offset(0), 
    buffer_length(buffer_length), source_offset(0),
    whitespace(" \r\n\t"), sep(" \r\n\t"), source(nullptr) {
    this->str = (char*) malloc(this->buffer_length);
    if (this->str == nullptr) {
      throw std::runtime_error("Failed to allocate BufferedParser buffer");
    }
  }

  ~BufferedParser() {
    if (this->str != nullptr) {
      free(this->str);
    }
  }

  void setSource(SimpleBufferSource* source) {
    this->source = source;
    this->offset = 0;
    this->length = 0;
    this->source_offset = 0;
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

    if (this->source == nullptr) {
      return false;
    }

    if (chars_to_keep > 0) {
      memmove(this->str, this->str + this->offset, chars_to_keep);
    }

    int64_t new_chars = this->source->fill_buffer(this->str + chars_to_keep, this->buffer_length - chars_to_keep);
    if (new_chars == 0) {
      this->source = nullptr;
    }

    this->source_offset += new_chars;
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
      std::string_view text = this->peekUntilSep();
      double out;
      auto result = fast_float::from_chars(text.begin(), text.end(), out);
      return result.ec == std::errc();
    } else {
      return this->isOneOf("-0123456789");
    }
  }

  // Returns true if the next character is a letter
  bool isLetter() {
    char found = this->peekChar();
    return (found >= 'a' && found <= 'z') || (found >= 'A' && found <= 'Z');
  }

  std::string_view assertWord() {
    std::string_view text = this->peekUntilSep();
    if (!this->isLetter()) {
      this->error("a word", quote(text));
    }

    this->offset += text.size();
    return text;
  }

  // Returns the integer currently ahead of the cursor,
  // throwing an exception if whatever is ahead of the
  // cursor cannot be parsed into an integer
  long assertInteger() {
    std::string_view text = this->peekUntilSep();
    char* endPtr;
    long out = std::strtol(text.begin(), &endPtr, 10);
    if (endPtr != text.end()) {
      this->error("an integer", quote(text));
    }

    this->offset += text.size();
    return out;
  }

  // Returns the double currently ahead of the cursor,
  // throwing an exception if whatever is ahead of the
  // cursor cannot be parsed into a double. This will
  // accept "inf", "-inf", and "nan".
  double assertNumber() {
    std::string_view text = this->peekUntilSep();
    double out;
    auto result = fast_float::from_chars(text.begin(), text.end(), out);

    if (result.ec != std::errc()) {
      this->error("a number", quote(text));
    } else {
      this->offset += text.size();
      return out;
    }
  }

  // Asserts that the character at the cursor is whitespace, and
  // returns a std::string of whitespace characters, advancing the
  // cursor to the end of the whitespace.
  void assertWhitespace() {
    if (!this->checkBuffer(1)) {
      this->error("whitespace", "end of input");
    }

    char found = this->str[this->offset];
    if (strchr(this->whitespace, found) == nullptr) {
      std::string_view untilSep = this->peekUntilSep();
      if (untilSep.size() == 0) {
        this->error("whitespace", quote(found));
      } else {
        this->error("whitespace", quote(this->peekUntilSep()));
      }
    }

    this->skipWhitespace();
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
  std::string_view readUntilSep() {
    this->skipWhitespace();
    int64_t wordLen = peekUntil(this->sep);
    bool finished = this->finished();
    if (wordLen == 0 && !finished) {
      wordLen = 1;
    }
    std::string_view out(this->str + this->offset, wordLen);
    this->offset += wordLen;
    return out;
  }

  // Returns the text between the cursor and the next separator without advancing the cursor.
  std::string_view peekUntilSep() {
    this->skipWhitespace();
    int64_t wordLen = peekUntil(this->sep);
    return std::string_view(this->str + this->offset, wordLen);
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

        this->checkBuffer(n_chars + 2);
      }
    }

    if (!found && (this->offset + n_chars + 1) == this->length) {
      n_chars++;
    }

    return n_chars;
  }

  [[ noreturn ]] void errorBefore(std::string expected, std::string_view found) {
    throw BufferedParserException(expected, quote(found), this->errorContext(this->offset - found.size()));
  }

  [[noreturn]] void error(std::string expected, std::string_view found) {
    std::stringstream stream;
    stream << found;
    throw BufferedParserException(expected, stream.str(), this->errorContext(this->offset));
  }

  [[noreturn]] void error(const char* expected, std::string found) {
    std::stringstream stream;
    stream << found;
    throw BufferedParserException(expected, stream.str(), this->errorContext(this->offset));
  }

  [[noreturn]] void error(std::string expected) {
    throw BufferedParserException(expected, quote(this->peekUntilSep()), this->errorContext(this->offset));
  }

  std::string errorContext(int64_t pos) {
    std::stringstream stream;
    stream << " at byte " << (this->source_offset - this->length + pos);
    return stream.str();
  }

private:
  char* str;
  int64_t length;
  int64_t offset;
  int64_t buffer_length;
  int64_t source_offset;
  const char* whitespace;
  const char* sep;
  SimpleBufferSource* source;

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

  static std::string quote(std::string_view input) {
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


template <class SourceType>
class BufferedWKTParser: public BufferedParser<SourceType> {
public:

  BufferedWKTParser(int64_t buffer_size): BufferedParser<SourceType>(buffer_size) {
    this->setSeparators(" \r\n\t,();=");
  }

  wk_meta_t assertGeometryMeta() {
    wk_meta_t meta;
    WK_META_RESET(meta, WK_GEOMETRY);

    std::string_view geometry_type = this->assertWord();

    if (geometry_type == "SRID") {
      this->assert_('=');
      meta.srid = this->assertInteger();
      this->assert_(';');
      geometry_type = this->assertWord();
    }

    meta.geometry_type = this->geometry_typeFromString(geometry_type);

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

    return meta;
  }

  int geometry_typeFromString(std::string_view geometry_type) {
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
      std::string_view word = this->assertWord();
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


// The BufferedWKTReader is carefully designed to (1) avoid any virtual method calls
// (via templating) and (2) to avoid using any C++ objects with non-trivial destructors.
// The non-trivial destructors bit is important because handler methods can and do longjmp
// when used in R. The object itself does not have a non-trivial destructor and it's expected
// that the scope in which it is declared uses the proper unwind-protection such that the
// object and its members are deleted.
template <class SourceType>
class BufferedWKTReader {
public:

  BufferedWKTReader(wk_handler_t* handler, int64_t buffer_size): handler(handler), s(buffer_size) {}

  int readFeature(wk_vector_meta_t* meta, int64_t feat_id, SourceType* source) {
    try {
      int result;
      HANDLE_OR_RETURN(this->handler->feature_start(meta, feat_id, this->handler->handler_data));

      if (source == nullptr) {
        HANDLE_OR_RETURN(this->handler->null_feature(this->handler->handler_data));
      } else {
        s.setSource(source);
        HANDLE_OR_RETURN(this->readGeometryWithType(WK_PART_ID_NONE));
        s.assertFinished();
      }

      return this->handler->feature_end(meta, feat_id, this->handler->handler_data);
    } catch (BufferedParserException& e) {
      return this->handler->error(e.what(), this->handler->handler_data);
    }
  }

protected:

  int readGeometryWithType(uint32_t part_id) {
    wk_meta_t meta = s.assertGeometryMeta();
    int result;
    HANDLE_OR_RETURN(this->handler->geometry_start(&meta, part_id, this->handler->handler_data));

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
      throw std::runtime_error("Unknown geometry type"); // # nocov
    }

    return this->handler->geometry_end(&meta, part_id, this->handler->handler_data);
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

        HANDLE_OR_RETURN(this->handler->geometry_start(&childMeta, part_id, this->handler->handler_data));

        if (s.isEMPTY()) {
          s.assertWord();
        } else {
          HANDLE_OR_RETURN(this->readPointCoordinate(&childMeta));
        }
        HANDLE_OR_RETURN(this->handler->geometry_end(&childMeta, part_id, this->handler->handler_data));

        part_id++;
      } while (s.assertOneOf(",)") != ')');

    } else { // ((0 0), (1 1))
      do {
        childMeta = this->childMeta(meta, WK_POINT);
        HANDLE_OR_RETURN(this->handler->geometry_start(&childMeta, part_id, this->handler->handler_data));
        HANDLE_OR_RETURN(this->readPoint(&childMeta));
        HANDLE_OR_RETURN(this->handler->geometry_end(&childMeta, part_id, this->handler->handler_data));
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
      HANDLE_OR_RETURN(this->handler->geometry_start(&childMeta, part_id, this->handler->handler_data));
      HANDLE_OR_RETURN(this->readLineString(&childMeta));
      HANDLE_OR_RETURN(this->handler->geometry_end(&childMeta, part_id, this->handler->handler_data));

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
      HANDLE_OR_RETURN(this->handler->geometry_start(&childMeta, part_id, this->handler->handler_data));
      HANDLE_OR_RETURN(this->readPolygon(&childMeta));
      HANDLE_OR_RETURN(this->handler->geometry_end(&childMeta, part_id, this->handler->handler_data));
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
      HANDLE_OR_RETURN(this->handler->ring_start(meta, WK_SIZE_UNKNOWN, ring_id, this->handler->handler_data));
      HANDLE_OR_RETURN(this->readCoordinates(meta));
      HANDLE_OR_RETURN(this->handler->ring_end(meta, WK_SIZE_UNKNOWN, ring_id, this->handler->handler_data));
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
    HANDLE_OR_RETURN(handler->coord(meta, coord, 0, this->handler->handler_data));
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
      HANDLE_OR_RETURN(handler->coord(meta, coord, coord_id, this->handler->handler_data));

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
  wk_handler_t* handler;
  BufferedWKTParser<SourceType> s;
};

int handle_wkt_r_sting(BufferedWKTReader<SimpleBufferSource>& streamer, SimpleBufferSource* source,
                       wk_vector_meta_t* meta, cpp11::r_string item,
                       R_xlen_t feat_id) {  
  if (item == NA_STRING) {
    return streamer.readFeature(meta, feat_id, nullptr);
  } else {
    const char* chars = CHAR(item);
    source->set_buffer(chars, strlen(chars));
    return streamer.readFeature(meta, feat_id, source);
  }
}

void wkt_read_wkt_unsafe(SEXP wkt_sexp,
                         BufferedWKTReader<SimpleBufferSource>* reader,
                         SimpleBufferSource* source, wk_vector_meta_t* global_meta) {
  
  R_xlen_t n_features = Rf_xlength(wkt_sexp);
  SEXP item;
  int result;

  for (R_xlen_t i = 0; i < n_features; i++) {
    if (((i + 1) % 1000) == 0) R_CheckUserInterrupt();

    item = STRING_ELT(wkt_sexp, i);
    if (item == NA_STRING) {
      HANDLE_CONTINUE_OR_BREAK(reader->readFeature(global_meta, i, nullptr));
    } else {
      const char* chars = CHAR(item);
      source->set_buffer(chars, strlen(chars));
      HANDLE_CONTINUE_OR_BREAK(reader->readFeature(global_meta, i, source));
    }

    if (result == WK_ABORT) {
      break;
    }
  }
}

SEXP wkt_read_wkt(SEXP data, wk_handler_t* handler) {
  BEGIN_CPP11

  SEXP wkt_sexp = VECTOR_ELT(data, 0);
  SEXP buffer_size_sexp = VECTOR_ELT(data, 1);
  SEXP reveal_size_sexp = VECTOR_ELT(data, 2);
  int buffer_size = INTEGER(buffer_size_sexp)[0];
  int reveal_size = LOGICAL(reveal_size_sexp)[0];

  if (TYPEOF(wkt_sexp) != STRSXP) {
    cpp11::safe[Rf_error]("Input to wkt handler must be a character vector");
  }

  R_xlen_t n_features = Rf_xlength(wkt_sexp);

  wk_vector_meta_t global_meta;
  WK_VECTOR_META_RESET(global_meta, WK_GEOMETRY);
  global_meta.flags |= WK_FLAG_DIMS_UNKNOWN;
  if (reveal_size) {
    global_meta.size = n_features;
  }

  SimpleBufferSource source;
  BufferedWKTReader<SimpleBufferSource> reader(handler, buffer_size);

  int result = cpp11::safe[handler->vector_start](&global_meta, handler->handler_data);
  if (result != WK_ABORT) {
    cpp11::safe[wkt_read_wkt_unsafe](wkt_sexp, &reader, &source, &global_meta);
  }

  return cpp11::safe[handler->vector_end](&global_meta, handler->handler_data);

  END_CPP11
}

extern "C" SEXP wk_c_read_wkt(SEXP data, SEXP handler_xptr) {
  return wk_handler_run_xptr(&wkt_read_wkt, data, handler_xptr);
}
