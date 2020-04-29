
#ifndef WK_WKT_READER_H
#define WK_WKT_READER_H


#include <string>
#include <clocale>

#include "wk/formatter.h"
#include "wk/geometry-handler.h"
#include "wk/string-tokenizer.h"
#include "wk/parse-exception.h"
#include "wk/coord.h"


class WKTReader {
public:

  WKTReader(WKGeometryHandler& handler): handler(handler) {

#ifdef _MSC_VER
    // Avoid multithreading issues caused by setlocale
    _configthreadlocale(_ENABLE_PER_THREAD_LOCALE);
#endif
    char* p = std::setlocale(LC_NUMERIC, nullptr);
    if(nullptr != p) {
      this->saved_locale = p;
    }
    std::setlocale(LC_NUMERIC, "C");
  }

  void read(const std::string& wellKnownText) {
    WKStringTokenizer tokenizer(wellKnownText);
    this->readGeometryTaggedText(&tokenizer);
  }

  ~WKTReader() {
    std::setlocale(LC_NUMERIC, saved_locale.c_str());
  }

protected:
  WKGeometryHandler& handler;

  std::vector<WKCoord> readCoordinates(WKStringTokenizer* tokenizer) {
    std::string nextToken = this->getNextEmptyOrOpener(tokenizer);
    std::vector<WKCoord> v;
    if(nextToken == "EMPTY") {
      return v;
    }

    v.push_back(this->readCoordinate(tokenizer));

    nextToken = this->getNextCloserOrComma(tokenizer);
    while(nextToken == ",") {
      v.push_back(this->readCoordinate(tokenizer));
      nextToken = this->getNextCloserOrComma(tokenizer);
    }

    return v;
  }

  double getNextNumber(WKStringTokenizer* tokenizer) {
    int type = tokenizer->nextToken();
    switch(type) {
    case WKStringTokenizer::TT_NUMBER:
      return tokenizer->getNVal();
    case WKStringTokenizer::TT_EOF:
      throw WKParseException("Expected number but encountered end of stream");
    case WKStringTokenizer::TT_EOL:
      throw WKParseException("Expected number but encountered end of line");
    case WKStringTokenizer::TT_WORD:
      throw WKParseException(Formatter() << "Expected number but encountered word" << tokenizer->getSVal());
    case '(':
      throw WKParseException("Expected number but encountered '('");
    case ')':
      throw WKParseException("Expected number but encountered ')'");
    case ',':
      throw WKParseException("Expected number but encountered ','");
    default:
      throw std::runtime_error(Formatter() << "getNextNumber(): Unexpected token type " << type);
    }
  }

  std::string getNextEmptyOrOpener(WKStringTokenizer* tokenizer) {
    std::string nextWord = this->getNextWord(tokenizer);

    // Skip the Z, M or ZM of an SF1.2 3/4 dim coordinate.
    // TODO we're going to need this in a sec
    if(nextWord == "Z" || nextWord == "M" || nextWord == "ZM") {
      nextWord = this->getNextWord(tokenizer);
    }

    if(nextWord == "EMPTY" || nextWord == "(") {
      return nextWord;
    }
    throw WKParseException(
        Formatter() <<
          "Expected 'Z', 'M', 'ZM', 'EMPTY' or '(' but encountered " <<
          nextWord
    );
  }

  std::string getNextCloserOrComma(WKStringTokenizer* tokenizer) {
    std::string nextWord = this->getNextWord(tokenizer);
    if(nextWord == "," || nextWord == ")") {
      return nextWord;
    }

    throw  WKParseException(Formatter() << "Expected ')' or ',' but encountered" << nextWord);
  }

  std::string getNextCloser(WKStringTokenizer* tokenizer) {
    std::string nextWord = getNextWord(tokenizer);
    if(nextWord == ")") {
      return nextWord;
    }

    throw WKParseException(Formatter() << "Expected ')' but encountered" << nextWord);
  }

  std::string getNextWord(WKStringTokenizer* tokenizer) {
    int type = tokenizer->nextToken();
    switch(type) {
    case WKStringTokenizer::TT_WORD: {
      std::string word = tokenizer->getSVal();
      int i = static_cast<int>(word.size());
      while(--i >= 0) {
        word[i] = static_cast<char>(toupper(word[i]));
      }
      return word;
    }
    case WKStringTokenizer::TT_EOF:
      throw WKParseException("Expected word but encountered end of stream");
    case WKStringTokenizer::TT_EOL:
      throw WKParseException("Expected word but encountered end of line");
    case WKStringTokenizer::TT_NUMBER:
      throw WKParseException(Formatter() << "Expected word but encountered number" << tokenizer->getNVal());
    case '(':
      return "(";
    case ')':
      return ")";
    case ',':
      return ",";
    default:
      throw std::runtime_error(Formatter() << "Unexpected token type " << type);
    }
  }

  void readGeometryTaggedText(WKStringTokenizer* tokenizer) {
    std::string type = this->getNextWord(tokenizer);

    if(type == "POINT") {
      return this->readPointText(tokenizer);

    } else if(type == "LINESTRING") {
      return this->readLineStringText(tokenizer);

    } else if(type == "LINEARRING") {
      return this->readLinearRingText(tokenizer);

    } else if(type == "POLYGON") {
      return this->readPolygonText(tokenizer);

    } else if(type == "MULTIPOINT") {
      return this->readMultiPointText(tokenizer);

    } else if(type == "MULTILINESTRING") {
      return this->readMultiLineStringText(tokenizer);

    } else if(type == "MULTIPOLYGON") {
      return this->readMultiPolygonText(tokenizer);

    } else if(type == "GEOMETRYCOLLECTION") {
      return this->readGeometryCollectionText(tokenizer);

    } else {
      throw WKParseException(Formatter() << "Unknown type" << type);
    }
  }

  void readPointText(WKStringTokenizer* tokenizer) {
    std::string nextToken = this->getNextEmptyOrOpener(tokenizer);
    if(nextToken == "EMPTY") {
      // TODO need this in a sec
      return;
    }

    this->readCoordinate(tokenizer);
    this->getNextCloser(tokenizer);
  }

  void readLineStringText(WKStringTokenizer* tokenizer) {
    this->readCoordinates(tokenizer);
  }

  void readLinearRingText(WKStringTokenizer* tokenizer) {
    this->readCoordinates(tokenizer);
  }

  void readMultiPointText(WKStringTokenizer* tokenizer) {
    std::string nextToken = getNextEmptyOrOpener(tokenizer);
    if (nextToken == "EMPTY") {
      return;
    }

    int tok = tokenizer->peekNextToken();

    if (tok == WKStringTokenizer::TT_NUMBER) {

      // Try to parse deprecated form "MULTIPOINT(0 0, 1 1)"
      std::vector<WKCoord> coords;

      do {
        coords.push_back(this->readCoordinate(tokenizer));
        nextToken = this->getNextCloserOrComma(tokenizer);
      } while (nextToken == ",");

    } else if(tok == '(') {
      // Try to parse correct form "MULTIPOINT((0 0), (1 1))"

      do {
        this->readPointText(tokenizer);
        nextToken = getNextCloserOrComma(tokenizer);
      } while (nextToken == ",");

    } else {
      std::stringstream err;
      err << "Unexpected token: ";
      switch(tok) {
      case WKStringTokenizer::TT_WORD:
        err << "WORD " << tokenizer->getSVal();
        break;
      case WKStringTokenizer::TT_NUMBER:
        err << "NUMBER " << tokenizer->getNVal();
        break;
      case WKStringTokenizer::TT_EOF:
      case WKStringTokenizer::TT_EOL:
        err << "EOF or EOL";
        break;
      case '(':
        err << "(";
        break;
      case ')':
        err << ")";
        break;
      case ',':
        err << ",";
        break;
      default:
        err << "??";
        break;
      }

      err << std::endl;
      throw WKParseException(err.str());
    }
  }

  void readPolygonText(WKStringTokenizer* tokenizer) {
    std::string nextToken = this->getNextEmptyOrOpener(tokenizer);
    if(nextToken == "EMPTY") {
      return;
    }

    this->readLinearRingText(tokenizer);
    nextToken = this->getNextCloserOrComma(tokenizer);
    while(nextToken == ",") {
      this->readLinearRingText(tokenizer);
      nextToken = this->getNextCloserOrComma(tokenizer);
    }
  }

  void readMultiLineStringText(WKStringTokenizer* tokenizer) {
    std::string nextToken = this->getNextEmptyOrOpener(tokenizer);
    if(nextToken == "EMPTY") {
      return;
    }

    do {
      this->readLineStringText(tokenizer);
      nextToken = this->getNextCloserOrComma(tokenizer);
    } while (nextToken == ",");
  }

  void readMultiPolygonText(WKStringTokenizer* tokenizer) {
    std::string nextToken = this->getNextEmptyOrOpener(tokenizer);
    if(nextToken == "EMPTY") {
      return;
    }

    do {
      this->readPolygonText(tokenizer);
      nextToken = this->getNextCloserOrComma(tokenizer);
    } while (nextToken == ",");
  }

  void readGeometryCollectionText(WKStringTokenizer* tokenizer) {
    std::string nextToken = this->getNextEmptyOrOpener(tokenizer);
    if(nextToken == "EMPTY") {
      return;
    }

    do {
      this->readGeometryTaggedText(tokenizer);
      nextToken = this->getNextCloserOrComma(tokenizer);
    } while (nextToken == ",");
  }

private:
  std::string saved_locale;

  const WKCoord readCoordinate(WKStringTokenizer* tokenizer) {
    WKCoord coord;
    coord.x = this->getNextNumber(tokenizer);
    coord.y = this->getNextNumber(tokenizer);

    if (this->isNumberNext(tokenizer)) {
      coord.z = this->getNextNumber(tokenizer);
      coord.hasZ = true;

      if(this->isNumberNext(tokenizer)) {
        coord.m = this->getNextNumber(tokenizer);
        coord.hasM = true;
      }

    }

    return coord;
  }

  bool isNumberNext(WKStringTokenizer* tokenizer) {
    return tokenizer->peekNextToken() == WKStringTokenizer::TT_NUMBER;
  }
};

#endif
