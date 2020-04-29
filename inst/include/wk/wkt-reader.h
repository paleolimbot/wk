
#ifndef WK_WKT_READER_H
#define WK_WKT_READER_H

#include <string>
#include <clocale>
#include <iostream>

#include "wk/reader.h"
#include "wk/io-string.h"
#include "wk/formatter.h"
#include "wk/geometry-handler.h"
#include "wk/string-tokenizer.h"
#include "wk/parse-exception.h"
#include "wk/coord.h"


class WKTReader: public WKReader {
public:

  WKTReader(WKStringProvider& provider, WKGeometryHandler& handler):
    WKReader(provider, handler), provider(provider) {
    // TODO evaluate if we need this if we use C++11's double parser
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

  void readFeature(size_t featureId) {
    const std::string& wellKnownText = this->provider.featureString();
    WKStringTokenizer tokenizer(wellKnownText);

    handler.nextFeatureStart(featureId);
    this->readGeometryTaggedText(&tokenizer, PART_ID_NONE);
    handler.nextFeatureEnd(featureId);
  }

  ~WKTReader() {
    std::setlocale(LC_NUMERIC, saved_locale.c_str());
  }

protected:
  WKStringProvider& provider;

  void readGeometryTaggedText(WKStringTokenizer* tokenizer, uint32_t partId) {
    std::string type = this->getNextWord(tokenizer);
    int geometryType;

    if(type == "POINT") {
      geometryType = WKGeometryType::Point;

    } else if(type == "LINESTRING") {
      geometryType = WKGeometryType::LineString;

    } else if(type == "POLYGON") {
      geometryType = WKGeometryType::Polygon;

    } else if(type == "MULTIPOINT") {
      geometryType = WKGeometryType::MultiPoint;

    } else if(type == "MULTILINESTRING") {
      geometryType = WKGeometryType::MultiLineString;

    } else if(type == "MULTIPOLYGON") {
      geometryType = WKGeometryType::MultiPolygon;

    } else if(type == "GEOMETRYCOLLECTION") {
      geometryType = WKGeometryType::GeometryCollection;

    } else {
      throw WKParseException(Formatter() << "Unknown type " << type);
    }

    WKGeometryMeta meta = this->getNextEmptyOrOpener(tokenizer, geometryType);
    this->readGeometry(tokenizer, meta, partId);
  }

  void readGeometry(WKStringTokenizer* tokenizer, const WKGeometryMeta meta, uint32_t partId) {
    handler.nextGeometryStart(meta, partId);

    // if empty, calling read* functions will fail becausse
    // the empty token has been consumed
    if (meta.size == 0) {
      handler.nextGeometryEnd(meta, partId);
      return;
    }

    switch (meta.geometryType) {

    case WKGeometryType::Point:
      this->readPointText(tokenizer, meta);
      break;

    case WKGeometryType::LineString:
      this->readLineStringText(tokenizer, meta);
      break;

    case WKGeometryType::Polygon:
      this->readPolygonText(tokenizer, meta);
      break;

    case WKGeometryType::MultiPoint:
      this->readMultiPointText(tokenizer, meta);
      break;

    case WKGeometryType::MultiLineString:
      this->readMultiLineStringText(tokenizer, meta);
      break;

    case WKGeometryType::MultiPolygon:
      this->readMultiPolygonText(tokenizer, meta);
      break;

    case WKGeometryType::GeometryCollection:
      this->readGeometryCollectionText(tokenizer, meta);
      break;

    default:
      throw WKParseException(
          Formatter() <<
            "Unrecognized geometry type: " <<
            meta.geometryType
      );
    }

    handler.nextGeometryEnd(meta, partId);
  }

  void readPointText(WKStringTokenizer* tokenizer, const WKGeometryMeta meta) {
    this->readCoordinate(tokenizer, meta, 0);
    this->getNextCloser(tokenizer);
  }

  void readLineStringText(WKStringTokenizer* tokenizer, const WKGeometryMeta meta) {
    this->readCoordinates(tokenizer, meta);
  }

  void readPolygonText(WKStringTokenizer* tokenizer, const WKGeometryMeta meta) {
    uint32_t ringId = 0;
    this->readLinearRingText(tokenizer, meta, ringId);
    ringId++;

    std::string nextToken;
    this->getNextCloserOrComma(tokenizer);
    while(nextToken == ",") {
      this->readLinearRingText(tokenizer, meta, ringId);
      ringId++;
      nextToken = this->getNextCloserOrComma(tokenizer);
    }
  }

  void readLinearRingText(WKStringTokenizer* tokenizer, const WKGeometryMeta meta, uint32_t ringId) {
    handler.nextLinearRingStart(meta, WKGeometryMeta::SIZE_UNKNOWN, ringId);
    this->getNextEmptyOrOpener(tokenizer, 0);
    this->readCoordinates(tokenizer, meta);
    handler.nextLinearRingEnd(meta, WKGeometryMeta::SIZE_UNKNOWN, ringId);
  }

  void readMultiPointText(WKStringTokenizer* tokenizer, const WKGeometryMeta meta) {

    int tok = tokenizer->peekNextToken();

    // the next token can be EMPTY, which we only
    // deal with as the 'correct' multipoint form
    std::string nextWord;
    if (tok == WKStringTokenizer::TT_WORD) {
      nextWord = tokenizer->getSVal();
    }

    if (tok == WKStringTokenizer::TT_NUMBER) {
      // Try to parse deprecated form "MULTIPOINT(0 0, 1 1)"
      std::string nextToken;
      uint32_t partId = 0;
      do {
        WKGeometryMeta childMeta(WKGeometryType::Point, meta.hasZ, meta.hasM, meta.hasSRID);
        childMeta.srid = meta.srid;

        handler.nextGeometryStart(childMeta, partId);
        this->readCoordinate(tokenizer, meta, 0);
        handler.nextGeometryEnd(childMeta, partId);
        partId++;

        nextToken = this->getNextCloserOrComma(tokenizer);
      } while (nextToken == ",");

    } else if(tok == '(' || (tok == WKStringTokenizer::TT_WORD && nextWord == "EMPTY")) {
      // Try to parse correct form "MULTIPOINT((0 0), (1 1))"
      std::string nextToken;
      uint32_t partId = 0;
      do {
        WKGeometryMeta childMeta = this->getNextEmptyOrOpener(tokenizer, WKGeometryType::Point);
        childMeta.hasZ = meta.hasZ;
        childMeta.hasM = meta.hasM;
        childMeta.srid = meta.srid;

        this->readGeometry(tokenizer, childMeta, partId);
        partId++;

        nextToken = getNextCloserOrComma(tokenizer);
      } while (nextToken == ",");

    } else {
      std::stringstream err;
      err << "Unexpected token: ";
      switch(tok) {
      case WKStringTokenizer::TT_WORD:
        err << "WORD " << nextWord;
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

  void readMultiLineStringText(WKStringTokenizer* tokenizer, const WKGeometryMeta meta) {
    std::string nextToken;
    uint32_t partId = 0;
    do {
      WKGeometryMeta childMeta = this->getNextEmptyOrOpener(tokenizer, WKGeometryType::LineString);
      childMeta.hasZ = meta.hasZ;
      childMeta.hasM = meta.hasM;
      childMeta.srid = meta.srid;

      this->readGeometry(tokenizer, childMeta, partId);
      partId++;

      nextToken = this->getNextCloserOrComma(tokenizer);
    } while (nextToken == ",");
  }

  void readMultiPolygonText(WKStringTokenizer* tokenizer, const WKGeometryMeta meta) {
    std::string nextToken;
    uint32_t partId = 0;
    do {
      WKGeometryMeta childMeta = this->getNextEmptyOrOpener(tokenizer, WKGeometryType::Polygon);
      childMeta.hasZ = meta.hasZ;
      childMeta.hasM = meta.hasM;
      childMeta.srid = meta.srid;

      this->readGeometry(tokenizer, childMeta, partId);
      partId++;

      nextToken = this->getNextCloserOrComma(tokenizer);
    } while (nextToken == ",");
  }

  void readGeometryCollectionText(WKStringTokenizer* tokenizer, const WKGeometryMeta meta) {
    std::string nextToken;

    uint32_t partId = 0;
    do {
      this->readGeometryTaggedText(tokenizer, partId);
      partId++;

      nextToken = this->getNextCloserOrComma(tokenizer);
    } while (nextToken == ",");
  }

  void readCoordinates(WKStringTokenizer* tokenizer, const WKGeometryMeta meta) {
    std::string nextToken;

    uint32_t coordId = 0;
    this->readCoordinate(tokenizer, meta, coordId);
    coordId++;
    nextToken = this->getNextCloserOrComma(tokenizer);
    while(nextToken == ",") {
      this->readCoordinate(tokenizer, meta, coordId);
      coordId++;
      nextToken = this->getNextCloserOrComma(tokenizer);
    }
  }

  void readCoordinate(WKStringTokenizer* tokenizer, const WKGeometryMeta meta, uint32_t coordId) {
    WKCoord coord;
    coord.x = this->getNextNumber(tokenizer);
    coord.y = this->getNextNumber(tokenizer);

    if (this->isNumberNext(tokenizer)) {
      if (meta.hasZ) {
        coord.z = this->getNextNumber(tokenizer);
        coord.hasZ = true;
      } else if (meta.hasM) {
        coord.m = this->getNextNumber(tokenizer);
        coord.hasM = true;
      } else {
        throw WKParseException(Formatter() << "Found unexpected coordiate " << this->getNextNumber(tokenizer));
      }

      if(this->isNumberNext(tokenizer)) {
        if (meta.hasM) {
          coord.m = this->getNextNumber(tokenizer);
          coord.hasM = true;
        } else {
          throw WKParseException(Formatter() << "Found unexpected coordiate " << this->getNextNumber(tokenizer));
        }
      } else if (meta.hasZ && meta.hasM) {
        throw WKParseException("Expected M coordinate but foound ','  or ')'");
      }
    } else if (meta.hasZ || meta.hasM) {
      throw WKParseException("Expected Z or M coordinate");
    }

    handler.nextCoordinate(meta, coord, coordId);
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
      throw WKParseException(Formatter() << "Expected number but encountered word " << tokenizer->getSVal());
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

  WKGeometryMeta getNextEmptyOrOpener(WKStringTokenizer* tokenizer, int geometryType) {
    std::string nextWord = this->getNextWord(tokenizer);

    bool hasZ = false;
    bool hasM = false;
    bool hasSRID = false;

    // Parse the Z, M or ZM of an SF1.2 3/4 dim coordinate.
    if (nextWord == "Z") {
      hasZ = true;
      nextWord = this->getNextWord(tokenizer);
    } else if (nextWord == "M") {
      hasM = true;
      nextWord = this->getNextWord(tokenizer);
    } else if (nextWord == "ZM") {
      hasZ = true;
      hasM = true;
      nextWord = this->getNextWord(tokenizer);
    }

    WKGeometryMeta meta = WKGeometryMeta(geometryType, hasZ, hasM, hasSRID);
    if(nextWord == "EMPTY") {
      meta.hasSize = true;
      meta.size = 0;
      return meta;

    } else if (nextWord == "(") {
      return meta;

    } else {
      throw WKParseException(
          Formatter() <<
            "Expected 'Z', 'M', 'ZM', 'EMPTY' or '(' but encountered " <<
              nextWord
      );
    }
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

  bool isNumberNext(WKStringTokenizer* tokenizer) {
    return tokenizer->peekNextToken() == WKStringTokenizer::TT_NUMBER;
  }

private:
  std::string saved_locale;

};

#endif
