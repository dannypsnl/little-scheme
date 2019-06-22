#pragma once

#include <exception>
#include <iostream>
#include <istream>
#include <sstream>
#include <string>
#include <variant>
#include <vector>

namespace scheme {

struct Token {
  enum class Type {
    LeftParen,   // (
    RightParen,  // )
    // float<system arch>
    Number,
    // basically mean except whitespace | comment | newline
    Identifier,
    Bad
  };
  Type type;
  std::variant<std::string, double> value;

  Token(Type t);
  Token(Type t, std::string v);
  Token(Type t, double v);
};

class Parser {
  std::istream &code;

 public:
  Parser(std::istream &c);

  Token next();
};

}  // namespace scheme
