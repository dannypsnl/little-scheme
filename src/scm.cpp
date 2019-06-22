#include "scm.h"

using scheme::Parser;
using scheme::Token;

Token::Token(Token::Type t) : type(t) {}
Token::Token(Token::Type t, std::string v) : type(t), value(v) {}
Token::Token(Token::Type t, double v) : type(t), value(v) {}

Parser::Parser(std::istream &c) : code(c) {}

Token Parser::next() {
  char c{};
  code.get(c);
  while (c == ' ' || c == '\n') {
    code.get(c);
  }
  switch (c) {
    case '(':
      return Token(Token::Type::LeftParen);
    case ')':
      return Token(Token::Type::RightParen);
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case '.': {
      code.putback(c);  // put number start back into stream
      double number_value{};
      code >> number_value;
      return Token(Token::Type::Number, number_value);
    }
    default: {
      std::string s{c};
      while (code.get(c)) {
        if (c == ' ' || c == '\n' || c == '(' || c == ')') {
          code.putback(c);
          break;
        } else {
          s += c;
        }
      }
      return Token(Token::Type::Identifier, s);
    }
  }
}
