#include <gtest/gtest.h>
#include <sstream>
#include <vector>
#include "scm.h"

using scheme::Token;
using ::testing::get;

TEST(Parser, Tokens) {
  std::stringstream ss{"(+ (* 1 2) a)"};
  scheme::Parser parser{ss};
  scheme::Token token = parser.next();
  ASSERT_EQ(token.type, Token::Type::LeftParen);  // (
  token = parser.next();
  ASSERT_EQ(token.type, Token::Type::Identifier);  // +
  token = parser.next();
  ASSERT_EQ(token.type, Token::Type::LeftParen);  // (
  token = parser.next();
  ASSERT_EQ(token.type, Token::Type::Identifier);  // *
  token = parser.next();
  ASSERT_EQ(token.type, Token::Type::Number);  // 1
  token = parser.next();
  ASSERT_EQ(token.type, Token::Type::Number);  // 2
  token = parser.next();
  ASSERT_EQ(token.type, Token::Type::RightParen);  // )
  token = parser.next();
  ASSERT_EQ(token.type, Token::Type::Identifier);  // a
  token = parser.next();
  ASSERT_EQ(token.type, Token::Type::RightParen);  // )
}
