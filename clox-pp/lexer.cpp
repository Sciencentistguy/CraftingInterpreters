#include "lexer.h"

#include <cctype>
#include <cstring>

#include "token.h"
Lexer::Lexer(const std::string& source) : source{source}, start{&source[0]}, current{&source[0]}, line{1} {
}

Token Lexer::scanToken() {
    skipWhitespace();
    start = current;
    if (isAtEnd()) {
        return makeToken(TokenType::Eof);
    }
    char c = advance();
    switch (c) {
        case '(':
            return makeToken(TokenType::Left_paren);
        case ')':
            return makeToken(TokenType::Right_paren);
        case '{':
            return makeToken(TokenType::Left_brace);
        case '}':
            return makeToken(TokenType::Right_brace);
        case ';':
            return makeToken(TokenType::Semicolon);
        case ',':
            return makeToken(TokenType::Comma);
        case '.':
            return makeToken(TokenType::Dot);
        case '-':
            return makeToken(TokenType::Minus);
        case '+':
            return makeToken(TokenType::Plus);
        case '/':
            //            if (peekNext() == '/') {
            //                while (peek()!='\n' && !isAtEnd()) {advance();}
            //            }
            return makeToken(TokenType::Slash);
        case '*':
            return makeToken(TokenType::Star);
        case '!':
            return makeToken(match('=') ? TokenType::Bang_equal : TokenType::Bang);
        case '=':
            return makeToken(match('=') ? TokenType::Equal_equal : TokenType::Equal);
        case '<':
            return makeToken(match('=') ? TokenType::Less_equal : TokenType::Less);
        case '>':
            return makeToken(match('=') ? TokenType::Greater_equal : TokenType::Greater);
        case '"':
            while (peek() != '"' && !isAtEnd()) {
                if (peek() == '\n') {
                    ++line;
                }
                advance();
            }
            if (isAtEnd()) {
                return errorToken("Unterminated string.");
            }
            advance();
            return makeToken(TokenType::String);
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
            while (std::isdigit(peek())) {
                advance();
            }
            if (peek() == '.' && std::isdigit(peekNext())) {
                advance();
                while (std::isdigit(peek())) {
                    advance();
                }
            }
            return makeToken(TokenType::Number);

        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z':
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
        case '_':
            while (std::isalpha(peek()) || std::isdigit(peek()) || peek() == '_') {
                advance();
            }
            return makeToken(identifierType());
        default:
            return errorToken("Unexpected character.");
    }
}

const char* Lexer::getStart() const {
    return start;
}

const char* Lexer::getCurrent() const {
    return current;
}

int Lexer::getLine() const {
    return line;
}

bool Lexer::isAtEnd() const {
    return *current == '\0';
}

char Lexer::advance() {
    return *current++;
}

bool Lexer::match(char c) {
    if (isAtEnd()) {
        return false;
    }
    if (*current != c) {
        return false;
    }
    current++;
    return true;
}

void Lexer::skipWhitespace() {
    while (true) {
        char c = peek();
        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;

            case '\n':
                ++line;
                advance();
                break;

            case '/':
                if (peekNext() == '/') {
                    while (peek() != '\n' && !isAtEnd()) {
                        advance();
                    }
                } else {
                    return;
                }
                break;

            default:
                return;
        }
    }
}

char Lexer::peek() const {
    return *current;
}

char Lexer::peekNext() const {
    if (isAtEnd()) {
        return '\0';
    }
    return current[1];
}

Token Lexer::makeToken(TokenType type) const {
    return Token(type, *this);
}

Token Lexer::errorToken(const char* errorMsg) const {
    return Token(errorMsg, *this);
}

TokenType Lexer::identifierType() const {
    switch (*start) {
        case 'a':
            return checkKeyword(1, 2, "nd", TokenType::And);
        case 'c':
            return checkKeyword(1, 4, "lass", TokenType::Class);
        case 'e':
            return checkKeyword(1, 3, "lse", TokenType::Else);
        case 'f':
            if (current - start > 1) {
                switch (start[1]) {
                    case 'a':
                        return checkKeyword(2, 3, "lse", TokenType::False);
                    case 'o':
                        return checkKeyword(2, 1, "r", TokenType::For);
                    case 'u':
                        return checkKeyword(2, 1, "n", TokenType::Fun);
                }
            }
            break;
        case 'i':
            return checkKeyword(1, 1, "f", TokenType::If);
        case 'n':
            return checkKeyword(1, 2, "il", TokenType::Nil);
        case 'o':
            return checkKeyword(1, 1, "r", TokenType::Or);
        case 'p':
            return checkKeyword(1, 4, "rint", TokenType::Print);
        case 'r':
            return checkKeyword(1, 5, "eturn", TokenType::Return);
        case 's':
            return checkKeyword(1, 4, "uper", TokenType::Super);
        case 't':
            if (current - start > 1) {
                switch (start[1]) {
                    case 'h':
                        return checkKeyword(2, 2, "is", TokenType::This);
                    case 'r':
                        return checkKeyword(2, 2, "ue", TokenType::True);
                }
            }
            break;
        case 'v':
            return checkKeyword(1, 2, "ar", TokenType::Var);
        case 'w':
            return checkKeyword(1, 4, "hile", TokenType::While);
    }
    return TokenType::Identifier;
}

TokenType Lexer::checkKeyword(int start, int length, const char* rest, TokenType type) const {
    if (this->current - this->start == start + length && std::memcmp(this->start + start, rest, length) == 0) {
        return type;
    }
    return TokenType::Identifier;
}
bool Lexer::isEmpty() const {
    return source.empty();
}
void Lexer::setSource(const std::string& source) {

    this->source = source;
    start = source.c_str();
    current = start;
}
