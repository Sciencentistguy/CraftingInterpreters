#include "lexer.h"

#include <cctype>

#include "main.h"

Lexer::Lexer(const std::string& source) : source{source} {
}

std::vector<Token> Lexer::scanTokens() {
    while (!isAtEnd()) {
        start = current;
        scanToken();
    }
    tokens.push_back(Token(TokenType::Eof, "", 0, line));
    return tokens;
}

bool Lexer::isAtEnd() const {
    return current >= source.length();
}

void Lexer::scanToken() {
    const char c = advance();
    switch (c) {
        case '(':
            addToken(TokenType::Left_paren);
            break;
        case ')':
            addToken(TokenType::Right_paren);
            break;
        case '{':
            addToken(TokenType::Left_brace);
            break;
        case '}':
            addToken(TokenType::Right_brace);
            break;
        case ',':
            addToken(TokenType::Comma);
            break;
        case '.':
            addToken(TokenType::Dot);
            break;
        case '+':
            addToken(TokenType::Plus);
            break;
        case '-':
            addToken(TokenType::Minus);
            break;
        case ';':
            addToken(TokenType::Semicolon);
            break;
        case '*':
            addToken(TokenType::Star);
            break;
        case '!':
            addToken(match('=') ? TokenType::Bang_equal : TokenType::Bang);
            break;
        case '=':
            addToken(match('=') ? TokenType::Equal_equal : TokenType::Equal);
            break;
        case '<':
            addToken(match('=') ? TokenType::Less_equal : TokenType::Less);
            break;
        case '>':
            addToken(match('=') ? TokenType::Greater_equal : TokenType::Greater);
            break;

        case '/':
            if (match('/')) {
                while (peek() != '\n' && !isAtEnd()) {
                    advance();
                }
            } else {
                addToken(TokenType::Slash);
            }
            break;

        case ' ':
        case '\r':
        case '\t':
            // Ignore whitespace.
            break;

        case '\n':
            line++;
            break;

        case '"':
            string();
            break;

        case '0':
        case '1':
        case '2':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            number();
            break;

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
        case '_':
            identifier();
            break;

        default:
            error(line, "Unexpected character '" + std::to_string(c) + "'.");
            break;
    }
}

char Lexer::advance() {
    current++;
    return source[current - 1];
}

void Lexer::addToken(const TokenType type) {
    addToken(type, std::any());
}

void Lexer::addToken(const TokenType type, const std::any& literal) {
    auto text = source.substr(start, (current - start));
    tokens.push_back(Token(type, text, literal, line));
}

bool Lexer::match(const char expected) {
    if (isAtEnd()) {
        return false;
    }
    if (source[current] != expected) {
        return false;
    }
    ++current;
    return true;
}

char Lexer::peek() const {
    if (isAtEnd()) {
        return '\0';
    }
    return source[current];
}

char Lexer::peekNext() const {
    if (current + 1 >= source.length()) {
        return '\0';
    }
    return source[current + 1];
}

void Lexer::string() {
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') {
            line++;
        }
        advance();
    }
    if (isAtEnd()) {
        error(line, "Unterminated string.");
        return;
    }
    advance();
    std::string value = source.substr(start + 1, (current - start) - 2);  // start + 1 current - 1
    addToken(TokenType::String, value);
}

void Lexer::number() {
    while (std::isdigit(peek())) {
        advance();
    }
    if (peek() == '.' && std::isdigit(peekNext())) {
        advance();
        while (std::isdigit(peek())) {
            advance();
        }
    }
    addToken(TokenType::Number, std::stod(source.substr(start, (current - start)).c_str()));
}

void Lexer::identifier() {
    while (std::isalnum(peek()) || peek() == '_') {
        advance();
    }
    std::string text = source.substr(start, (current - start));
    if (keywords.find(text) == keywords.end()) {
        // not a keyword
        addToken(TokenType::Identifier);
    } else {
        const TokenType type = keywords.at(text);
        addToken(type);
    }
}
