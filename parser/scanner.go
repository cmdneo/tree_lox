package parser

import (
	"fmt"
	"os"
	"strconv"
	"tree_lox/token"
)

const EOF_CHAR = '\x00'

type Scanner struct {
	source  string
	start   int
	current int
	line    int
}

func MakeScanner(source string) Scanner {
	return Scanner{source: source, start: 0, current: 0, line: 1}
}

func (s *Scanner) NextToken() token.Token {
	// Skip blanks and comments.
	s.skipBlanks()
	// We use a loop since there can be multiple consecutive comments.
	for s.peek() == '/' && s.peekNext() == '/' {
		for !s.isAtEnd() && s.advance() != '\n' {
			// Consume the line.
		}
		s.skipBlanks()
	}

	s.start = s.current

	if s.isAtEnd() {
		return s.makeTok(token.END_OF_FILE)
	}

	c := s.advance()

	switch c {
	case '(':
		return s.makeTok(token.LEFT_PAREN)
	case ')':
		return s.makeTok(token.RIGHT_PAREN)
	case '{':
		return s.makeTok(token.LEFT_BRACE)
	case '}':
		return s.makeTok(token.RIGHT_BRACE)

	case '-':
		return s.makeTok(token.MINUS)
	case '+':
		return s.makeTok(token.PLUS)
	case '*':
		return s.makeTok(token.STAR)
	case '/':
		return s.makeTok(token.SLASH)

	case ',':
		return s.makeTok(token.COMMA)
	case '.':
		return s.makeTok(token.DOT)
	case ';':
		return s.makeTok(token.SEMICOLON)
	case ':':
		return s.makeTok(token.COLON)
	case '?':
		return s.makeTok(token.QUESTION)

	case '!':
		if s.match('=') {
			return s.makeTok(token.BANG_EQUAL)
		} else {
			return s.makeTok(token.BANG)
		}
	case '=':
		if s.match('=') {
			return s.makeTok(token.EQUAL_EQUAL)
		} else {
			return s.makeTok(token.EQUAL)
		}
	case '<':
		if s.match('=') {
			return s.makeTok(token.LESS_EQUAL)
		} else {
			return s.makeTok(token.LESS)
		}
	case '>':
		if s.match('=') {
			return s.makeTok(token.GREATER)
		} else {
			return s.makeTok(token.GREATER_EQUAL)
		}

	case '"':
		return s.do_string()
	}

	if isDigit(c) {
		return s.do_number()
	}

	if isIdentFirstChar(c) {
		return s.do_identifier()
	}

	return s.errorTok(fmt.Sprintf("Unknown character '%c' (%v).", c, c))
}

func (s *Scanner) do_string() token.Token {
	for !s.isAtEnd() {
		if s.advance() == '"' {
			tok := s.makeTok(token.STRING)
			tok.Literal = s.source[s.start+1 : s.current-1]
			return tok
		}
	}

	return s.errorTok("Unclosed string literal.")

}

func (s *Scanner) do_number() token.Token {
	for isDigit(s.peek()) {
		s.advance()
	}

	if s.peek() == '.' && isDigit(s.peekNext()) {
		s.advance() // Eat the '.'
	}

	for isDigit(s.peek()) {
		s.advance()
	}

	tok := s.makeTok(token.NUMBER)
	val, err := strconv.ParseFloat(tok.Lexeme, 64)
	if err != nil {
		return s.errorTok(fmt.Sprintf("Invalid number (%v)", err.Error()))
	}

	tok.Literal = val
	return tok
}

func (s *Scanner) do_identifier() token.Token {
	for isIdentChar(s.peek()) {
		s.advance()
	}

	tok := s.makeTok(token.IDENTIFIER)
	k := token.IDENTIFIER

	// Check if a keyword
	switch tok.Lexeme {
	case "var":
		k = token.VAR
	case "fun":
		k = token.FUN
	case "class":
		k = token.CLASS
	case "super":
		k = token.SUPER
	case "this":
		k = token.THIS
	case "if":
		k = token.IF
	case "else":
		k = token.ELSE
	case "while":
		k = token.WHILE
	case "for":
		k = token.FOR
	case "or":
		k = token.OR
	case "and":
		k = token.AND
	case "assert":
		k = token.ASSERT
	case "print":
		k = token.PRINT
	case "return":
		k = token.RETURN
	case "break":
		k = token.BREAK
	case "continue":
		k = token.CONTINUE
	case "nil":
		k = token.NIL
	case "true":
		k = token.TRUE
	case "false":
		k = token.FALSE
	}

	tok.Kind = k
	return tok
}

// Utility methods
// -----------------------------------------------
func (s *Scanner) skipBlanks() {
	for !s.isAtEnd() {
		switch s.peek() {
		case ' ', '\t', '\n', '\v':
			s.advance()
		default:
			return
		}
	}
}

func (s *Scanner) errorTok(message string) token.Token {
	err_tok := s.makeTok(token.INVALID)

	at := fmt.Sprintf("'%v'", err_tok.Lexeme)
	if s.isAtEnd() {
		at = "end"
	}

	fmt.Fprintf(os.Stderr, "[line %v] Error at %v: %v\n", s.line, at, message)
	return err_tok
}

func (s *Scanner) makeTok(kind token.TokenKind) token.Token {
	return token.Token{Lexeme: s.source[s.start:s.current], Kind: kind, Line: s.line}
}

// Scanner character matching and processing methods
// --------------------------------------------------------
func (s *Scanner) match(expected byte) bool {
	if s.peek() == expected {
		s.advance()
		return true
	} else {
		return false
	}
}

func (s *Scanner) peekNext() byte {
	if s.current+1 < len(s.source) {
		return s.source[s.current+1]
	} else {
		return EOF_CHAR
	}
}

func (s *Scanner) peek() byte {
	if !s.isAtEnd() {
		return s.source[s.current]
	} else {
		return EOF_CHAR
	}
}

func (s *Scanner) advance() byte {
	if s.isAtEnd() {
		return EOF_CHAR
	}

	ret := s.source[s.current]
	s.current++
	if ret == '\n' {
		s.line++
	}

	return ret
}

func (s *Scanner) isAtEnd() bool {
	return s.current == len(s.source)
}

// Character class functions
// --------------------------------------------------------
func isDigit(c byte) bool {
	return '0' <= c && c <= '9'
}

func isIdentChar(c byte) bool {
	return isIdentFirstChar(c) || isDigit(c)
}

func isIdentFirstChar(c byte) bool {
	return c == '_' ||
		'a' <= c && c <= 'z' ||
		'A' <= c && c <= 'Z'
}
