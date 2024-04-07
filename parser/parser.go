package parser

import (
	"fmt"
	"os"
	"tree_lox/ast"
	"tree_lox/token"
	"tree_lox/util"
)

const MAX_CALL_PARAMS = 255

type Parser struct {
	// Scanning information
	scn      Scanner
	previous token.Token
	current  token.Token

	// Current scope information
	scopes []localScope
	// Current class type
	currentClass classKind
	// Current function type
	currentFunction functionKind
	// Current loop kind along with it update clause(for 'fors' loop).
	currentLoop loopKind

	hadError bool
}

type SyntaxError struct{}

func MakeParser(source string) Parser {
	return Parser{
		scn:      MakeScanner(source),
		previous: token.Token{},
		current:  token.Token{},
		// Parsing state info
		scopes:          make([]localScope, 0, 8),
		currentClass:    kindNoClass,
		currentFunction: kindNoFunction,
		currentLoop:     kindNoLoop,
	}
}

func (p *Parser) Parse() []ast.Stmt {
	// Prime the parser: take in first token.
	p.advance()

	stmts := make([]ast.Stmt, 0)
	for !p.check(token.END_OF_FILE) {
		func() {
			// Synchronize tokens if malformed syntax is detected.
			defer func() {
				if v := recover(); v != nil {
					p.synchronize()
				}
			}()

			stmts = append(stmts, p.declaration())
		}()
	}

	if p.hadError {
		return nil
	} else {
		return stmts
	}
}

// Statement parsing methods
// --------------------------------------------------------
func (p *Parser) declaration() ast.Stmt {
	switch {
	case p.match(token.CLASS):
		return p.classDeclaration()
	case p.match(token.FUN):
		return p.function(kindFunction)
	case p.match(token.VAR):
		return p.varDeclaration()

	default:
		return p.statement()
	}
}

func (p *Parser) classDeclaration() ast.Stmt {
	name := p.consume(token.IDENTIFIER, "Expect class name.")

	p.declareVariable()
	p.defineVariable() // A class can refer to itself.

	superclass := (*ast.Variable)(nil)
	if p.match(token.LESS) {
		sname := p.consume(token.IDENTIFIER, "Expect superclass name.")
		p.useVariable(sname)
		if sname.Lexeme == name.Lexeme {
			p.error("A class cannot inherit from itself.")
			// Continue after the error as the syntax is well formed.
		} else {
			sp := p.useVariable(sname)
			superclass = &sp
		}
	}

	// Track if inside a class.
	old_class := p.currentClass
	p.currentClass = kindClass
	if superclass != nil {
		p.currentClass = kindSubclass
	}
	defer func() { p.currentClass = old_class }()

	p.consume(token.LEFT_BRACE, "Expect '{' before class body.")

	ret := ast.Class{Name: name, Superclass: superclass}

	for !p.check(token.RIGHT_BRACE) && !p.check(token.END_OF_FILE) {
		// If multiple methods have the same name then the last one is taken.
		// Class constructor is named 'init'.
		kind := kindMethod
		if p.check(token.IDENTIFIER) && p.current.Lexeme == "init" {
			kind = kindInitializer
		}
		ret.Methods[p.current.Lexeme] = p.function(kind)
	}

	p.consume(token.RIGHT_BRACE, "Expect '}' after class body.")
	return &ret
}

// For functions, methods and initializers.
func (p *Parser) function(kind functionKind) ast.Function {
	// Track if inside a function.
	old_func := p.currentFunction
	p.currentFunction = kind
	defer func() { p.currentFunction = old_func }()

	kind_str := kind.String()
	name := p.consume(token.IDENTIFIER, "Expect "+kind_str+" name.")

	p.declareVariable()
	p.defineVariable() // A function can refer to itself inside it.

	// Begin function scope, function parameters inside the function's scope.
	p.pushScope()
	defer p.popScope()

	p.consume(token.LEFT_PAREN, "Expect '(' after "+kind_str+" name.")
	params := make([]token.Token, 0)

	if !p.check(token.RIGHT_PAREN) {
		for {
			if len(params) >= MAX_CALL_PARAMS {
				p.error_at(p.current, fmt.Sprintf(
					"Can't have more than %v arguments.", MAX_CALL_PARAMS,
				))
			}
			// Continue ever after the error as the syntax is well formed.

			param := p.consume(token.IDENTIFIER, "Expect parameter name.")
			params = append(params, param)
			p.declareVariable()
			p.defineVariable()

			if !p.match(token.COMMA) {
				break
			}
		}
	}
	p.consume(token.RIGHT_PAREN, "Expect ')' after parameters.")

	p.consume(token.LEFT_BRACE, "Expect '{' before "+kind_str+" body.")
	body := p.bare_block()

	return ast.Function{Name: name, Params: params, Body: body}
}

func (p *Parser) varDeclaration() ast.Stmt {
	name := p.consume(token.IDENTIFIER, "Expect a variable name.")
	p.declareVariable()

	init_value := ast.Expr(ast.Literal{Value: nil})

	if p.match(token.EQUAL) {
		init_value = p.expression()
	}
	// A variable is defined only after its initialization is complete.
	p.defineVariable()

	p.consume(token.SEMICOLON, "Expect ';' after variable declaration.")
	return ast.Var{Name: name, Initializer: init_value}
}

func (p *Parser) statement() ast.Stmt {
	switch {
	case p.match(token.ASSERT):
		return p.assertStatement()
	case p.match(token.PRINT):
		return p.printStatement()

	case p.match(token.BREAK):
		return p.breakStatement()
	case p.match(token.CONTINUE):
		return p.continueStatement()
	case p.match(token.RETURN):
		return p.returnStatement()

	case p.match(token.IF):
		return p.ifStatement()
	case p.match(token.WHILE):
		return p.whileStatement()
	case p.match(token.FOR):
		return p.forStatement()

	case p.match(token.LEFT_BRACE):
		return p.block()

	default:
		return p.expressionStatement()
	}
}

func (p *Parser) assertStatement() ast.Stmt {
	keyword := p.previous
	expr := p.expression()
	p.consume(token.SEMICOLON, "Expect ';' after expression.")

	return ast.Assert{Expression: expr, Keyword: keyword}
}

func (p *Parser) printStatement() ast.Stmt {
	expr := p.expression()
	p.consume(token.SEMICOLON, "Expect ';' after expression.")

	return ast.Print{Expression: expr}

}

func (p *Parser) breakStatement() ast.Stmt {
	kw := p.previous
	p.consume(token.SEMICOLON, "Expect ';' after 'break'.")

	return ast.Break{Keyword: kw}
}

func (p *Parser) continueStatement() ast.Stmt {
	kw := p.previous
	p.consume(token.SEMICOLON, "Expect ';' after 'continue'.")

	return ast.Continue{Keyword: kw}
}

func (p *Parser) returnStatement() ast.Stmt {
	kw := p.previous
	value := ast.Expr(nil) // A return with no expression returns nil.

	if !p.check(token.SEMICOLON) {
		value = p.expression()
		p.consume(token.SEMICOLON, "Expect ';' after return value.")
	} else {
		p.consume(token.SEMICOLON, "Expect ';' after return.")
	}

	return ast.Return{Keyword: kw, Value: value}
}

func (p *Parser) ifStatement() ast.Stmt {
	p.consume(token.LEFT_PAREN, "Expect '(' after 'if'.")
	condition := p.expression()
	p.consume(token.RIGHT_PAREN, "Expect ')' after condition.")

	then_branch := p.statement()
	else_branch := ast.Stmt(nil)
	if p.match(token.ELSE) {
		else_branch = p.statement()
	}

	return ast.If{
		Condition:  condition,
		ThenBranch: then_branch,
		ElseBranch: else_branch,
	}
}

func (p *Parser) whileStatement() ast.Stmt {
	// Track if inside a loop.
	old_loop := p.currentLoop
	p.currentLoop = kindWhileLoop
	defer func() { p.currentLoop = old_loop }()

	p.consume(token.LEFT_PAREN, "Expect '(' after 'while'.")
	condition := p.expression()
	p.consume(token.RIGHT_PAREN, "Expect ')' after condition.")

	body := p.statement()

	// A while loop is just a for loop with no update expression and
	// initializer. Like so: for (condition) body_stmt
	return ast.For{Condition: condition, Body: body, Update: nil}
}

func (p *Parser) forStatement() ast.Stmt {
	// Track if inside a loop.
	old_loop := p.currentLoop
	p.currentLoop = kindForLoop
	defer func() { p.currentLoop = old_loop }()

	// The 'for' loop has a seperate scope for the variable(if any)
	// in its initializer clause and the update clause.
	//
	// We do the following construction:
	// { initializer; while(condition, update_expr) body_stmt }
	p.pushScope() // Push a new scope for the initializer as above.
	defer p.popScope()

	p.consume(token.LEFT_PAREN, "Expect '(' after 'for'.")

	init := ast.Stmt(nil)
	switch {
	case p.match(token.SEMICOLON):
		init = nil
	case p.match(token.VAR):
		init = p.varDeclaration()
	default:
		init = p.expressionStatement()
	}

	cond := ast.Expr(ast.Literal{Value: true})
	if !p.check(token.SEMICOLON) {
		cond = p.expression()
	}
	p.consume(token.SEMICOLON, "Expect ';' after loop condition.")

	update := ast.Expr(nil)
	if !p.check(token.RIGHT_PAREN) {
		update = p.expression()
	}
	p.consume(token.RIGHT_PAREN, "Expect ')' after for-clauses.")

	body := p.statement()

	for_loop := ast.For{
		Condition: cond,
		Update:    update,
		Body:      body,
	}
	return ast.MakeBlock(init, for_loop)
}

func (p *Parser) block() ast.Stmt {
	p.pushScope()
	defer p.popScope()

	return ast.MakeBlock(p.bare_block()...)
}

func (p *Parser) expressionStatement() ast.Stmt {
	expr := p.expression()
	p.consume(token.SEMICOLON, "Expect ';' after expression.")

	return ast.Expression{Expression: expr}
}

// Expression parsing methods
// --------------------------------------------------------
func (p *Parser) expression() ast.Expr {
	return p.assignment()
}

func (p *Parser) assignment() ast.Expr {
	// Since the '=' can be any number of tokens ahead,
	// parse the LHS first and then check for equal sign and verify that the
	// assingment target valid.
	expr := p.ternary()

	if p.match(token.EQUAL) {
		equals := p.previous
		value := p.assignment()

		switch target := expr.(type) {
		case ast.Variable:
			return ast.Assign{Target: target, Expr: value}
		case ast.Get:
			// If Get(like: expr.name) then transform it into Set.
			// Where the name is the property to be set.
			return ast.Set{
				Object: target.Object,
				Name:   target.Name,
				Value:  value,
			}
		default:
			p.error_at(equals, "Invalid assingment target.")
			// Continue after the error as the syntax is well formed.
		}
	}

	return expr
}

func (p *Parser) ternary() ast.Expr {
	expr := p.logic_or()

	if p.match(token.QUESTION) {
		true_expr := p.expression()
		p.consume(token.COLON, "Expect colon in ternary expression.")
		false_expr := p.ternary()

		return ast.Ternary{
			Condition: expr,
			TrueExpr:  true_expr,
			FalseExpr: false_expr,
		}
	}

	return expr
}

// Generic helper function for parsing left-associative binary expressions.
func do_left_binary_expr[E ast.Binary | ast.Logical](
	p *Parser, next_rule func() ast.Expr, matches ...token.TokenKind) ast.Expr {
	left := next_rule()

	for p.match_any(matches...) {
		op := p.previous
		right := next_rule()

		left = ast.Expr(E{Operator: op, Left: left, Right: right})
	}

	return left
}

func (p *Parser) logic_or() ast.Expr {
	return do_left_binary_expr[ast.Logical](p, p.logic_and, token.OR)
}

func (p *Parser) logic_and() ast.Expr {
	return do_left_binary_expr[ast.Logical](p, p.equality, token.AND)
}

func (p *Parser) equality() ast.Expr {
	return do_left_binary_expr[ast.Binary](p, p.comparison,
		token.EQUAL_EQUAL, token.BANG_EQUAL)
}

func (p *Parser) comparison() ast.Expr {
	return do_left_binary_expr[ast.Binary](p, p.term,
		token.LESS, token.LESS_EQUAL, token.GREATER, token.GREATER_EQUAL)
}

func (p *Parser) term() ast.Expr {
	return do_left_binary_expr[ast.Binary](p, p.factor,
		token.PLUS, token.MINUS)
}

func (p *Parser) factor() ast.Expr {
	return do_left_binary_expr[ast.Binary](p, p.unary,
		token.STAR, token.SLASH)
}

func (p *Parser) unary() ast.Expr {
	if p.match_any(token.BANG, token.PLUS, token.MINUS) {
		op := p.previous
		right := p.unary()
		return ast.Unary{Operator: op, Right: right}
	}

	return p.call()
}

func (p *Parser) call() ast.Expr {
	// This parses function calls and get(property access),
	// both are left-associative.
	expr := p.primary()

	for {
		if p.match(token.DOT) {
			name := p.consume(token.IDENTIFIER, "Expect property name after '.'.")
			expr = ast.Get{Object: expr, Name: name}
		} else if p.match(token.LEFT_PAREN) {
			expr = p.finish_call(expr)
		} else {
			break
		}
	}

	return expr
}

func (p *Parser) primary() ast.Expr {
	switch {
	case p.match(token.FALSE):
		return ast.Literal{Value: false}
	case p.match(token.TRUE):
		return ast.Literal{Value: true}
	case p.match(token.NIL):
		return ast.Literal{Value: nil}

	case p.match(token.THIS):
		return ast.This{Keyword: p.previous}

	case p.match(token.SUPER):
		// Any usage'super' must access a method of the superclass.
		kword := p.previous
		p.consume(token.DOT, "Expect '.' after super.")
		method := p.consume(token.IDENTIFIER, "Expect superclass method name.")
		return ast.Super{Keyword: kword, Method: method}

	case p.match_any(token.NUMBER, token.STRING):
		return ast.Literal{Value: p.previous.Literal}

	case p.match(token.IDENTIFIER):
		return p.useVariable(p.previous)

	case p.match(token.LEFT_PAREN):
		expr := p.expression()
		p.consume(token.RIGHT_PAREN, "Expect ')' after expression.")
		return ast.Grouping{Expr: expr}

	}

	p.error_at(p.current, "Expect expression.")
	panic(SyntaxError{})
}

// Parsing helpers
// --------------------------------------------------------
// Parses: declaration* '}', does not do scope management.
func (p *Parser) bare_block() []ast.Stmt {
	stmts := make([]ast.Stmt, 0)

	for !p.check(token.RIGHT_BRACE) && !p.check(token.END_OF_FILE) {
		stmts = append(stmts, p.declaration())
	}

	p.consume(token.RIGHT_BRACE, "Expect '}' after block.")

	return stmts
}

// Parses: expr? (',' expr)* ')'
func (p *Parser) finish_call(callee ast.Expr) ast.Call {
	args := make([]ast.Expr, 0)

	if !p.check(token.RIGHT_PAREN) {
		for {
			if len(args) >= MAX_CALL_PARAMS {
				p.error_at(p.current, fmt.Sprintf(
					"Can't have more than %v arguments.", MAX_CALL_PARAMS,
				))
			}
			// Continue after the error as the syntax is well formed.

			args = append(args, p.expression())

			if !p.match(token.COMMA) {
				break
			}
		}
	}

	paren := p.consume(token.RIGHT_PAREN, "Expect ')' after arguments.")
	return ast.Call{Callee: callee, Paren: paren, Arguments: args}
}

// Variable and scope management
// --------------------------------------------------------
func (p *Parser) pushScope() {
	p.scopes = append(p.scopes, makeLocalScope(len(p.scopes)+1))
}

func (p *Parser) popScope() {
	util.Pop(&p.scopes)
}

// Declares the variable(last consumed token) in the current local scope.
// Print and set error if the variable already existed in the scope.
func (p *Parser) declareVariable() {
	name := p.previous

	// If global then do nothing
	if len(p.scopes) == 0 {
		return
	}

	slot, _ := util.Last(p.scopes).getVariable(name.Lexeme)

	if slot < 0 {
		util.Last(p.scopes).putVariable(name.Lexeme)
		return
	}

	p.error(
		"Variable with name '%v' already exists in the scope.",
		name.Lexeme,
	)
}

func (p *Parser) defineVariable() {
	// If global then do nothing
	if len(p.scopes) == 0 {
		return
	}

	util.Last(p.scopes).markDefined()
}

// Determines if the variable being referred to is global or local and
// returns its Variable object with information to resolve it at runtime.
func (p *Parser) useVariable(name token.Token) ast.Variable {
	// Determine if local a variable or a global variable.
	for i := range p.scopes {
		// Reversed, inside out traversal.
		at := len(p.scopes) - i - 1

		if slot, def := p.scopes[at].getVariable(name.Lexeme); slot >= 0 {
			if !def {
				p.error_at(name, "Cannot read variable in its own initializer.")
				// Continue after the error as the syntax is well formed.
			}

			return ast.Variable{Name: p.previous, Distance: i, Slot: slot}
		}
	}

	return ast.Variable{Name: p.previous, Distance: -1, Slot: -1}
}

// Error reporting and recovery methods
// --------------------------------------------------------
func (p *Parser) error(format string, args ...any) {
	p.error_at(p.previous, format, args...)
}

func (p *Parser) error_at(tok token.Token, message string, args ...any) {
	p.hadError = true

	at := "'" + tok.Lexeme + "'"
	if tok.Kind == token.END_OF_FILE {
		at = "end"
	}

	fmt.Fprintf(os.Stderr, "[line %v] Error at %v: ", tok.Line, at)
	fmt.Fprintf(os.Stderr, message+"\n", args...)
}

func (p *Parser) synchronize() {
	// Discard token on whic error happened and continue to do so until we
	// find a token which might be the begining of a new statement/declaration.
	p.advance()

	for p.current.Kind != token.END_OF_FILE {
		// If a statement or block has ended then we might see a new statement.
		switch p.previous.Kind {
		case token.SEMICOLON, token.RIGHT_BRACE:
			return
		}

		// Or if we see a token which is begining of a statement.
		switch p.current.Kind {
		case token.LEFT_BRACE, token.CLASS, token.FUN, token.VAR,
			token.FOR, token.IF, token.WHILE,
			token.RETURN, token.PRINT, token.ASSERT:
			return

		default:
			p.advance()
		}
	}
}

// Parser token matching and processing methods
// --------------------------------------------------------
func (p *Parser) consume(kind token.TokenKind, message string) token.Token {
	if p.check(kind) {
		return p.advance()
	}

	p.error(message)
	panic(SyntaxError{})
}

func (p *Parser) match_any(kinds ...token.TokenKind) bool {
	for _, kind := range kinds {
		if p.check(kind) {
			p.advance()
			return true
		}
	}

	return false
}

func (p *Parser) match(kind token.TokenKind) bool {
	if p.check(kind) {
		p.advance()
		return true
	}

	return false
}

func (p *Parser) check(kind token.TokenKind) bool {
	return p.current.Kind == kind
}

func (p *Parser) advance() token.Token {
	p.previous = p.current
	p.current = p.scn.NextToken()
	return p.previous
}
