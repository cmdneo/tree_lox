package ast

import (
	"tree_lox/token"
)

type Stmt interface {
	Accept(StmtVisitor)
}

type StmtVisitor interface {
	VisitBlockStmt(s Block)
	VisitExpressionStmt(s Expression)
	VisitPrintStmt(s Print)
	VisitAssertStmt(s Assert)
	VisitBreakStmt(s Break)
	VisitContinueStmt(s Continue)
	VisitReturnStmt(s Return)
	VisitIfStmt(s If)
	VisitForStmt(s For)
	VisitVarStmt(s Var)
	VisitFunctionStmt(s Function)
	VisitClassStmt(s Class)
}

type Block struct {
	Statements []Stmt
}

type Expression struct {
	Expression Expr
}

type Print struct {
	Expression Expr
}

type Assert struct {
	Keyword    token.Token
	Expression Expr
}

type Break struct {
	Keyword token.Token
}

type Continue struct {
	Keyword token.Token
}

type Return struct {
	Keyword token.Token
	Value   Expr
}

type If struct {
	Condition  Expr
	ThenBranch Stmt
	ElseBranch Stmt // Can be nil
}

type For struct {
	Condition Expr
	Body      Stmt
	// For supporting 'for' loops with 'continue' statements we use a
	// seperate field 'Update', since the continue statement skips all
	// statements below it but we need to execute the update expression
	// at the end of each iteration.
	// We use the construction:
	//     { initializer; for(condition, update_expr) body_stmt }
	// Where 'update_expr' is evaluated at the end of each iteration.
	Update Expr
}

type Var struct {
	Name        token.Token
	Initializer Expr
}

type Function struct {
	Name   token.Token
	Params []token.Token
	Body   []Stmt
}

type Class struct {
	Name       token.Token
	Superclass *Variable // Can be nil
	Methods    map[string]Function
}

// Implement the Stmt interface for each statement type we have.
func (s Block) Accept(v StmtVisitor)      { v.VisitBlockStmt(s) }
func (s Expression) Accept(v StmtVisitor) { v.VisitExpressionStmt(s) }
func (s Print) Accept(v StmtVisitor)      { v.VisitPrintStmt(s) }
func (s Assert) Accept(v StmtVisitor)     { v.VisitAssertStmt(s) }
func (s Break) Accept(v StmtVisitor)      { v.VisitBreakStmt(s) }
func (s Continue) Accept(v StmtVisitor)   { v.VisitContinueStmt(s) }
func (s Return) Accept(v StmtVisitor)     { v.VisitReturnStmt(s) }
func (s If) Accept(v StmtVisitor)         { v.VisitIfStmt(s) }
func (s For) Accept(v StmtVisitor)        { v.VisitForStmt(s) }
func (s Var) Accept(v StmtVisitor)        { v.VisitVarStmt(s) }
func (s Function) Accept(v StmtVisitor)   { v.VisitFunctionStmt(s) }
func (s Class) Accept(v StmtVisitor)      { v.VisitClassStmt(s) }

// Makes a block from a list of statements
func MakeBlock(statements ...Stmt) Block {
	return Block{Statements: statements}
}
