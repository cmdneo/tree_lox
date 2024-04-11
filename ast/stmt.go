package ast

import (
	"tree_lox/token"
)

type Stmt interface {
	Accept(StmtVisitor) ControlKind
}

type StmtVisitor interface {
	VisitBlockStmt(s *Block) ControlKind
	VisitExpressionStmt(s *Expression) ControlKind
	VisitPrintStmt(s *Print) ControlKind
	VisitAssertStmt(s *Assert) ControlKind
	VisitBreakStmt(s *Break) ControlKind
	VisitContinueStmt(s *Continue) ControlKind
	VisitReturnStmt(s *Return) ControlKind
	VisitIfStmt(s *If) ControlKind
	VisitForStmt(s *For) ControlKind
	VisitVarStmt(s *Var) ControlKind
	VisitFunctionStmt(s *Function) ControlKind
	VisitClassStmt(s *Class) ControlKind
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
	// Store a pointer to the function declaration, since we .
	Methods map[string]*Function
}

// Implement the Stmt interface for each statement type we have.
func (s *Block) Accept(v StmtVisitor) ControlKind      { return v.VisitBlockStmt(s) }
func (s *Expression) Accept(v StmtVisitor) ControlKind { return v.VisitExpressionStmt(s) }
func (s *Print) Accept(v StmtVisitor) ControlKind      { return v.VisitPrintStmt(s) }
func (s *Assert) Accept(v StmtVisitor) ControlKind     { return v.VisitAssertStmt(s) }
func (s *Break) Accept(v StmtVisitor) ControlKind      { return v.VisitBreakStmt(s) }
func (s *Continue) Accept(v StmtVisitor) ControlKind   { return v.VisitContinueStmt(s) }
func (s *Return) Accept(v StmtVisitor) ControlKind     { return v.VisitReturnStmt(s) }
func (s *If) Accept(v StmtVisitor) ControlKind         { return v.VisitIfStmt(s) }
func (s *For) Accept(v StmtVisitor) ControlKind        { return v.VisitForStmt(s) }
func (s *Var) Accept(v StmtVisitor) ControlKind        { return v.VisitVarStmt(s) }
func (s *Function) Accept(v StmtVisitor) ControlKind   { return v.VisitFunctionStmt(s) }
func (s *Class) Accept(v StmtVisitor) ControlKind      { return v.VisitClassStmt(s) }

// Makes a block from a list of statements
func NewBlock(statements ...Stmt) *Block {
	return &Block{Statements: statements}
}
