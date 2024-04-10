package ast

import (
	"tree_lox/token"
)

type Expr interface {
	Accept(ExprVisitor) any
}

type ExprVisitor interface {
	VisitAssignExpr(e Assign) any
	VisitTernaryExpr(e Ternary) any
	VisitLogicalExpr(e Logical) any
	VisitBinaryExpr(e Binary) any
	VisitUnaryExpr(e Unary) any
	VisitCallExpr(e Call) any
	VisitGetExpr(e Get) any
	VisitSetExpr(e Set) any
	VisitSuperExpr(e Super) any
	VisitThisExpr(e This) any
	VisitGroupingExpr(e Grouping) any
	VisitLiteralExpr(e Literal) any
	VisitVariableExpr(e Variable) any
}

type Assign struct {
	Target Variable
	Expr   Expr
}

type Ternary struct {
	Condition           Expr
	TrueExpr, FalseExpr Expr
}

type Logical struct {
	Operator    token.Token
	Left, Right Expr
}

type Binary struct {
	Operator    token.Token
	Left, Right Expr
}

type Unary struct {
	Operator token.Token
	Right    Expr
}

type Call struct {
	Callee    Expr
	Paren     token.Token
	Arguments []Expr
}

type Get struct {
	Object Expr
	Name   token.Token
}

type Set struct {
	Object Expr
	Name   token.Token
	Value  Expr
}

// super, this, grouping, variable and literal are primary expressions.

type Super struct {
	Method   token.Token
	Variable Variable // Always a local variable.
}

type This struct {
	Variable Variable // Always a local variable.
}

type Grouping struct {
	Expr Expr
}

type Variable struct {
	Name token.Token
	// Scope distance of the local variable from its delcaration, -1 if global.
	Distance int
	// Slot index in the environment if a local variable.
	Slot int
}

type Literal struct {
	Value any
}

// Implement the Expr interface for each expression type we have.
func (e Assign) Accept(v ExprVisitor) any   { return v.VisitAssignExpr(e) }
func (e Ternary) Accept(v ExprVisitor) any  { return v.VisitTernaryExpr(e) }
func (e Logical) Accept(v ExprVisitor) any  { return v.VisitLogicalExpr(e) }
func (e Binary) Accept(v ExprVisitor) any   { return v.VisitBinaryExpr(e) }
func (e Unary) Accept(v ExprVisitor) any    { return v.VisitUnaryExpr(e) }
func (e Call) Accept(v ExprVisitor) any     { return v.VisitCallExpr(e) }
func (e Get) Accept(v ExprVisitor) any      { return v.VisitGetExpr(e) }
func (e Set) Accept(v ExprVisitor) any      { return v.VisitSetExpr(e) }
func (e Super) Accept(v ExprVisitor) any    { return v.VisitSuperExpr(e) }
func (e This) Accept(v ExprVisitor) any     { return v.VisitThisExpr(e) }
func (e Grouping) Accept(v ExprVisitor) any { return v.VisitGroupingExpr(e) }
func (e Literal) Accept(v ExprVisitor) any  { return v.VisitLiteralExpr(e) }
func (e Variable) Accept(v ExprVisitor) any { return v.VisitVariableExpr(e) }
