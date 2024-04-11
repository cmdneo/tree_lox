package ast

import (
	"tree_lox/token"
	"tree_lox/value"
)

type Expr interface {
	Accept(ExprVisitor) value.Value
}

type ExprVisitor interface {
	VisitAssignExpr(e *Assign) value.Value
	VisitTernaryExpr(e *Ternary) value.Value
	VisitLogicalExpr(e *Logical) value.Value
	VisitBinaryExpr(e *Binary) value.Value
	VisitUnaryExpr(e *Unary) value.Value
	VisitCallExpr(e *Call) value.Value
	VisitGetExpr(e *Get) value.Value
	VisitSetExpr(e *Set) value.Value
	VisitSuperExpr(e *Super) value.Value
	VisitThisExpr(e *This) value.Value
	VisitGroupingExpr(e *Grouping) value.Value
	VisitLiteralExpr(e *Literal) value.Value
	VisitVariableExpr(e *Variable) value.Value
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
	Value value.Value
}

// Implement the Expr interface for each expression type we have.
func (e *Assign) Accept(v ExprVisitor) value.Value   { return v.VisitAssignExpr(e) }
func (e *Ternary) Accept(v ExprVisitor) value.Value  { return v.VisitTernaryExpr(e) }
func (e *Logical) Accept(v ExprVisitor) value.Value  { return v.VisitLogicalExpr(e) }
func (e *Binary) Accept(v ExprVisitor) value.Value   { return v.VisitBinaryExpr(e) }
func (e *Unary) Accept(v ExprVisitor) value.Value    { return v.VisitUnaryExpr(e) }
func (e *Call) Accept(v ExprVisitor) value.Value     { return v.VisitCallExpr(e) }
func (e *Get) Accept(v ExprVisitor) value.Value      { return v.VisitGetExpr(e) }
func (e *Set) Accept(v ExprVisitor) value.Value      { return v.VisitSetExpr(e) }
func (e *Super) Accept(v ExprVisitor) value.Value    { return v.VisitSuperExpr(e) }
func (e *This) Accept(v ExprVisitor) value.Value     { return v.VisitThisExpr(e) }
func (e *Grouping) Accept(v ExprVisitor) value.Value { return v.VisitGroupingExpr(e) }
func (e *Literal) Accept(v ExprVisitor) value.Value  { return v.VisitLiteralExpr(e) }
func (e *Variable) Accept(v ExprVisitor) value.Value { return v.VisitVariableExpr(e) }
