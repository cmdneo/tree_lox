package interpreter

import (
	"tree_lox/ast"
	"tree_lox/value"
)

type ExprPrinter struct{}

func (p ExprPrinter) print(e ast.Expr) string {
	result := e.Accept(ExprPrinter{})
	return string(result.(value.String))
}

func (p ExprPrinter) VisitAssignExpr(e *ast.Assign) value.Value {
	return parens("=", p.print(&e.Target), p.print(e.Expr))
}

func (p ExprPrinter) VisitTernaryExpr(e *ast.Ternary) value.Value {
	return parens(
		"?:",
		p.print(e.Condition),
		p.print(e.TrueExpr),
		p.print(e.FalseExpr),
	)
}

func (p ExprPrinter) VisitLogicalExpr(e *ast.Logical) value.Value {
	return parens(e.Operator.Lexeme, p.print(e.Left), p.print(e.Right))
}

func (p ExprPrinter) VisitBinaryExpr(e *ast.Binary) value.Value {
	return parens(e.Operator.Lexeme, p.print(e.Left), p.print(e.Right))
}

func (p ExprPrinter) VisitUnaryExpr(e *ast.Unary) value.Value {
	return parens(e.Operator.Lexeme, p.print(e.Right))
}

func (p ExprPrinter) VisitCallExpr(e *ast.Call) value.Value {
	// Put initial content before args
	args := []string{"()", p.print(e.Callee) + ":"}

	for _, arg := range e.Arguments {
		args = append(args, p.print(arg))
	}

	return parens(args...)

}

func (p ExprPrinter) VisitGetExpr(e *ast.Get) value.Value {
	return parens("get", p.print(e.Object), e.Name.Lexeme)
}

func (p ExprPrinter) VisitSetExpr(e *ast.Set) value.Value {
	return parens("get", p.print(e.Object), e.Name.Lexeme, p.print(e.Value))
}

func (p ExprPrinter) VisitSuperExpr(e *ast.Super) value.Value {
	return toS("super." + e.Method.Lexeme)
}

func (p ExprPrinter) VisitThisExpr(e *ast.This) value.Value {
	return toS("this")
}

func (p ExprPrinter) VisitGroupingExpr(e *ast.Grouping) value.Value {
	return parens("group", p.print(e.Expr))
}

func (p ExprPrinter) VisitLiteralExpr(e *ast.Literal) value.Value {
	return toS(e.Value.String())
}

func (p ExprPrinter) VisitVariableExpr(e *ast.Variable) value.Value {
	if e.Distance < 0 {
		return toS("gvar:" + e.Name.Lexeme)
	} else {
		return toS("lvar:" + e.Name.Lexeme)
	}
}

func parens(frags ...string) value.String {
	ret := "("

	for i, frag := range frags {
		ret += frag

		if i != len(frags)-1 {
			ret += " "
		} else {
			ret += ")"
		}
	}

	return value.String(ret)
}

// Convert string to value.String type.
// We need this since ExprVisitor functions cannot return string
// but the new-type String.
func toS(s string) value.String {
	return value.String(s)
}
