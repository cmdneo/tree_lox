package interpreter

import (
	"tree_lox/ast"
	"tree_lox/object"
)

type ExprPrinter struct{}

func (p ExprPrinter) print(e ast.Expr) string {
	return e.Accept(ExprPrinter{}).(string)
}

func (p ExprPrinter) VisitAssignExpr(e ast.Assign) any {
	return parens("=", p.print(e.Target), p.print(e.Expr))
}

func (p ExprPrinter) VisitTernaryExpr(e ast.Ternary) any {
	return parens(
		"?:",
		p.print(e.Condition),
		p.print(e.TrueExpr),
		p.print(e.FalseExpr),
	)
}

func (p ExprPrinter) VisitLogicalExpr(e ast.Logical) any {
	return parens(e.Operator.Lexeme, p.print(e.Left), p.print(e.Right))
}

func (p ExprPrinter) VisitBinaryExpr(e ast.Binary) any {
	return parens(e.Operator.Lexeme, p.print(e.Left), p.print(e.Right))

}

func (p ExprPrinter) VisitUnaryExpr(e ast.Unary) any {
	return parens(e.Operator.Lexeme, p.print(e.Right))
}

func (p ExprPrinter) VisitCallExpr(e ast.Call) any {
	// Put initial content before args
	args := []string{"()", p.print(e.Callee) + ":"}

	for _, arg := range e.Arguments {
		args = append(args, p.print(arg))
	}

	return parens(args...)

}

func (p ExprPrinter) VisitGetExpr(e ast.Get) any {
	return parens("get", p.print(e.Object), e.Name.Lexeme)
}

func (p ExprPrinter) VisitSetExpr(e ast.Set) any {
	return parens("get", p.print(e.Object), e.Name.Lexeme, p.print(e.Value))
}

func (p ExprPrinter) VisitSuperExpr(e ast.Super) any {
	return "super." + e.Keyword.Lexeme
}

func (p ExprPrinter) VisitThisExpr(e ast.This) any {
	return "this"
}

func (p ExprPrinter) VisitGroupingExpr(e ast.Grouping) any {
	return parens("group", p.print(e.Expr))
}

func (p ExprPrinter) VisitLiteralExpr(e ast.Literal) any {
	return object.AsString(e)
}

func (p ExprPrinter) VisitVariableExpr(e ast.Variable) any {
	if e.Distance < 0 {
		return "gvar:" + e.Name.Lexeme
	} else {
		return "lvar:" + e.Name.Lexeme
	}
}

func parens(frags ...string) string {
	ret := "("

	for i, frag := range frags {
		ret += frag

		if i != len(frags)-1 {
			ret += " "
		} else {
			ret += ")"
		}
	}

	return ret
}
