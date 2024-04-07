package interpreter

import (
	"fmt"
	"os"
	"tree_lox/ast"
	"tree_lox/object"
	"tree_lox/token"
	"tree_lox/util"
)

type Interpreter struct {
	// Global variables
	globals map[string]any
	// Local variable scopes
	localEnv *object.LocalEnv
	// Functions we are currently inside.
	calledFunctions []string
	// Current stack distance from the error site.
	errorDistance int
}

type runtimeError struct{}

func MakeInterpreter() Interpreter {
	return Interpreter{
		globals:  map[string]any{},
		localEnv: nil,
		// The top-level implicit function is named '<script>'.
		calledFunctions: []string{"<script>"},
		errorDistance:   0,
	}
}

func (i *Interpreter) Interpret(statements []ast.Stmt) {
	defer func() {
		switch err := recover().(type) {
		case nil:
		case runtimeError:
			// Just handle it, the error messages when the error is made and
			// further stack trace by VisitCallExpr via panic recovery.
		default:
			panic(err)
		}
	}()

	// Discard all previous local environments (if any) left due to
	// any error in the code executed.
	i.localEnv = nil
	// i.calledFunctions = i.calledFunctions[:1]
	i.errorDistance = 0

	for _, stmt := range statements {
		i.execute(stmt)
	}
}

// Statement evaluators
// --------------------------------------------------------
func (i *Interpreter) VisitBlockStmt(s ast.Block) {
	i.executeBlock(s.Statements, object.MakeLocalEnv(i.localEnv))
}

func (i *Interpreter) VisitExpressionStmt(s ast.Expression) {
	i.evaluate(s.Expression)
}

func (i *Interpreter) VisitPrintStmt(s ast.Print) {
	fmt.Printf("%v\n", i.evaluate(s.Expression))
}

func (i *Interpreter) VisitAssertStmt(s ast.Assert) {
	if !object.Truthiness(i.evaluate(s.Expression)) {
		panic(i.makeError(s.Keyword, "Assertion failure."))
	}
}

func (i *Interpreter) VisitBreakStmt(s ast.Break) {
	panic(controlBreak{})
}

func (i *Interpreter) VisitContinueStmt(s ast.Continue) {
	panic(controlContinue{})
}

func (i *Interpreter) VisitReturnStmt(s ast.Return) {
	value := any(nil)
	if s.Value != nil {
		value = i.evaluate(s.Value)
	}

	panic(controlReturn{Value: value})
}

func (i *Interpreter) VisitIfStmt(s ast.If) {
	if object.Truthiness(i.evaluate(s.Condition)) {
		i.execute(s.ThenBranch)
	} else if s.ElseBranch != nil {
		i.execute(s.ElseBranch)
	}
}

func (i *Interpreter) VisitForStmt(s ast.For) {
	// Handle break
	defer func() {
		switch r := recover().(type) {
		case nil:
		case controlBreak:
		default:
			panic(r)
		}
	}()

	for object.Truthiness(i.evaluate(s.Condition)) {
		func() {
			// Handle 'continue' statement and run the update clause.
			defer func() {
				switch r := recover().(type) {
				case nil:
				case controlContinue:
				default:
					panic(r)
				}

				if s.Update != nil {
					i.evaluate(s.Update)
				}
			}()

			i.execute(s.Body)
		}()
	}
}

func (i *Interpreter) VisitVarStmt(s ast.Var) {
	val := i.evaluate(s.Initializer)
	i.defineVariable(s.Name.Lexeme, val)
}

func (i *Interpreter) VisitFunctionStmt(s ast.Function) {
	fun := object.Function{Declaration: s, Enclosing: i.localEnv}
	i.defineVariable(s.Name.Lexeme, fun)
}

func (i *Interpreter) VisitClassStmt(s ast.Class) {
	// TODO
	panic("Undone!")
}

// Expression evaluators
// --------------------------------------------------------
func (i *Interpreter) VisitAssignExpr(e ast.Assign) any {
	val := i.evaluate(e.Expr)

	if e.Target.Distance < 0 {
		name := e.Target.Name.Lexeme

		if _, exists := i.globals[name]; exists {
			i.globals[name] = val
		} else {
			panic(i.makeError(e.Target.Name, "Undefined variable '%v'.", name))
		}
	} else {
		i.localEnv.AssignAt(e.Target.Slot, val, e.Target.Distance)
	}

	return val
}

func (i *Interpreter) VisitTernaryExpr(e ast.Ternary) any {
	truth := object.Truthiness(i.evaluate(e.Condition))

	if truth {
		return i.evaluate(e.TrueExpr)
	} else {
		return i.evaluate(e.FalseExpr)
	}
}

func (i *Interpreter) VisitLogicalExpr(e ast.Logical) any {
	left := i.evaluate(e.Left)

	// Return the value of the expression which determines the truth value of
	// the logical expression and not a boolean, similar to what python does.
	switch e.Operator.Kind {
	case token.OR:
		if object.Truthiness(left) {
			return left
		}

	case token.AND:
		if !object.Truthiness(left) {
			return left
		}

	default:
		panic("Invalid operator in logical expression.")
	}

	return i.evaluate(e.Right)

}

// Checks if both are of the type given.
func hasType[T any](a, b any) bool {
	_, e := a.(T)
	_, f := b.(T)
	return e && f
}

func (i *Interpreter) VisitBinaryExpr(e ast.Binary) any {
	left := i.evaluate(e.Left)
	right := i.evaluate(e.Right)

	check_nums := func() {
		if hasType[float64](left, right) {
			return
		}
		panic(i.makeError(e.Operator,
			"Operands must be either two strings or two numbers."))
	}

	check_num_or_str := func() {
		if hasType[float64](left, right) || hasType[string](left, right) {
			return
		}
		panic(i.makeError(e.Operator, "Operands must numbers."))
	}

	switch e.Operator.Kind {
	case token.PLUS:
		check_num_or_str()
		return object.Add(left, right)
	case token.MINUS:
		check_nums()
		return object.Sub(left, right)
	case token.STAR:
		check_nums()
		return object.Mul(left, right)
	case token.SLASH:
		check_nums()
		return object.Div(left, right)

	case token.GREATER:
		check_num_or_str()
		return object.GreaterThan(left, right)
	case token.GREATER_EQUAL:
		check_num_or_str()
		return object.GreaterThan(left, right) || object.EqualTo(left, right)

	case token.LESS:
		check_num_or_str()
		return object.LessThan(left, right)
	case token.LESS_EQUAL:
		check_num_or_str()
		return object.LessThan(left, right) || object.EqualTo(left, right)

	case token.EQUAL_EQUAL:
		return object.EqualTo(left, right)
	case token.BANG_EQUAL:
		return !object.EqualTo(left, right)

	default:
		panic("Invalid operator token in binary expression.")
	}
}

func (i *Interpreter) VisitUnaryExpr(e ast.Unary) any {
	right := i.evaluate(e.Right)

	check_num := func() {
		if hasType[float64](right, float64(0)) {
			return
		}
		panic(i.makeError(e.Operator, "Operand must be a number."))
	}

	switch e.Operator.Kind {
	case token.BANG:
		return !object.Truthiness(right)

	case token.PLUS:
		check_num()
		return right
	case token.MINUS:
		check_num()
		return object.Neg(right)

	default:
		panic("Invalid operator token in unary expression.")
	}
}

func (i *Interpreter) VisitCallExpr(e ast.Call) any {
	var ret_val any = nil

	fun, ok := i.evaluate(e.Callee).(object.Function)

	if !ok {
		panic(i.makeError(e.Paren, "Can only call function and classes."))
	}

	if fun.Arity() != len(e.Arguments) {
		panic(i.makeError(
			e.Paren, "Expected %v arguments but got %v arguments.",
			fun.Arity(), len(e.Arguments),
		))
	}

	func() {
		// Extract return value and pop off the called function name.
		// If any runtime erro happens then
		defer func() {
			util.Pop(&i.calledFunctions)

			switch r := recover().(type) {
			case nil:
			case controlReturn:
				ret_val = r.Value
			case runtimeError:
				i.errorDistance++
				// Print the call site(line and caller) of the function.
				printLocation(
					i.errorDistance, e.Paren.Line,
					*util.Last(i.calledFunctions),
				)

				panic(r) // re-throw the error
			default:
				panic(r)
			}

		}()

		// Push the function name for stack trace generation.
		i.calledFunctions = append(i.calledFunctions, fun.Declaration.Name.Lexeme)

		// Evaluate arguments and put it inside the function's environment.
		fun_env := object.MakeLocalEnv(fun.Enclosing)
		for _, arg := range e.Arguments {
			fun_env.PushVariable(i.evaluate(arg))
		}

		i.executeBlock(fun.Declaration.Body, fun_env)
	}()

	return ret_val
}

func (i *Interpreter) VisitGetExpr(e ast.Get) any {
	// TODO
	panic("Undone!")
}

func (i *Interpreter) VisitSetExpr(e ast.Set) any {
	// TODO
	panic("Undone!")
}

func (i *Interpreter) VisitSuperExpr(e ast.Super) any {
	// TODO
	panic("Undone!")
}

func (i *Interpreter) VisitThisExpr(e ast.This) any {
	// TODO
	panic("Undone!")
}

func (i *Interpreter) VisitGroupingExpr(e ast.Grouping) any {
	// TODO
	return i.evaluate(e.Expr)
}

func (i *Interpreter) VisitLiteralExpr(e ast.Literal) any {
	return e.Value
}

func (i *Interpreter) VisitVariableExpr(e ast.Variable) any {
	// If global variable
	if e.Distance < 0 {
		if value, ok := i.globals[e.Name.Lexeme]; ok {
			return value
		}

		panic(i.makeError(e.Name, "Undefined variable '%v'.", e.Name.Lexeme))
	} else {
		return i.localEnv.GetAt(e.Slot, e.Distance)
	}
}

// Error reporting methods
// --------------------------------------------------------
// Print the error message with and return a runtimeError object.
func (i *Interpreter) makeError(tok token.Token, format string, args ...any) runtimeError {
	i.errorDistance = 0

	// Print the message...
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	// and the location(in current function) of the error.
	printLocation(0, tok.Line, *util.Last(i.calledFunctions))

	return runtimeError{}
}

// Utility methods
// --------------------------------------------------------
func (i *Interpreter) execute(e ast.Stmt) {
	e.Accept(i)
}

func (i *Interpreter) evaluate(e ast.Expr) any {
	return e.Accept(i)
}

func (i *Interpreter) executeBlock(statements []ast.Stmt, environ object.LocalEnv) {
	// Use supplied environment to execute code and later restore the old one.
	old_env := i.localEnv
	i.localEnv = &environ
	defer func() {
		i.localEnv = old_env
	}()

	for _, stmt := range statements {
		i.execute(stmt)
	}
}

// Defines a variable in the current scope(can be local or global).
func (i *Interpreter) defineVariable(name string, value any) {
	if i.localEnv == nil {
		i.globals[name] = value // Global
	} else {
		i.localEnv.PushVariable(value) // Local
	}

}

// Other utility functions
// --------------------------------------------------------

func printLocation(distance, line int, funName string) {
	fmt.Fprintf(os.Stderr, "%5v: [line %v] in %v\n", distance, line, funName)
}
