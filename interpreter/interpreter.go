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
	calledFunction  []callInfo
	// Current stack distance from the error site.
	errorDistance int
	// Return value, only one function returns it value at a time.
	returnedValue any
}

type runtimeError struct{}

type callInfo struct {
	FunctionName string
	CallLine     int
}

func MakeInterpreter() Interpreter {
	return Interpreter{
		globals:  map[string]any{},
		localEnv: nil,
		// The top-level implicit function is named '<script>'.
		calledFunctions: []string{"<script>"},
		calledFunction:  []callInfo{},
		errorDistance:   0,
	}
}

func (i *Interpreter) Interpret(statements []ast.Stmt) {
	defer func() {
		switch err := recover().(type) {
		case nil:
		case runtimeError:
			// Just handle it, the error messages when the error is made and
			// TODO Print call stack for error
		default:
			panic(err)
		}
	}()

	// Discard all previous local environments (if any) left due to
	// any error in the code executed.
	i.localEnv = nil
	i.calledFunctions = i.calledFunctions[:1]
	i.errorDistance = 0

	for _, stmt := range statements {
		i.execute(stmt)
	}
}

// Wrapper methods which pass in the interpreter as a
// visitor to the statements and expressions.
// --------------------------------------------------------
func (i *Interpreter) execute(e ast.Stmt) ast.ControlKind {
	return e.Accept(i)
}

func (i *Interpreter) evaluate(e ast.Expr) any {
	return e.Accept(i)
}

// Statement evaluators
// --------------------------------------------------------
func (i *Interpreter) VisitBlockStmt(s *ast.Block) ast.ControlKind {
	return i.executeBlock(s.Statements, object.NewLocalEnv(i.localEnv))
}

func (i *Interpreter) VisitExpressionStmt(s *ast.Expression) ast.ControlKind {
	i.evaluate(s.Expression)
	return ast.ControlLinear
}

func (i *Interpreter) VisitPrintStmt(s *ast.Print) ast.ControlKind {
	fmt.Printf("%v\n", object.AsString(i.evaluate(s.Expression)))
	return ast.ControlLinear
}

func (i *Interpreter) VisitAssertStmt(s *ast.Assert) ast.ControlKind {
	if !object.Truthiness(i.evaluate(s.Expression)) {
		panic(i.makeError(s.Keyword, "Assertion failure."))
	} else {
		return ast.ControlLinear
	}
}

func (i *Interpreter) VisitBreakStmt(s *ast.Break) ast.ControlKind {
	return ast.ControlBreak
}

func (i *Interpreter) VisitContinueStmt(s *ast.Continue) ast.ControlKind {
	return ast.ControlContinue
}

func (i *Interpreter) VisitReturnStmt(s *ast.Return) ast.ControlKind {
	value := any(nil)
	if s.Value != nil {
		value = i.evaluate(s.Value)
	}

	i.returnedValue = value
	return ast.ControlReturn
}

func (i *Interpreter) VisitIfStmt(s *ast.If) ast.ControlKind {
	if object.Truthiness(i.evaluate(s.Condition)) {
		return i.execute(s.ThenBranch)
	} else if s.ElseBranch != nil {
		return i.execute(s.ElseBranch)
	}

	return ast.ControlLinear
}

func (i *Interpreter) VisitForStmt(s *ast.For) ast.ControlKind {
	for object.Truthiness(i.evaluate(s.Condition)) {
		flow := i.execute(s.Body)

		switch flow {
		case ast.ControlBreak:
			return ast.ControlLinear
		case ast.ControlReturn:
			return ast.ControlReturn
		}

		if s.Update != nil {
			i.evaluate(s.Update)
		}
	}

	return ast.ControlLinear
}

func (i *Interpreter) VisitVarStmt(s *ast.Var) ast.ControlKind {
	val := i.evaluate(s.Initializer)
	i.defineVariable(s.Name.Lexeme, val)

	return ast.ControlLinear
}

func (i *Interpreter) VisitFunctionStmt(s *ast.Function) ast.ControlKind {
	fun := object.Function{Declaration: s, Enclosing: i.localEnv}
	i.defineVariable(s.Name.Lexeme, &fun)

	return ast.ControlLinear
}

func (i *Interpreter) VisitClassStmt(s *ast.Class) ast.ControlKind {
	superclass := (*object.Class)(nil)

	if s.Superclass != nil {
		if class, ok := i.evaluate(s.Superclass).(*object.Class); ok {
			superclass = class
		} else {
			panic(i.makeError(s.Superclass.Name, "Superclass must be a class."))
		}
	}

	// Create a new environment for the superclass and define 'super' in it.
	// It is shared among all instances since we only access the methods of the
	// superclass and methods and do not change anything.
	// INFO: This environment will enclose the environments of all the methods.
	if superclass != nil {
		i.localEnv = object.NewLocalEnv(i.localEnv)
		i.defineVariable("super", superclass)
	}

	// The environment containing 'this' is created per instance per method
	// access, not here, since it is different for each instance.
	methodMap := map[string]object.Function{}

	for name, fun := range s.Methods {
		methodMap[name] = object.Function{
			Declaration: fun,
			Enclosing:   i.localEnv,
			IsInit:      name == "init",
		}
	}

	// Pop the 'super' environment.
	if superclass != nil {
		i.localEnv = i.localEnv.GetEnclosing()
	}

	// Define the class after popping the 'super' environment.
	class := object.Class{
		Name:       s.Name.Lexeme,
		Methods:    methodMap,
		Superclass: superclass,
	}
	i.defineVariable(class.Name, &class)

	return ast.ControlLinear
}

// Expression evaluators
// --------------------------------------------------------
func (i *Interpreter) VisitAssignExpr(e *ast.Assign) any {
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

func (i *Interpreter) VisitTernaryExpr(e *ast.Ternary) any {
	truth := object.Truthiness(i.evaluate(e.Condition))

	if truth {
		return i.evaluate(e.TrueExpr)
	} else {
		return i.evaluate(e.FalseExpr)
	}
}

func (i *Interpreter) VisitLogicalExpr(e *ast.Logical) any {
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

func (i *Interpreter) VisitBinaryExpr(e *ast.Binary) any {
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

func (i *Interpreter) VisitUnaryExpr(e *ast.Unary) any {
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

func (i *Interpreter) VisitCallExpr(e *ast.Call) any {
	return i.performCall(e)
}

func (i *Interpreter) VisitGetExpr(e *ast.Get) any {
	if obj, ok := i.evaluate(e.Object).(*object.Instance); ok {
		if val, ok := obj.Get(e.Name.Lexeme); ok {
			return val
		}

		panic(i.makeError(e.Name, "Undefined property '%v'.", e.Name.Lexeme))
	}

	panic(i.makeError(e.Name, "Only instances have fields."))
}

func (i *Interpreter) VisitSetExpr(e *ast.Set) any {
	if instance, ok := i.evaluate(e.Object).(*object.Instance); ok {
		val := i.evaluate(e.Value)
		instance.Set(e.Name.Lexeme, val)
		return val
	}

	panic(i.makeError(e.Name, "Only instances have fields."))
}

func (i *Interpreter) VisitSuperExpr(e *ast.Super) any {
	// 'super' is guranteed to be a class.
	super, ok := i.resolveVariable(&e.Variable).(*object.Class)
	if !ok {
		panic("'super' is not a class")
	}

	if method := super.Get(e.Method.Lexeme); method != nil {
		return *method
	}

	panic(i.makeError(e.Method, "Undefined property '%v'.", e.Method.Lexeme))
}

func (i *Interpreter) VisitThisExpr(e *ast.This) any {
	return i.resolveVariable(&e.Variable)
}

func (i *Interpreter) VisitGroupingExpr(e *ast.Grouping) any {
	return i.evaluate(e.Expr)
}

func (i *Interpreter) VisitLiteralExpr(e *ast.Literal) any {
	return e.Value
}

func (i *Interpreter) VisitVariableExpr(e *ast.Variable) any {
	return i.resolveVariable(e)
}

// Error reporting methods
// --------------------------------------------------------
// Print the error message with and return a runtimeError object.
func (i *Interpreter) makeError(
	tok token.Token, format string, args ...any,
) runtimeError {
	i.errorDistance = 0

	// Print the message...
	fmt.Fprintf(os.Stderr, format+"\n", args...)
	// and the location(in current function) of the error.
	printLocation(0, tok.Line, *util.Last(i.calledFunctions))

	return runtimeError{}
}

// Utility methods
// --------------------------------------------------------
func (i *Interpreter) executeBlock(
	statements []ast.Stmt, environ *object.LocalEnv,
) ast.ControlKind {
	// Use supplied environment to execute code and later restore the old one.
	old_env := i.localEnv
	i.localEnv = environ
	defer func() {
		i.localEnv = old_env
	}()

	for _, stmt := range statements {
		if flow := i.execute(stmt); flow != ast.ControlLinear {
			return flow
		}
	}

	return ast.ControlLinear
}

// Performs call and returns the return value.
func (i *Interpreter) performCall(e *ast.Call) any {
	callee := i.evaluate(e.Callee)
	arity, name, ok := getCallableInfo(callee)

	if !ok {
		panic(i.makeError(e.Paren, "Can only call functions and classes."))
	}

	if arity != len(e.Arguments) {
		panic(i.makeError(
			e.Paren, "Expected %v arguments but got %v arguments.",
			arity, len(e.Arguments),
		))
	}

	// Push the callable name for stack trace generation.
	i.calledFunctions = append(i.calledFunctions, name)
	defer util.Pop(&i.calledFunctions)

	// Evaluate arguments and put their values inside the newly created
	// function's environment.
	fun_env := object.NewLocalEnv(nil)
	for _, arg := range e.Arguments {
		fun_env.PushVariable(i.evaluate(arg))
	}

	// TODO improve this garbage!
	switch obj := callee.(type) {
	case *object.Class:
		// Create the instance
		instance := &object.Instance{
			Fields: map[string]any{},
			Class:  obj,
		}

		if init, ok := instance.Get("init"); ok {
			// Since the instance is blank the "init" must be a function.
			fun := init.(*object.Function)
			fun_env.SetEnclosing(fun.Enclosing)
			i.executeBlock(fun.Declaration.Body, fun_env)
		}

		return instance

	case *object.Function:
		fun_env.SetEnclosing(obj.Enclosing)
		i.executeBlock(obj.Declaration.Body, fun_env)
		return i.returnedValue

	case *object.NativeFunction:
		return obj.Function(fun_env.GetAllValues()...)

	default:
		panic("Non callable object called.")
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

// Returns the value of the variable.
func (i *Interpreter) resolveVariable(v *ast.Variable) any {
	// If global variable
	if v.Distance < 0 {
		if value, ok := i.globals[v.Name.Lexeme]; ok {
			return value
		}

		panic(i.makeError(v.Name, "Undefined variable '%v'.", v.Name.Lexeme))
	} else {
		return i.localEnv.GetAt(v.Slot, v.Distance)
	}
}

// Other utility functions
// --------------------------------------------------------
func printLocation(distance, line int, funName string) {
	fmt.Fprintf(os.Stderr, "%5v: [line %v] in %v\n", distance, line, funName)
}

// Get info if callable object, return arity and name, along with
// if callable(as boolean).
func getCallableInfo(callable any) (int, string, bool) {
	arity := -1
	name := ""

	switch obj := callable.(type) {
	case *object.Class:
		arity = obj.Arity()
		name = obj.Name

	case *object.Function:
		arity = obj.Arity()
		name = obj.Declaration.Name.Lexeme

	case *object.NativeFunction:
		arity = obj.Arity()
		name = obj.Name

	default:
		return arity, name, false
	}

	return arity, name, true
}
