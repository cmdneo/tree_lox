package interpreter

import (
	"fmt"
	"os"
	"tree_lox/ast"
	"tree_lox/object"
	"tree_lox/token"
	"tree_lox/util"
	"tree_lox/value"
)

const maxCallDepth int = 1024

type Interpreter struct {
	// Global variables
	globals map[string]value.Value
	// Local variable scopes
	localEnv *object.LocalEnv
	// Functions we are currently inside.
	calledFunctions []callInfo
	// Return value, only one function returns it value at a time.
	returnedValue value.Value
	// Tracks if had error
	hadError bool
}

// Call info for generating stack traces
type callInfo struct {
	// Function name
	Name string
	// Line number from where a call was made inside this function.
	// Update this when a new function is called.
	DescendLine int
}

type runtimeError struct{}

// For the 'error' interface
func (r runtimeError) Error() string {
	return "Runtime error"
}

func MakeInterpreter() Interpreter {
	globals := map[string]value.Value{}

	// Add the native functions into global scope.
	for _, nfn := range object.NativeFunctionsList {
		globals[nfn.Name] = nfn
	}

	return Interpreter{
		globals:         globals,
		localEnv:        nil,
		calledFunctions: []callInfo{},
		returnedValue:   value.Nil{},
		hadError:        false,
	}
}

// Interpret the code. Return true if no runtime error occured
// and false if any runtime error occured.
func (i *Interpreter) Interpret(statements []ast.Stmt) bool {
	defer func() {
		switch err := recover().(type) {
		case nil:
		case runtimeError:
			// Just handle it, the error messages and stack trace is
			// printed when this error was created.
		default:
			panic(err)
		}
	}()

	i.hadError = false
	// Discard all previous local environments (if any) left due to errors.
	i.localEnv = nil
	// The top-level code is assumend to be an implicit function named '<script>'.
	i.calledFunctions = []callInfo{{Name: "<script>"}}

	for _, stmt := range statements {
		i.execute(stmt)
	}

	return !i.hadError
}

// Wrapper methods which pass in the interpreter as a
// visitor to the statements and expressions.
// --------------------------------------------------------
func (i *Interpreter) execute(e ast.Stmt) ast.ControlKind {
	return e.Accept(i)
}

func (i *Interpreter) evaluate(e ast.Expr) value.Value {
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
	fmt.Printf("%v\n", i.evaluate(s.Expression).String())
	return ast.ControlLinear
}

func (i *Interpreter) VisitAssertStmt(s *ast.Assert) ast.ControlKind {
	if !value.Truthiness(i.evaluate(s.Expression)) {
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
	value := value.Value(value.Nil{})
	if s.Value != nil {
		value = i.evaluate(s.Value)
	}

	i.returnedValue = value
	return ast.ControlReturn
}

func (i *Interpreter) VisitIfStmt(s *ast.If) ast.ControlKind {
	if value.Truthiness(i.evaluate(s.Condition)) {
		return i.execute(s.ThenBranch)
	} else if s.ElseBranch != nil {
		return i.execute(s.ElseBranch)
	}

	return ast.ControlLinear
}

func (i *Interpreter) VisitForStmt(s *ast.For) ast.ControlKind {
	for value.Truthiness(i.evaluate(s.Condition)) {
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
func (i *Interpreter) VisitAssignExpr(e *ast.Assign) value.Value {
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

func (i *Interpreter) VisitTernaryExpr(e *ast.Ternary) value.Value {
	truth := value.Truthiness(i.evaluate(e.Condition))

	if truth {
		return i.evaluate(e.TrueExpr)
	} else {
		return i.evaluate(e.FalseExpr)
	}
}

func (i *Interpreter) VisitLogicalExpr(e *ast.Logical) value.Value {
	left := i.evaluate(e.Left)

	// Return the value of the expression which determines the truth value of
	// the logical expression and not a boolean, similar to what python does.
	switch e.Operator.Kind {
	case token.OR:
		if value.Truthiness(left) {
			return left
		}

	case token.AND:
		if !value.Truthiness(left) {
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

func (i *Interpreter) VisitBinaryExpr(e *ast.Binary) value.Value {
	left := i.evaluate(e.Left)
	right := i.evaluate(e.Right)

	check_nums := func() {
		if hasType[value.Number](left, right) {
			return
		}
		panic(i.makeError(e.Operator,
			"Operands must be either two strings or two numbers."))
	}

	check_num_or_str := func() {
		if hasType[value.Number](left, right) ||
			hasType[value.String](left, right) {
			return
		}
		panic(i.makeError(e.Operator, "Operands must numbers."))
	}

	switch e.Operator.Kind {
	case token.PLUS:
		check_num_or_str()
		return value.Add(left, right)
	case token.MINUS:
		check_nums()
		return value.Sub(left, right)
	case token.STAR:
		check_nums()
		return value.Mul(left, right)
	case token.SLASH:
		check_nums()
		return value.Div(left, right)

	case token.GREATER:
		check_num_or_str()
		return value.GreaterThan(left, right)
	case token.GREATER_EQUAL:
		check_num_or_str()
		return value.GreaterThan(left, right) || value.EqualTo(left, right)

	case token.LESS:
		check_num_or_str()
		return value.LessThan(left, right)
	case token.LESS_EQUAL:
		check_num_or_str()
		return value.LessThan(left, right) || value.EqualTo(left, right)

	case token.EQUAL_EQUAL:
		return value.EqualTo(left, right)
	case token.BANG_EQUAL:
		return !value.EqualTo(left, right)

	default:
		panic("Invalid operator token in binary expression.")
	}
}

func (i *Interpreter) VisitUnaryExpr(e *ast.Unary) value.Value {
	right := i.evaluate(e.Right)

	check_num := func() {
		if hasType[float64](right, float64(0)) {
			return
		}
		panic(i.makeError(e.Operator, "Operand must be a number."))
	}

	switch e.Operator.Kind {
	case token.BANG:
		return !value.Truthiness(right)

	case token.PLUS:
		check_num()
		return right
	case token.MINUS:
		check_num()
		return value.Neg(right)

	default:
		panic("Invalid operator token in unary expression.")
	}
}

func (i *Interpreter) VisitCallExpr(e *ast.Call) value.Value {
	callee := i.evaluate(e.Callee)
	name, arity := getArityAndName(callee)

	if arity < 0 {
		panic(i.makeError(e.Paren, "Can only call functions and classes."))

	}
	if arity != len(e.Arguments) {
		panic(i.makeError(
			e.Paren, "Expected %v arguments but got %v arguments.",
			arity, len(e.Arguments),
		))
	}

	// Evaluate the arguments and put the resulting values inside
	// a newly created function environment.
	fun_env := object.NewLocalEnv(nil)
	for _, arg := range e.Arguments {
		fun_env.PushVariable(i.evaluate(arg))
	}

	// Enforce a maximum call depth before starting the call but after
	// evaluating its arguments. The before and after is completely arbtriary
	if len(i.calledFunctions) == maxCallDepth {
		panic(i.makeError(
			e.Paren, "Max call depth %v reached.",
			maxCallDepth-1, // Exclude the top-level from depth count shown.
		))
	}

	// A native function call does not count towards call stack. So call it
	// witout pushing any callInfo and just return the return value.
	if nfn, ok := callee.(*object.NativeFunction); ok {
		// For handling native error.
		defer func() {
			switch v := recover().(type) {
			case nil:
			case object.NativeError:
				panic(i.makeError(
					e.Paren, "Error in native-function '%v': %v",
					name, v.Error(),
				))
			default:
				panic(v)
			}
		}()

		return nfn.Call(fun_env.GetAllValues())
	}

	// Update the line number in previous function from where it
	// called this function(or callable).
	util.Last(i.calledFunctions).DescendLine = e.Paren.Line
	// Push the call ifno for stack trace generation.
	i.calledFunctions = append(i.calledFunctions, callInfo{Name: name})
	defer util.Pop(&i.calledFunctions)

	// Perform the call for class and function types
	switch callable := callee.(type) {
	case *object.Class:
		instance := object.NewInstance(callable)
		if method, ok := instance.Get("init"); ok {
			// If 'init' is present in a blank instance it must be a function.
			method := method.(*object.Function)
			fun_env.SetEnclosing(method.Enclosing)
			i.executeBlock(method.Declaration.Body, fun_env)
		}
		i.returnedValue = instance

	case *object.Function:
		fun_env.SetEnclosing(callable.Enclosing)
		i.returnedValue = value.Nil{} // Default return value
		// Return value will be set by the return statement(if any) executed.
		i.executeBlock(callable.Declaration.Body, fun_env)

	default:
		panic("Attempt to call a non-callable object.")
	}

	return i.returnedValue
}

func (i *Interpreter) VisitGetExpr(e *ast.Get) value.Value {
	if obj, ok := i.evaluate(e.Object).(*object.Instance); ok {
		if val, ok := obj.Get(e.Name.Lexeme); ok {
			return val
		}

		panic(i.makeError(e.Name, "Undefined property '%v'.", e.Name.Lexeme))
	}

	panic(i.makeError(e.Name, "Only instances have fields."))
}

func (i *Interpreter) VisitSetExpr(e *ast.Set) value.Value {
	if instance, ok := i.evaluate(e.Object).(*object.Instance); ok {
		val := i.evaluate(e.Value)
		instance.Set(e.Name.Lexeme, val)
		return val
	}

	panic(i.makeError(e.Name, "Only instances have fields."))
}

func (i *Interpreter) VisitSuperExpr(e *ast.Super) value.Value {
	// 'super' is guranteed to be a class.
	super, ok := i.resolveVariable(&e.Variable).(*object.Class)
	if !ok {
		panic("'super' is not a class")
	}

	if method := super.Get(e.Method.Lexeme); method != nil {
		return method
	}

	panic(i.makeError(e.Method, "Undefined property '%v'.", e.Method.Lexeme))
}

func (i *Interpreter) VisitThisExpr(e *ast.This) value.Value {
	return i.resolveVariable(&e.Variable)
}

func (i *Interpreter) VisitGroupingExpr(e *ast.Grouping) value.Value {
	return i.evaluate(e.Expr)
}

func (i *Interpreter) VisitLiteralExpr(e *ast.Literal) value.Value {
	return e.Value
}

func (i *Interpreter) VisitVariableExpr(e *ast.Variable) value.Value {
	return i.resolveVariable(e)
}

// Error reporting methods
// --------------------------------------------------------
// Print the error message with and return a runtimeError object.
func (i *Interpreter) makeError(
	tok token.Token, format string, args ...any,
) runtimeError {
	// Set location in the current function to as provided.
	util.Last(i.calledFunctions).DescendLine = tok.Line

	// Print the message...
	fmt.Fprintf(os.Stderr, format+"\n", args...)

	// Most recent call is printed first, last is the top-level.
	for d := range i.calledFunctions {
		frame := i.calledFunctions[len(i.calledFunctions)-1-d] // reversed
		printLocation(d, frame.DescendLine, frame.Name)
	}

	i.hadError = true
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

// Defines a variable in the current scope(can be local or global).
func (i *Interpreter) defineVariable(name string, value value.Value) {
	if i.localEnv == nil {
		i.globals[name] = value // Global
	} else {
		i.localEnv.PushVariable(value) // Local
	}

}

// Returns the value of the variable.
func (i *Interpreter) resolveVariable(v *ast.Variable) value.Value {
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

// Tries to convert a value to callable, returns -1 arity if not a callable.
func getArityAndName(callee value.Value) (string, int) {
	switch v := callee.(type) {
	case *object.Function:
		return v.Declaration.Name.Lexeme, v.Arity()
	case *object.NativeFunction:
		return v.Name, v.Arity()
	case *object.Class:
		return v.Name, v.Arity()
	}

	return "", -1
}
