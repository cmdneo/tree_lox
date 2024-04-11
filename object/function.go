package object

import (
	"fmt"
	"tree_lox/ast"
	"tree_lox/value"
)

type Function struct {
	Declaration *ast.Function
	Enclosing   *LocalEnv
	IsInit      bool // Is class constructor?
}

// Implement the value.Value interface
// --------------------------------------------------------
func (*Function) LoxValueMarkerFunc() {}

func (f *Function) String() string {
	return fmt.Sprintf("<fn %v>", f.Declaration.Name.Lexeme)
}

// --------------------------------------------------------

func NewFunction(decl *ast.Function, enclosing *LocalEnv, is_init bool) *Function {
	return &Function{
		Declaration: decl,
		Enclosing:   enclosing,
		IsInit:      is_init,
	}
}

func (f *Function) Arity() int {
	return len(f.Declaration.Params)
}

// Creates a new function and binds it to the instance.
func (f *Function) newBind(instance *Instance) *Function {
	// Put the instance in a new scope enclosed by the scope which
	// previously enclosed the function's scope.
	// This way we can bind the instance to the function accessed, thus
	// making the function a bound method.
	env := NewLocalEnv(f.Enclosing)
	env.PushVariable(instance) // add 'this'.

	return &Function{Declaration: f.Declaration, Enclosing: env}
}

// TODO Move it somewhere else.
type NativeFunction struct {
	Arity_   int
	Function func(args ...value.Value) value.Value
	Name     string
}

func (n *NativeFunction) Arity() int {
	return n.Arity_
}

// Implement the value.Value interface
// --------------------------------------------------------
func (*NativeFunction) LoxValueMarkerFunc() {}

func (n *NativeFunction) String() string {
	return fmt.Sprintf("<native fn %v>", n.Name)
}

// --------------------------------------------------------
