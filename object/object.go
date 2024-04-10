package object

import (
	"fmt"
	"math"
	"strconv"
	"tree_lox/ast"
)

// Error type marker.
// Checks are performed before calling any of the methods present below.
// These types are only for providing information in the case of a crash.
type TypeError struct{}

// All lox objects are stored in 'any' typed variables as following:
// Types 'bool', 'number' and 'string' are represented using go's
// primitive types bool, float64 and string respectively.
// While types 'instance', 'class' and 'function' are implemented as structs.

// Lox function object
// --------------------------------------------------------
type Function struct {
	Declaration ast.Function
	Enclosing   *LocalEnv
	IsInit      bool // Is class constructor?
}

func (f *Function) Arity() int {
	return len(f.Declaration.Params)
}

func (f *Function) bind(instance *Instance) Function {
	// Put the instance in a new scope enclosed by the method's scope.
	// This way we can bind the instance to the method accessed.
	env := NewLocalEnv(f.Enclosing)
	env.PushVariable(*instance) // Add 'this'.

	return Function{Declaration: f.Declaration, Enclosing: env}
}

// Lox native function object
// --------------------------------------------------------
type NativeFunction struct {
	Arity_   int
	Function func(args ...any) any
	Name     string
}

func (n *NativeFunction) Arity() int {
	return n.Arity_
}

// Lox class object
// --------------------------------------------------------
type Class struct {
	Name       string
	Methods    map[string]Function
	Superclass *Class // Can be nil
}

func (c *Class) Arity() int {
	if method, ok := c.Methods["init"]; ok {
		return method.Arity()
	} else {
		return 0
	}
}

// Lox class instance object
// --------------------------------------------------------
type Instance struct {
	Fields map[string]any
	Class  Class
}

func (i *Instance) Get(name string) (any, bool) {
	// Fields take precedence over methods
	if value, ok := i.Fields[name]; ok {
		return value, true
	} else if method, ok := i.Class.Methods[name]; ok {
		// Puts 'this' so that the method can access it.
		return method.bind(i), true
	} else {
		return nil, false
	}
}

func (i *Instance) Set(name string, value any) {
	i.Fields[name] = value
}

// Logical operations
// --------------------------------------------------------
func Truthiness(s any) bool {
	switch v := s.(type) {
	case nil:
		return false
	case bool:
		return v

	default:
		return true
	}
}

func LessThan(s, t any) bool {
	switch u := s.(type) {
	case float64:
		switch v := t.(type) {
		case float64:
			return u < v
		}

	case string:
		switch v := t.(type) {
		case string:
			return u < v
		}
	}

	panic(TypeError{})
}

func GreaterThan(s, t any) bool {
	switch u := s.(type) {
	case float64:
		switch v := t.(type) {
		case float64:
			return u > v
		}

	case string:
		switch v := t.(type) {
		case string:
			return u > v
		}
	}

	panic(TypeError{})
}

func EqualTo(s, t any) bool {
	// TODO Do proper type checks.
	return s == t
}

// Mathematical operations
// --------------------------------------------------------
func Neg(s any) any {
	switch u := s.(type) {
	case float64:
		return -u
	}

	panic(TypeError{})
}

func Add(s, t any) any {
	switch u := s.(type) {
	case float64:
		switch v := t.(type) {
		case float64:
			return u + v
		}

	case string:
		switch v := t.(type) {
		case string:
			return u + v
		}
	}

	panic(TypeError{})
}

func Sub(s, t any) any {
	switch u := s.(type) {
	case float64:
		switch v := t.(type) {
		case float64:
			return u - v
		}
	}

	panic(TypeError{})
}

func Mul(s, t any) any {
	switch u := s.(type) {
	case float64:
		switch v := t.(type) {
		case float64:
			return u * v
		}
	}

	panic(TypeError{})
}

func Div(s, t any) any {

	switch u := s.(type) {
	case float64:
		switch v := t.(type) {
		case float64:
			return u / v
		}
	}

	panic(TypeError{})
}

func Rem(s, t any) any {
	a, e := s.(float64)
	b, f := t.(float64)

	if e && f {
		return math.Remainder(a, b)
	}

	panic(TypeError{})
}

func AsString(s any) string {
	switch v := s.(type) {
	case nil:
		return "nil"

	case bool:
		if v {
			return "true"
		} else {
			return "false"
		}

	case string:
		return v

	case float64:
		return strconv.FormatFloat(v, 'f', -1, 64)

	case Function:
		return fmt.Sprintf("<fn %v>", v.Declaration.Name.Lexeme)

	case Class:
		return fmt.Sprintf("<class %v>", v.Name)

	case Instance:
		return fmt.Sprintf("<instance of %v>", v.Class.Name)
	}

	panic(TypeError{})
}
