package object

import (
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

type Function struct {
	Declaration ast.Function
	Enclosing   *LocalEnv
}

func (f *Function) Arity() int {
	return len(f.Declaration.Params)
}

type Class struct {
	Methods    map[string]Function
	Superclass *Class // Can be nil
}

type Instance struct {
	Fields map[string]any
	Class  Class
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
		return "<fn !>"

	case Class:
		return "<class !>"

	case Instance:
		return "<instance of !>"
	}

	panic(TypeError{})
}
