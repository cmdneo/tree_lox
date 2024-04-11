package value

import "strconv"

// The lox value interface every value stored in any variable
// must be of this type(implement this interface).
type Value interface {
	String() string
	LoxValueMarkerFunc()
}

// Panic thrown with this type on invalid logical or mathematical operation.
type TypeError struct{}

// Primitve value types, that are: Nil, Boolean, Number and String are
// defined as in terms of go primitive types and are stored by value.
// For objects see tree_lox/object, they are stored as pointers.

type Nil struct{}
type Boolean bool
type Number float64
type String string

// Implement the value.Value interface for primitive types.
// --------------------------------------------------------
func (Nil) LoxValueMarkerFunc()     {}
func (Boolean) LoxValueMarkerFunc() {}
func (Number) LoxValueMarkerFunc()  {}
func (String) LoxValueMarkerFunc()  {}

func (n Nil) String() string {
	return "nil"
}

func (b Boolean) String() string {
	if b {
		return "true"
	} else {
		return "false"
	}
}

func (n Number) String() string {
	return strconv.FormatFloat(float64(n), 'f', -1, 64)
}

func (s String) String() string {
	return string(s)
}

// --------------------------------------------------------

// Logical operations for value.
// --------------------------------------------------------
func Truthiness(s Value) Boolean {
	switch v := s.(type) {
	case Nil:
		return false
	case Boolean:
		return Boolean(v)

	default:
		return true
	}
}

func LessThan(s, t Value) Boolean {
	switch u := s.(type) {
	case Number:
		switch v := t.(type) {
		case Number:
			return u < v
		}

	case String:
		switch v := t.(type) {
		case String:
			return u < v
		}
	}

	panic(TypeError{})
}

func GreaterThan(s, t Value) Boolean {
	switch u := s.(type) {
	case Number:
		switch v := t.(type) {
		case Number:
			return u > v
		}

	case String:
		switch v := t.(type) {
		case String:
			return u > v
		}
	}

	panic(TypeError{})
}

func EqualTo(s, t Value) Boolean {
	// Two *Values* are equal only if their types and stored values are equal.
	// For primitive types this works fine since they are stored as values.
	// For object types which are stored as pointers in the Value, this also
	// works fine since two objects are considered equal only if they point
	// to the same underlying object.
	// The following comparison compares the type and the value stored in the
	// Value interface object and evaluates to true if both are equal.
	return s == t
}

// Mathematical operations for value.
// --------------------------------------------------------
func Neg(s Value) Value {
	switch u := s.(type) {
	case Number:
		return -u
	}

	panic(TypeError{})
}

func Add(s, t Value) Value {
	switch u := s.(type) {
	case Number:
		switch v := t.(type) {
		case Number:
			return u + v
		}

	case String:
		switch v := t.(type) {
		case String:
			return u + v
		}
	}

	panic(TypeError{})
}

func Sub(s, t Value) Value {
	switch u := s.(type) {
	case Number:
		switch v := t.(type) {
		case Number:
			return u - v
		}
	}

	panic(TypeError{})
}

func Mul(s, t Value) Value {
	switch u := s.(type) {
	case Number:
		switch v := t.(type) {
		case Number:
			return u * v
		}
	}

	panic(TypeError{})
}

func Div(s, t Value) Value {

	switch u := s.(type) {
	case Number:
		switch v := t.(type) {
		case Number:
			return u / v
		}
	}

	panic(TypeError{})
}
