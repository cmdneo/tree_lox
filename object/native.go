package object

import (
	"fmt"
	"time"
	"tree_lox/value"
)

var NativeFunctionsList []*NativeFunction = []*NativeFunction{
	{"clock", 0, clock},
	{"string", 1, tostring},
	{"getattr", 2, getattr},
	{"setattr", 3, setattr},
	{"delattr", 2, delattr},
	{"isinstance", 2, isinstance},
}

type NativeFunction struct {
	Name       string
	ParamCount int
	Function   func(args []value.Value) value.Value
}

// Implement the value.Value interface
// --------------------------------------------------------
func (*NativeFunction) LoxValueMarkerFunc() {}

func (n *NativeFunction) String() string {
	return fmt.Sprintf("<native fn %v>", n.Name)
}

// --------------------------------------------------------

func (n *NativeFunction) Arity() int {
	return n.ParamCount
}

func (n *NativeFunction) Call(args []value.Value) value.Value {
	// Arity is verified by the interpreter, so crash on a mismatch here.
	if len(args) != n.Arity() {
		panic("Got wrong number of arguments in native function.")

	}

	return n.Function(args)
}

// Error thrown by native functions on domain or type error.
// Note arity is verified by the interpreter.
// --------------------------------------------------------
type NativeError struct {
	message string
}

// For 'error' interface
func (n NativeError) Error() string { return n.message }

func makeNativeError(format string, args ...any) NativeError {
	return NativeError{message: fmt.Sprintf(format, args...)}
}

// Native functions
// --------------------------------------------------------

func clock(args []value.Value) value.Value {
	return value.Number(time.Now().UnixMilli()) / 1000.0
}

func tostring(args []value.Value) value.Value {
	return value.String(args[0].String())
}

func getattr(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument to 'getattr' should be an instance.")
	field := extractArg[value.String](args[1],
		"Second argument to 'getattr' should be a field name.")

	if v, ok := instance.Get(string(field)); ok {
		return v
	} else {
		panic(makeNativeError("Instance has no attribute names '%v'.", field))
	}
}

func setattr(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument to 'setattr' should be an instance.")
	field := extractArg[value.String](args[1],
		"Second argument to 'setattr' should be a field name.")
	val := args[0]

	instance.Set(string(field), val)
	return value.Nil{}
}

func delattr(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument to 'delattr' should be an instance.")
	field := extractArg[value.String](args[1],
		"Second argument to 'delattr' should be a field name.")

	if _, ok := instance.Fields[string(field)]; ok {
		delete(instance.Fields, string(field))
	} else {
		panic(makeNativeError("Instance has no attribute names '%v'.", field))
	}

	return value.Nil{}
}

func isinstance(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument to 'isinstance' should be an instance.")
	class := extractArg[*Class](args[0],
		"Second argument to 'isinstance' should be a class.")

	return value.Boolean(instance.Class == class)
}

// Type ehcecking helpers
// --------------------------------------------------------
func extractArg[T value.Value](arg value.Value, err_message string) T {
	if v, ok := arg.(T); ok {
		return v
	}

	panic(makeNativeError(err_message))

}
