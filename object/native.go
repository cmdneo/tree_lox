package object

import (
	"fmt"
	"time"
	"tree_lox/value"
)

var NativeFunctionsList []*NativeFunction = []*NativeFunction{
	{"clock", 0, clock},
	{"sleep", 1, sleep},
	{"string", 1, string_},
	{"len", 1, len_},
	{"hasattr", 2, hasattr},
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

func sleep(args []value.Value) value.Value {
	seconds := extractArg[value.Number](args[0],
		"First argument should be a number.")

	nanos := time.Duration(seconds * 1e9)
	time.Sleep(nanos)
	return value.Nil{}
}

func len_(args []value.Value) value.Value {
	str := extractArg[value.String](args[0],
		"First argument should be a string.")

	return value.Number(len(str))
}

func string_(args []value.Value) value.Value {
	return value.String(args[0].String())
}

func hasattr(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument should be an instance.")
	field := extractArg[value.String](args[1],
		"Second argument should be a field name.")

	_, ok := instance.Get(string(field))
	return value.Boolean(ok)
}

func getattr(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument should be an instance.")
	field := extractArg[value.String](args[1],
		"Second argument should be a field name.")

	if v, ok := instance.Get(string(field)); ok {
		return v
	} else {
		panic(makeNativeError("Instance has no attribute names '%v'.", field))
	}
}

func setattr(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument should be an instance.")
	field := extractArg[value.String](args[1],
		"Second argument should be a field name.")
	val := args[0]

	instance.Set(string(field), val)
	return value.Nil{}
}

func delattr(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument should be an instance.")
	field := extractArg[value.String](args[1],
		"Second argument should be a field name.")

	if _, ok := instance.Fields[string(field)]; ok {
		delete(instance.Fields, string(field))
	} else {
		panic(makeNativeError("Instance has no attribute names '%v'.", field))
	}

	return value.Nil{}
}

func isinstance(args []value.Value) value.Value {
	instance := extractArg[*Instance](args[0],
		"First argument should be an instance.")
	class := extractArg[*Class](args[1],
		"Second argument should be a class.")

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
