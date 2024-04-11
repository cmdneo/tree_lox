package object

import "fmt"

type Class struct {
	Name string
	// Store the function object by value since it is only 2-ptr + 1-bool.
	Methods    map[string]Function
	Superclass *Class // Can be nil
}

// Implement the value.Value interface
// --------------------------------------------------------
func (*Class) LoxValueMarkerFunc() {}

func (c *Class) String() string {
	return fmt.Sprintf("<class %v>", c.Name)
}

// --------------------------------------------------------

func NewClass(name string, methods map[string]Function, superclass *Class) *Class {
	return &Class{
		Name:       name,
		Methods:    methods,
		Superclass: superclass,
	}
}

func (c *Class) Arity() int {
	if method, ok := c.Methods["init"]; ok {
		return method.Arity()
	} else {
		return 0
	}
}

func (c *Class) Get(name string) *Function {
	if fun, ok := c.Methods[name]; ok {
		return &fun
	} else if c.Superclass != nil {
		return c.Superclass.Get(name)
	} else {
		return nil
	}
}
