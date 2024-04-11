package object

import (
	"fmt"
	"tree_lox/value"
)

type Instance struct {
	Fields map[string]value.Value
	Class  *Class
}

// Implement the value.Value interface
// --------------------------------------------------------
func (*Instance) LoxValueMarkerFunc() {}

func (c *Instance) String() string {
	return fmt.Sprintf("<instance of %v>", c.Class.Name)
}

// --------------------------------------------------------

func NewInstance(class *Class) *Instance {
	return &Instance{Class: class}
}

func (i *Instance) Get(name string) (value.Value, bool) {
	// Fields take precedence over methods
	if value, ok := i.Fields[name]; ok {
		return value, true
	} else if method := i.Class.Get(name); method != nil {
		// Puts 'this' so that the method can access it.
		return method.newBind(i), true
	} else {
		return nil, false
	}
}

func (i *Instance) Set(name string, value value.Value) {
	i.Fields[name] = value
}
