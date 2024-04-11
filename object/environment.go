package object

import "tree_lox/value"

type LocalEnv struct {
	enclosing *LocalEnv
	values    []value.Value
}

const initialEnvSize int = 4

func NewLocalEnv(enclosing *LocalEnv) *LocalEnv {
	return &LocalEnv{
		values:    make([]value.Value, 0, initialEnvSize),
		enclosing: enclosing,
	}
}

func (e *LocalEnv) PushVariable(value value.Value) {
	e.values = append(e.values, value)
}

func (e *LocalEnv) PopVariable(count int) {
	e.values = e.values[:len(e.values)-count]
}

// Return the object stored in the distance number of enclosing scopes away.
// The variable being accessed must exist in that scope.
func (e *LocalEnv) GetAt(slot, distance int) value.Value {
	return ancestor(e, distance).values[slot]
}

// Assign to the object stored in the distance number of enclosing scopes away.
// The variable being accessed must exist in that scope.
func (e *LocalEnv) AssignAt(slot int, value value.Value, distance int) {
	ancestor(e, distance).values[slot] = value
}

func (e *LocalEnv) SetEnclosing(env *LocalEnv) {
	e.enclosing = env
}

func (e *LocalEnv) GetEnclosing() *LocalEnv {
	return e.enclosing
}

func (e *LocalEnv) GetAllValues() []value.Value {
	return e.values
}

func ancestor(env *LocalEnv, distance int) *LocalEnv {
	ret := env

	for i := 0; i < distance; i++ {
		ret = ret.enclosing
	}

	return ret
}
