package object

type LocalEnv struct {
	enclosing *LocalEnv
	values    []any
}

func NewLocalEnv(enclosing *LocalEnv) *LocalEnv {
	return &LocalEnv{values: make([]any, 0), enclosing: enclosing}
}

func (e *LocalEnv) PushVariable(value any) {
	e.values = append(e.values, value)
}

func (e *LocalEnv) PopVariable(count int) {
	e.values = e.values[:len(e.values)-count]
}

// Return the object stored in the distance number of enclosing scopes away.
// The variable being accessed must exist in that scope.
func (e *LocalEnv) GetAt(slot, distance int) any {
	return ancestor(e, distance).values[slot]
}

// Assign to the object stored in the distance number of enclosing scopes away.
// The variable being accessed must exist in that scope.
func (e *LocalEnv) AssignAt(slot int, value any, distance int) {
	ancestor(e, distance).values[slot] = value
}

func (e *LocalEnv) SetEnclosing(env *LocalEnv) {
	e.enclosing = env
}

func (e *LocalEnv) GetEnclosing() *LocalEnv {
	return e.enclosing
}

func (e *LocalEnv) GetAllValues() []any {
	return e.values
}

func ancestor(env *LocalEnv, distance int) *LocalEnv {
	ret := env

	for i := 0; i < distance; i++ {
		ret = env.enclosing
	}

	return ret
}
