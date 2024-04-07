package parser

type localScope struct {
	// Local variables are usually small in number, so this is fine.
	locals []localVar
	depth  int
}

type localVar struct {
	name    string
	defined bool
}

func makeLocalScope(depth int) localScope {
	return localScope{locals: make([]localVar, 0, 4), depth: depth}
}

// Returns the slot index along with if defined, -1 if variable is not present.
func (s *localScope) getVariable(name string) (int, bool) {
	for i, local := range s.locals {
		if local.name == name {
			return i, local.defined
		}
	}

	return -1, false
}

// Pushes the variable into the scope
func (s *localScope) putVariable(name string) {
	s.locals = append(s.locals, localVar{name: name, defined: false})
}

// Marks the last put variable as defined
func (s *localScope) markDefined() {
	s.locals[len(s.locals)-1].defined = true
}
