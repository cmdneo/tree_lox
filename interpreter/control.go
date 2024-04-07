package interpreter

// Control as directed by the statement executed.

type controlReturn struct {
	Value any
}

type controlBreak struct{}

type controlContinue struct{}
