package ast

type ControlKind uint8

const (
	ControlLinear ControlKind = iota
	ControlBreak
	ControlContinue
	ControlReturn
)
