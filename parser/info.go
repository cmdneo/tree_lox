package parser

type loopKind uint8

const (
	kindNoLoop loopKind = iota
	kindForLoop
	kindWhileLoop
)

//go:generate stringer -type=functionKind
type functionKind uint8

const (
	kindNoFunction functionKind = iota
	kindFunction
	kindMethod
	kindInitializer
)

type classKind uint8

const (
	kindNoClass classKind = iota
	kindClass
	kindSubclass
)

func (f functionKind) String() string {
	switch f {
	case kindNoFunction:
		return "<script>"
	case kindFunction:
		return "function"
	case kindMethod:
		return "method"
	case kindInitializer:
		return "initializer"
	default:
		panic("Unknown functionKind.")
	}
}
