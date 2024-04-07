package main

import (
	"bufio"
	"fmt"
	"os"
	"tree_lox/interpreter"
	"tree_lox/parser"
)

var lox_interpreter interpreter.Interpreter = interpreter.MakeInterpreter()

func main() {
	switch len(os.Args) {
	case 0, 1:
		execPrompt()
	case 2:
		execFromFile(os.Args[1])

	default:
		os.Exit(1)
	}
}

func execFromFile(filepath string) {
	source, err := os.ReadFile(filepath)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Cannot open file becase: %v.\n", err.Error())
		os.Exit(1)
	}

	p := parser.MakeParser(string(source))
	stmts := p.Parse()
	if stmts == nil {
		os.Exit(1)
	}

	lox_interpreter.Interpret(stmts)

}

func execPrompt() {
	line_scanner := bufio.NewScanner(os.Stdin)

	for {
		fmt.Fprint(os.Stderr, "> ")
		if !line_scanner.Scan() {
			break
		}

		p := parser.MakeParser(line_scanner.Text())
		stmts := p.Parse()

		if stmts != nil {
			lox_interpreter.Interpret(stmts)
		}
	}

	if err := line_scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading input: %v\n.", err.Error())
		os.Exit(1)
	}

	fmt.Fprintln(os.Stderr, "[EXIT]")
}
