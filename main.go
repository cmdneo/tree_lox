package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"runtime/pprof"
	"tree_lox/interpreter"
	"tree_lox/parser"
)

// Maintain the interpreter state by making it global throughout the session.
var lox_interpreter interpreter.Interpreter = interpreter.MakeInterpreter()

func main() {
	// Start CPU profile if enabled via the env-var CPUPROFILE.
	if prof_out, has := os.LookupEnv("CPUPROFILE"); has && prof_out != "" {
		f, err := os.Create(prof_out)
		if err != nil {
			log.Fatalf(
				"Cannot create profile output file: '%v' (%v).\n",
				prof_out, err,
			)
		}
		pprof.StartCPUProfile(f)
		defer pprof.StopCPUProfile()
	}

	switch len(os.Args) {
	case 0, 1:
		execPrompt()
	case 2:
		execFromFile(os.Args[1])

	default:
		fmt.Fprintf(os.Stderr, "Usage: %v [filename]\n", os.Args[0])
		os.Exit(1)
	}
}

func execFromFile(filepath string) {
	source, err := os.ReadFile(filepath)

	if err != nil {
		fmt.Fprintf(os.Stderr, "Cannot open file '%v'  (%v).\n", filepath, err.Error())
		os.Exit(1)
	}

	p := parser.MakeParser(string(source))
	stmts := p.Parse()
	if stmts == nil {
		os.Exit(1)
	}

	if !lox_interpreter.Interpret(stmts) {
		os.Exit(1)
	}
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
