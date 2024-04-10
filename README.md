Tree-Lox
======

A tree-walk interpreter for the lox language as described in
the book "Crafting Interpreters" written in Go.

It can execute lox scripts from a given file or can be launched in REPL mode:
```bash
lox <file-name> # Run from a file
lox             # Start the REPL
```

Additional features
-------------------
 - Strings can be compared lexicographically using the comparison operators
 - Ternary operator (`:?`)
 - `break` and `continue` statements
 - `assert` statement

Examples
--------
### Fibonacci numbers
```
fun fib(n) {
	if (n <= 1) return 1;
	return fib(n - 1) + fib(n - 2);
}

for (var i = 0; i < 20; i = i + 1) {
	print fib(i);
}
```

