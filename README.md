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


Built-in Functions
------------------
 - `clock()`: Returns the time since **January 1 1970, 00:00:00** (UNIX-epoch) in seconds
 - `sleep(<time-in-seconds>)`: Pause execution of the script for the given time
 - `string(<value>)`: Convert a Lox object to its string representation
 - `len(<string>)`: Calculate the length of a string in bytes
 - `instanceof(<instance>, <class>)`: Check whether an instance is of a specific class.
 - `hasattr(<instance>, <name>)`,
 - `getattr(<instance>, <name>)`,
 - `delattr(<instance>, <name>)` and
 - `setattr(<instance>, <name>, <value>)`: Functions for manipulating instance attributes


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


Building
--------
From the project root directory run:

```bash
go build
```
