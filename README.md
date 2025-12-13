# CNIX

Compile with intermidary files and optimisation:
```
gcc -save-temps -O2 ctest.c -o ctest
```

Assemble assembly files with GCC:
```
gcc -nostdlib asmtest.s -o asmtest
```

## Grammar

```
[term] = integer_literal
       | indentifer
       | ([expression])
     
[expression] = [expression] + [expression]
		     | [expression] - [expression]
		     | [expression] * [expression]
		     | [expression] / [expression]
		     | [expression] > [expression]
		     | [expression] >= [expression]
		     | [expression] < [expression]
		     | [expression] <= [expression]
		     | [expression] == [expression]
		     | [expression] & [expression]
		     | [expression] | [expression]
		     | [term]

[statement] = exit([expression]);
			| var identifier = [expression];
			| identifier = [expression];
			| if ([expression]) [scope] [else]

[scope] = { [statement]* }
        | [statement]

[else] = else [scope]
	   | ε

[program] = [statement]*
```