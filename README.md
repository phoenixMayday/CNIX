# CNIX

## Requirements

- **gcc**
- **make**
- **libc6-dev**
- **x86-64 Linux system**

All dependencies can be installed with `apt`:

```
sudo apt update
sudo apt install build-essential
```

## Build + run:

```
make run FILE=./examples/example.cnix 
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
			| [scope]
			| if ([expression]) [scope] [else]
			| for ([for-init]; [for-condition]; [for-increment])
			
[for-init] = [statement]
		   | ε
		   
[for-condition] = [expression]
		        | ε
 
[for-increment] = [statement]
		        | ε

[scope] = { [statement]* }
        | [statement]

[else] = else [scope]
	   | ε

[program] = [statement]*
```

## Useful commands:

Compile with intermidary files and optimisation:
```
gcc -save-temps -O2 ctest.c -o ctest
```

Assemble assembly files with GCC:
```
gcc -nostdlib asmtest.s -o asmtest
```