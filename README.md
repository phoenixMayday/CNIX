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
[type] = byte | word | long | qword
       | int8 | int16 | int32 | int64
       | uint8 | uint16 | uint32 | uint64
       | char

[pointer-type] = [type] *

[array-type] = [type][]

[var-type] = [type]
           | [pointer-type]
           | [array-type]

[term] = integer_literal
       | identifier
       | identifier[[expression]]        // array indexing
       | ([expression])
       | &identifier                     // address-of
       | *[expression]                   // dereference
       | alloc([expression])             // heap allocation
       | free([expression])              // heap deallocation
       | {[expression-list]}             // stack array allocation
       | [type]([expression])            // type conversion
       | 'char_lit'                      // character
       | "[string]"                      // string

[expression-list] = [expression]
                  | [expression], [expression-list]
                  | ε

[string] = char_lit[string]
         | ε

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
            | [var-type] identifier = [expression];
            | identifier = [expression];
            | identifier[[expression]] = [expression];  // array assignment
            | [scope]
            | if ([expression]) [scope] [else]
            | for ([for-init] [for-condition]; [for-increment])
            
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