# golang-gdb.py

GDB python extension for golang.

The script is based on runtime-gdb.py script shipped with golang, and it has following improvement.
1. print stack backtrace of goroutine for both running process and core dump.
2. switch to a specific goroutine, and examing variables of each call frames of the goroutine.

## How to use

### Load script
(gdb) source /path/to/the/script/golang-gdb.py

### List all goroutines:
(gdb) info goroutine

### Print all goroutine stacks
(gdb) goroutine all bt

### Swith to goroutine with goid 2, and examine frame 5
(gdb) goroutine 2

(gdb) goroutine frame 5

(gdb) goroutine info locals

(gdb) goroutine info args

(gdb) goroutine print var1

### Online help
(gdb) help goroutine
