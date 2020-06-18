#!/bin/bash
export DISPLAY=:0


#nasm -f elf64  -o target/Adder/prog.o target/Adder/prog.s
#clang-9 -g   -o target/Adder/prog.run target/Adder/prog.o rts/main.c

nasm -f elf -o target/Adder/prog.o target/Adder/prog.s
clang-9 -g -S -m32 -o target/Adder/main rts/main.c
clang-9 -g -m32 -o target/Adder/prog.run target/Adder/prog.o rts/main.c

./target/Adder/prog.run