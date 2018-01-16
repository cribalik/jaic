#!/usr/bin/env sh
dir="$(dirname "$0")"

g++ -ansi -pedantic -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/compiler.c" -o "$dir/build/zenc"
g++ -ansi -pedantic -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/assembler.c" -o "$dir/build/zasm"
g++ -ansi -pedantic -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/zvm.c" -o "$dir/build/zvm"