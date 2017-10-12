#!/usr/bin/env sh
dir="$(dirname "$0")"

gcc -ansi -pedantic -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/compiler.c" -o "$dir/build/zenc"
gcc -ansi -pedantic -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/assembler.c" -o "$dir/build/zasm"