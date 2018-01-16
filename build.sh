#!/usr/bin/env sh
dir="$(dirname "$0")"

g++ -ansi -pedantic -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/compiler.cpp" -o "$dir/build/zenc"
g++ -ansi -pedantic -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/assembler.cpp" -o "$dir/build/zasm"
g++ -ansi -pedantic -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/zvm.cpp" -o "$dir/build/zvm"