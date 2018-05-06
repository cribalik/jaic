#!/usr/bin/env sh
dir="$(dirname "$0")"


g++ -ansi -pedantic -std=c++11 -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/compiler.cpp" -o "$dir/build/zenc" -fsanitize=address -DASAN_OPTIONS=abort_on_error=1 
g++ -ansi -pedantic -std=c++11 -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/assembler.cpp" -o "$dir/build/zasm" -fsanitize=address -DASAN_OPTIONS=abort_on_error=1 
g++ -ansi -pedantic -std=c++11 -g -Wall -Wextra -Wno-unused-function -DLINUX "$dir/src/zvm.cpp" -o "$dir/build/zvm" -fsanitize=address -DASAN_OPTIONS=abort_on_error=1 
