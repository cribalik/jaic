@echo off

rem set compiler_flags=-Od -MT -nologo -fp:fast -fp:except- -Gm- -GR- -Zo -WX -W4 -wd4201 -wd4100 -wd4189 -wd4505 -wd4127 -FC -Z7
set compiler_flags=-Od -nologo -fp:fast -fp:except- -W4 -wd4505 -wd4201 -wd4408 -Z7
rem -D_CRT_SECURE_NO_WARNINGS=1
set linker_flags=-incremental:no -opt:ref -debug:full -profile

IF NOT EXIST .\build mkdir .\build

cl %compiler_flags% .\src\zvm.cpp -Fe.\build\zvm.exe -link %linker_flags% -debug
cl %compiler_flags% .\src\assembler.cpp -Fe.\build\zasm.exe -link %linker_flags% -debug
cl %compiler_flags% .\src\compiler.cpp -Fe.\build\zenc.exe -link %linker_flags% -debug
