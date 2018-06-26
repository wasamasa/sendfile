@echo off

REM No feature detection for now, as none of the supported
REM features work on Windows anyway.  TODO: Figure out if we
REM can make mmap work, given that the memory-mapped-files egg
REM works on Windows....
set feature_flags=
%CHICKEN_CSC% %feature_flags% -C %CFLAGS% -L %LDFLAGS% %*
