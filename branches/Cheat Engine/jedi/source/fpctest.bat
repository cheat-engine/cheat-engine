@echo   Free Pascal Compiler test...
@echo   For error messages, see fpctest.err.
@echo off
@if "%1"=="" goto usage
@if not exist %1. goto invpath
@del fpctest.err
@for %%f in (common\Jcl*.pas windows\Jcl*.pas) do make file=%%f fpc=%1 -fMakefile.fpc
@goto exit
:invpath
@echo invalid path "%1"
@goto exit
:usage
@echo   usage: fpctest ^<path to fpc installation^>
:exit