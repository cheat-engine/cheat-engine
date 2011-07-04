@echo off
echo Continue by pressing ^<ENTER^> to overwrite the old JCL version of
echo this unit with a freshly created one (from prototype).
echo To cancel this operation press ^<CTRL+C^> or close this window!
echo.
echo.
pause
rem Remove the nonJCL parts
set tempname=.\Temp_Hardlinks.pas
simple_pp.pl ..\Hardlinks.pas JCL > "%tempname%"
simple_pp.pl %tempname% !PROTOTYPE > "..\..\windows\Hardlinks.pas"
del /f "%tempname%"

