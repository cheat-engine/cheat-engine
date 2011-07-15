@echo off
command /C echo changing to dos-16 file structure
set C_DEFINES=

set copycmd=/Y
copy sources.ce sources

build -cZ
if %ERRORLEVEL%==0 goto success
goto error

:success
	if "%AMD64%"=="1" goto x86success

	copy .\obj%BUILD_ALT_DIR%\i386\dbk.sys "..\Cheat Engine\bin\dbk32.sys"
	goto successend

:x86success:
	copy .\obj%BUILD_ALT_DIR%\amd64\dbk.sys "..\Cheat Engine\bin\dbk64.sys"
	goto successend




:error
echo.
echo error. Check the compile log
goto exit

:successend
echo.
echo done
if "%AMD64%"=="1" echo Don't forget to sign your driver


:exit