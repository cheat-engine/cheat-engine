@echo off
command /C echo changing to dos-16 file structure
set C_DEFINES=

set LINKER_FLAGS=/INTEGRITYCHECK

set copycmd=/Y
copy sources.ce sources

build -cZ
if %ERRORLEVEL%==0 goto success
goto error

:success
	if "%AMD64%"=="1" goto x86success

	copy .\obj%BUILD_ALT_DIR%\i386\dbk.sys "..\Cheat Engine\bin\dbk32.sys"
        copy .\obj%BUILD_ALT_DIR%\i386\dbk.sys .\obj%BUILD_ALT_DIR%\i386\dbk32.sys
        copy .\obj%BUILD_ALT_DIR%\i386\dbk.pdb .\obj%BUILD_ALT_DIR%\i386\dbk32.pdb

	goto successend

:x86success:
	copy .\obj%BUILD_ALT_DIR%\amd64\dbk.sys "..\Cheat Engine\bin\dbk64.sys"
        copy .\obj%BUILD_ALT_DIR%\amd64\dbk.sys .\obj%BUILD_ALT_DIR%\amd64\dbk64.sys
        copy .\obj%BUILD_ALT_DIR%\amd64\dbk.pdb .\obj%BUILD_ALT_DIR%\amd64\dbk64.pdb
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