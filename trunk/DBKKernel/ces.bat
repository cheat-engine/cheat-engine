@echo off
command /C echo changing to dos-16 file structure
set C_DEFINES=

set copycmd=/Y
copy sources.cesigned sources

build -cZ
if %ERRORLEVEL%==0 goto success
goto error

:success
	if "%AMD64%"=="1" goto x86success

	copy .\obj%BUILD_ALT_DIR%\i386\dbk.sys "..\Cheat Engine\bin\dbk32.sys"
        signtool sign /ac "..\cheat engine\release\sig\GlobalSign Root CA.crt" /n "Cheat Engine" /t http://timestamp.globalsign.com/scripts/timstamp.dll "..\Cheat Engine\bin\dbk32.sys"
	goto successend

:x86success:
	copy .\obj%BUILD_ALT_DIR%\amd64\dbk.sys "..\Cheat Engine\bin\dbk64.sys"
        signtool sign /ac "..\cheat engine\release\sig\GlobalSign Root CA.crt" /n "Cheat Engine" /t http://timestamp.globalsign.com/scripts/timstamp.dll "..\Cheat Engine\bin\dbk64.sys"

	siggen\siggen.exe "..\Cheat Engine\bin\cheatengine-i386.exe"
	siggen\siggen.exe "..\Cheat Engine\bin\cheatengine-x86_64.exe"
	siggen\siggen.exe "..\Cheat Engine\bin\vmdisk.img"


	goto successend




:error
echo.
echo error. Check the compile log
goto exit

:successend
echo.
echo done
if "%AMD64%"=="1" echo Please verify the file is signed




:exit