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
        "c:\Program Files (x86)\Windows Kits\8.1\bin\x64\signtool.exe" sign /a /sha1 D010B04E1B71FF1B91A0688B07DB740B5D29D4DC /ac "..\cheat engine\release\sig\GlobalSign Root CA.crt" /t http://timestamp.globalsign.com/scripts/timstamp.dll "..\Cheat Engine\bin\dbk32.sys"
	"c:\Program Files (x86)\Windows Kits\8.1\bin\x64\signtool.exe" sign /a /sha1 A9F93214813B99645897CA59E4E83EBA4F65818B /ac "..\cheat engine\release\sig\GlobalSign Root CA.crt" /tr http://timestamp.globalsign.com/?signature=sha2 /td SHA256 /fd SHA256 /as "..\Cheat Engine\bin\dbk32.sys



	goto successend

:x86success:
	copy .\obj%BUILD_ALT_DIR%\amd64\dbk.sys "..\Cheat Engine\bin\dbk64.sys"
        "c:\Program Files (x86)\Windows Kits\8.1\bin\x64\signtool.exe" sign /a /sha1 D010B04E1B71FF1B91A0688B07DB740B5D29D4DC /ac "..\cheat engine\release\sig\GlobalSign Root CA.crt" /t http://timestamp.globalsign.com/scripts/timstamp.dll "..\Cheat Engine\bin\dbk64.sys"
	"c:\Program Files (x86)\Windows Kits\8.1\bin\x64\signtool.exe" sign /a /sha1 A9F93214813B99645897CA59E4E83EBA4F65818B /ac "..\cheat engine\release\sig\GlobalSign Root CA.crt" /tr http://timestamp.globalsign.com/?signature=sha2 /td SHA256 /fd SHA256 /as "..\Cheat Engine\bin\dbk64.sys


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