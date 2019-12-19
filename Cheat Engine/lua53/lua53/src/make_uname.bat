@echo off
REM This builds all the libraries of the folder for 1 uname

call tecmake %1 %2 %3 %4 %5 %6 %7 %8

if "%1"==""          goto end
if "%1"=="mingw4"    goto luaexe
if "%1"=="mingw4_64" goto luaexe
if "%1"=="cygw17"    goto luaexe
if "%1"=="vc10"      goto luaexe
if "%1"=="vc10_64"   goto luaexe
if "%1"=="all"       goto lua_all
goto end

:lua_all
call make_uname mingw4
call make_uname mingw4_64
call make_uname cygw17
goto end

:luaexe
call tecmake %1 "MF=lua" %2 %3 %4 %5 %6 %7
call tecmake %1 "MF=wlua" %2 %3 %4 %5 %6 %7
call tecmake %1 "MF=luac" %2 %3 %4 %5 %6 %7
if "%1"=="mingw4"    copy /Y ..\lib\dllw4\*.dll* ..\bin\Win32\
if "%1"=="mingw4_64" copy /Y ..\lib\dllw4_64\*.dll* ..\bin\Win64\
if "%1"=="cygw17"    copy /Y ..\lib\cygw17\*.dll ..\bin\cygw17\
if "%1"=="vc10"      copy /Y ..\lib\dll10\*.dll* ..\bin\Win32\
if "%1"=="vc10_64"   copy /Y ..\lib\dll10_64\*.dll* ..\bin\Win64\
goto end

:end
