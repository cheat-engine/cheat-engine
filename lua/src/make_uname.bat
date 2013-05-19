@echo off
REM This builds all the libraries of the folder for 1 uname

call tecmake %1 %2 %3 %4 %5 %6 %7 %8

if "%1"==""         goto end
if "%1"=="vc8"      goto luaexe
if "%1"=="vc8_64"   goto luaexe
if "%1"=="cygw15"   goto luaexe
if "%1"=="cygw17"   goto luaexe
if "%1"=="dll8"     goto luadll8
if "%1"=="dll8_64"  goto luadll8_64
if "%1"=="all"      goto lua_all
goto end

:lua_all
call make_uname vc8
call make_uname vc8_64
call make_uname cygw15
call make_uname cygw17
if "%1"=="all"  goto luadll8
goto end

:luaexe
call tecmake %1 "MF=lua" %2 %3 %4 %5 %6 %7
call tecmake %1 "MF=wlua" %2 %3 %4 %5 %6 %7
call tecmake %1 "MF=luac" %2 %3 %4 %5 %6 %7
call tecmake %1 "MF=bin2c" %2 %3 %4 %5 %6 %7
if "%1"=="cygw15"  copy /Y ..\lib\cygw15\*.dll ..\bin\cygw15\
if "%1"=="cygw17"  copy /Y ..\lib\cygw17\*.dll ..\bin\cygw17\
goto end

:luadll8
copy /Y ..\lib\dll8\*.dll* ..\bin\Win32\
if "%1"=="all"  goto luadll8_64
goto end

:luadll8_64
copy /Y ..\lib\dll8_64\*.dll* ..\bin\Win64\
if "%1"=="all"  goto luadllproxy
goto end

:luadllproxy
call tecmake dll "MF=dllproxy" %2 %3 %4 %5 %6 %7
call tecmake dll7 "MF=dllproxy" %2 %3 %4 %5 %6 %7
call tecmake dll8 "MF=dllproxy" %2 %3 %4 %5 %6 %7
call tecmake dll8_64 "MF=dllproxy" %2 %3 %4 %5 %6 %7
call tecmake dll9 "MF=dllproxy" %2 %3 %4 %5 %6 %7
call tecmake dll9_64 "MF=dllproxy" %2 %3 %4 %5 %6 %7
goto end

:end
