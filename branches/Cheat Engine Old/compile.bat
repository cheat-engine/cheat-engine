@echo off

if "%1"=="234" goto ok

echo Please, use delphi and the ddk to compile yourself
start delphi32 cheatengine.dpr
goto exit

:ok

cd injectedpointerscan
dcc32 -b pscan
if errorlevel 1 goto exit_error
echo ok

cd ..

cd standalone
dcc32 -b trainerwithassembler
if errorlevel 1 goto exit_error

cd ..
upx -9 trainerwithassembler.exe

cd cehook
dcc32 -b cehook
if errorlevel 1 goto exit_error
cd..


cd "Cheat Engine Net"
cd client
dcc32 -b client
if errorlevel 1 goto exit_error

cd ..
cd Server
dcc32 -b ceserver
if errorlevel 1 goto exit_error

cd ..
cd ..

cd stealth
dcc32 -b stealth
if errorlevel 1 goto exit_error
cd ..

cd directxhook
dcc32 -b dxhook
if errorlevel 1 goto exit_error
cd ..

cd tutorial
dcc32 -b project1
if errorlevel 1 goto exit_error
cd ..

cd dbk32
dcc32 -b dbk32
if errorlevel 1 goto exit_error
cd "Kernelmodule unloader"

dcc32 -b kernelmoduleunloader
if errorlevel 1 goto exit_error
cd ..
cd ..

cd SystemcallRetriever
dcc32 -b emptydll
if errorlevel 1 goto exit_error
dcc32 -b emptyprocess
if errorlevel 1 goto exit_error
dcc32 -b systemcallsignal
if errorlevel 1 goto exit_error
dcc32 -b Systemcallretriever
if errorlevel 1 goto exit_error
cd ..

cd dbkkernel

rem imagine what this does to the command promp
command /?

call ce
if errorlevel 1 goto exit_error
cd..


brc32 -r trainer
dcc32 -b cheatengine
if errorlevel 1 goto exit_error
goto exit

:exit_error
echo Error while compiling. Fix it!!!

:exit
echo Bye
