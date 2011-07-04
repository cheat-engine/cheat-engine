@echo off
set copycmd=/Y
copy cheatengine.exe "release\beta\Cheat Engine.exe"
copy emptyprocess.exe "release\beta"
copy kernelmoduleunloader.exe "release\beta"
copy systemcallretriever.exe "release\beta"
copy systemcallsignal.exe "release\beta"
copy tutorial\project1.exe "release\beta\tutorial.exe"
copy dbk32.dll "release\beta"
copy dbk32.sys "release\beta"
copy cehook.dll "release\beta"
copy dxhook.dll "release\beta"
copy stealth.dll "release\beta"
copy pscan.dll "release\beta"
copy directxhook\d3dx81ab.dll "release\beta"
copy directxhook\d3dx9.dll "release\beta"
copy "cheat engine net\client\client.exe" "release\beta\Cheat Engine Client.exe"
copy "cheat engine net\server\ceserver.exe" "release\beta\Cheat Engine Server.exe"
cd release\beta
del *.rar
start /WAIT winrar A -m5 ce.rar *.*
cd ../..
echo done
