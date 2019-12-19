del *.exe
del *.dll
del *.lib
del *.o
del *.obj
cl /MT /O2 /c /DLUA_BUILD_AS_DLL /DLUA_USER_H=\"lua_user.h\" *.c
ren lua.obj lua.o
ren luac.obj luac.o
copy luac.o luac.obj
link /DLL /IMPLIB:lua53-64.lib /OUT:lua53-64.dll *.obj
link /OUT:lua64.exe lua.o lua53-64.lib
lib /OUT:lua53-64-static.lib *.obj
link /OUT:luac64.exe luac.o lua53-64-static.lib
copy lua53-64.dll "d:\svn\Cheat Engine\bin"