unit CustomTypeHandler;

{$mode delphi}
{
This class is used as a wrapper for different kinds of custom types
}

interface





{$ifdef jni} //not yet implemented, but the interface is available
uses
  Classes, SysUtils, math;

type PLua_state=pointer;
{$else}
uses
  dialogs, Classes, SysUtils,cefuncproc, lua, lauxlib, lualib,
  math, commonTypeDefs;
{$endif}

type TConversionRoutine=function(data: pointer):integer; stdcall;
type TReverseConversionRoutine=procedure(i: integer; output: pointer); stdcall;

//I should have used cdecl from the start
type TConversionRoutine2=function(data: pointer; address: ptruint):integer; cdecl;
type TReverseConversionRoutine2=procedure(i: integer; address: ptruint; output: pointer); cdecl;


type
  TCustomTypeType=(cttAutoAssembler, cttLuaScript, cttPlugin);
  TCustomType=class
  private
    fname: string;
    ffunctiontypename: string; //lua

    lua_bytestovaluefunctionid: integer;
    lua_valuetobytesfunctionid: integer;
    lua_bytestovalue: string; //help string that contains the functionname so it doesn't have to build up this string at runtime
    lua_valuetobytes: string;


    routine: pointer;
    reverseroutine: pointer;


    {$ifndef jni}
    c: TCEAllocArray;
    ce: TCEExceptionListArray;
    {$endif}
    currentscript: tstringlist;
    fCustomTypeType: TCustomTypeType; //plugins set this to cttPlugin
    fScriptUsesFloat: boolean;
    fScriptUsesCDecl: boolean;



    procedure unloadscript;
    procedure setName(n: string);
    procedure setfunctiontypename(n: string);
  public

    bytesize: integer;
    preferedAlignment: integer;

    //these 4 functions are just to make it easier
    procedure ConvertToData(f: single; output: pointer; address: ptruint); overload;
    function ConvertFromData(data: pointer; address: ptruint): single; overload;
    procedure ConvertToData(i: integer; output: pointer; address: ptruint); overload;
    function ConvertFromData(data: pointer; address: ptruint): integer; overload;

    function ConvertDataToInteger(data: pointer; address: ptruint): integer;
    function ConvertDataToIntegerLua(data: pbytearray; address: ptruint): integer;
    procedure ConvertIntegerToData(i: integer; output: pointer; address: ptruint);
    procedure ConvertIntegerToDataLua(i: integer; output: pbytearray; address: ptruint);

    function ConvertDataToFloat(data: pointer; address: ptruint): single;
    function ConvertDataToFloatLua(data: pbytearray; address: ptruint): single;
    procedure ConvertFloatToData(f: single; output: pointer; address: ptruint);
    procedure ConvertFloatToDataLua(f: single; output: pbytearray; address: ptruint);



    function getScript:string;
    procedure setScript(script:string; luascript: boolean=false);
    constructor CreateTypeFromAutoAssemblerScript(script: string);
    constructor CreateTypeFromLuaScript(script: string);
    destructor destroy; override;

    procedure remove;  //call this instead of destroy
    procedure showDebugInfo;
  published
    property name: string read fName write setName;
    property functiontypename: string read ffunctiontypename write setfunctiontypename; //lua
    property CustomTypeType: TCustomTypeType read fCustomTypeType;
    property script: string read getScript write setScript;
    property scriptUsesFloat: boolean read fScriptUsesFloat write fScriptUsesFloat;
  end;
  PCustomType=^TCustomType;

function GetCustomTypeFromName(name:string):TCustomType; //global function to retrieve a custom type

function registerCustomTypeLua(L: PLua_State): integer; cdecl;
function registerCustomTypeAutoAssembler(L: PLua_State): integer; cdecl;

var customTypes: TList; //list holding all the custom types
//    AllIncludesCustomType: boolean;
    MaxCustomTypeSize: integer;

implementation

{$ifndef jni}
uses mainunit, LuaHandler, LuaClass,autoassembler;
{$endif}

resourcestring
  rsACustomTypeWithNameAlreadyExists = 'A custom type with name %s already '
    +'exists';
  rsACustomFunctionTypeWithNameAlreadyExists = 'A custom function type with '
    +'name %s already exists';
  rsFailureCreatingLuaObject = 'Failure creating lua object';
  rsOnlyReturnTypenameBytecountAndFunctiontypename = 'Only return typename, '
    +'bytecount and functiontypename';
  rsBytesizeIs0 = 'bytesize is 0';
  rsInvalidFunctiontypename = 'invalid functiontypename';
  rsInvalidTypename = 'invalid typename';
  rsUndefinedError = 'Undefined error';
  rsCTHParameter3IsNotAValidFunction = 'Parameter 3 is not a valid function';
  rsCTHParameter4IsNotAValidFunction = 'Parameter 4 is not a valid function';
  rsCTHInvalidNumberOfParameters = 'Invalid number of parameters';

function GetCustomTypeFromName(name:string): TCustomType;
var i: integer;
begin
  result:=nil;

  for i:=0 to customTypes.Count-1 do
  begin
    if uppercase(TCustomType(customtypes.Items[i]).name)=uppercase(name) then
    begin
      result:=TCustomType(customtypes.Items[i]);
      break;
    end;
  end;
end;

procedure TCustomType.setName(n: string);
var i: integer;
begin
  //check if there is already a script with this name (and not this one)
  for i:=0 to customtypes.count-1 do
    if uppercase(TCustomType(customtypes[i]).name)=uppercase(n) then
    begin
      if TCustomType(customtypes[i])<>self then
        raise exception.create(Format(rsACustomTypeWithNameAlreadyExists, [n]));
    end;

  fname:=n;
end;

procedure TCustomType.setfunctiontypename(n: string);
var i: integer;
begin
  //check if there is already a script with this functiontype name (and not this one)
  for i:=0 to customtypes.count-1 do
    if uppercase(TCustomType(customtypes[i]).functiontypename)=uppercase(n) then
    begin
      if TCustomType(customtypes[i])<>self then
        raise exception.create(Format(rsACustomFunctionTypeWithNameAlreadyExists, [n]));
    end;

  ffunctiontypename:=n;

  lua_bytestovalue:=n+'_bytestovalue';
  lua_valuetobytes:=n+'_valuetobytes';
end;

function TCustomType.getScript: string;
begin
  if ((fCustomTypeType=cttAutoAssembler) or (fCustomTypeType=cttLuaScript)) and (currentscript<>nil) then
    result:=currentscript.text
  else
    result:='';
end;

procedure TCustomType.ConvertIntegerToDataLua(i: integer; output: pbytearray; address: ptruint);
var
  L: PLua_State;
  r: integer;
  c,b: integer;
begin
{$ifndef jni}
  l:=LuaVM;

  if lua_valuetobytesfunctionid=-1 then
  begin
    lua_getglobal(LuaVM, pchar(lua_valuetobytes));
    lua_valuetobytesfunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);
  end;
  lua_rawgeti(Luavm, LUA_REGISTRYINDEX, lua_valuetobytesfunctionid);

  lua_pushinteger(L, i);
  lua_pushinteger(L, address);
  if lua_pcall(l,2,bytesize,0)=0 then
  begin
    r:=lua_gettop(L);
    if r>0 then
    begin
      b:=0;
      for c:=-r to -1 do
      begin
        output[b]:=lua_tointeger(L, c);
        inc(b);
      end;

      lua_pop(L,r);
    end;
  end;

{$endif}

end;

procedure TCustomType.ConvertIntegerToData(i: integer; output: pointer; address: ptruint);
var f: single;
begin

  if scriptUsesFloat then //convert to a float and pass that
  begin
    f:=i;
    i:=pdword(@f)^;
  end;

  if assigned(reverseroutine) then
  begin
    if fScriptUsesCDecl then
      TReverseConversionRoutine2(reverseroutine)(i,address, output)
    else
      TReverseConversionRoutine(reverseroutine)(i,output);
  end
  else
  begin
    //possible lua
    if fCustomTypeType=cttLuaScript then
      ConvertIntegerToDataLua(i, output, address);
  end;
end;

function TCustomType.ConvertDataToIntegerLua(data: pbytearray; address: ptruint): integer; //split up for speed
var
  L: PLua_State;
  i: integer;
begin
  {$IFNDEF jni}
  l:=LuaVM;


    result:=0;

    if lua_bytestovaluefunctionid=-1 then
    begin
      lua_getglobal(LuaVM, pchar(lua_bytestovalue));
      lua_bytestovaluefunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);
    end;


   // messagebox(0,'going to call rawgeti','bla',0);
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, lua_bytestovaluefunctionid);


   // messagebox(0,'after call rawgeti','bla',0);


    for i:=0 to bytesize-1 do
      lua_pushinteger(L,data[i]);

    lua_pushinteger(L, address);

    lua_call(L, bytesize+1,1);
    {
    if lua_pcall(L, bytesize+1,1, 0)<>0 then
    begin
      Log('customtype error:'+Lua_ToString(L,-1));
    end;
    }
    result:=lua_tointeger(L, -1);

    lua_pop(L,lua_gettop(l));

  {$ENDIF}

end;



function TCustomType.ConvertDataToInteger(data: pointer; address: ptruint): integer;
var
  i: dword;
  f: single absolute i;
begin
  if assigned(routine) then
  begin
    if fScriptUsesCDecl then
      result:=TConversionRoutine2(routine)(data, address)
    else
      result:=TConversionRoutine(routine)(data);
  end
  else
  begin
    //possible lua
    if fCustomTypeType=cttLuaScript then
      result:=ConvertDataToIntegerLua(data, address)
    else
      result:=0;
  end;

  if fScriptUsesFloat then //the result is still in float state
  begin
    i:=result;
    result:=trunc(f);
  end;
end;


procedure TCustomType.ConvertFloatToDataLua(f: single; output: pbytearray; address: ptruint);
//I REALLY doubt anyone in their right mind would use lua to encode a float as bytes, but it's here...
var
  L: PLua_State;
  r: integer;
  c,b: integer;
begin
  {$IFNDEF jni}
  l:=LuaVM;

    if lua_valuetobytesfunctionid=-1 then
    begin
      lua_getglobal(L, pchar(lua_valuetobytes));
      lua_valuetobytesfunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);
    end;
    lua_rawgeti(L, LUA_REGISTRYINDEX, lua_valuetobytesfunctionid);

    lua_pushnumber(L, f);
    lua_pushinteger(L, address);
    if lua_pcall(l,2,bytesize,0)=0 then
    begin
      r:=lua_gettop(L);
      if r>0 then
      begin
        b:=0;
        for c:=-r to -1 do
        begin
          output[b]:=lua_tointeger(L, c);
          inc(b);
        end;

        lua_pop(L,r);
      end;
    end;


  {$ENDIF}
end;


procedure TCustomType.ConvertFloatToData(f: single; output: pointer; address: ptruint);
var i: integer;
begin

  i:=pdword(@f)^; //convert the f to a integer without conversion (reverseroutine takes an integer, but could be any 32-bit value really)

  if not scriptUsesFloat then //WHY even call this ?
    i:=trunc(f);

  if assigned(reverseroutine) then
  begin
    if fScriptUsesCDecl then
      TReverseConversionRoutine2(reverseroutine)(i,address, output)
    else
      TReverseConversionRoutine(reverseroutine)(i,output);
  end
  else
  begin
    //possible lua
    if fCustomTypeType=cttLuaScript then
      ConvertFloatToDataLua(f, output, address);
  end;
end;

function TCustomType.ConvertDataToFloatLua(data: PByteArray; address: ptruint): single;
//again, why would anyone use lua for this ?
var
  L: PLua_State;
  i: integer;
begin
  {$IFNDEF jni}
  l:=LuaVM;


    if lua_bytestovaluefunctionid=-1 then
    begin
      lua_getglobal(L, pchar(lua_bytestovalue));
      lua_bytestovaluefunctionid:=luaL_ref(L,LUA_REGISTRYINDEX);
    end;


    lua_rawgeti(L, LUA_REGISTRYINDEX, lua_bytestovaluefunctionid);


    for i:=0 to bytesize-1 do
      lua_pushinteger(L,data[i]);

    lua_pushinteger(L, address);

    lua_call(L, bytesize+1,1);
    result:=lua_tonumber(L, -1);

    lua_pop(L,lua_gettop(l));

  {$ENDIF}

end;



function TCustomType.ConvertDataToFloat(data: pointer; address: ptruint): single;
var
  i: dword;
  f: single absolute i;
begin
  if assigned(routine) then
  begin
    if fScriptUsesCDecl then
      i:=TConversionRoutine2(routine)(data,address)
    else
      i:=TConversionRoutine(routine)(data);

    if not fScriptUsesFloat then //the result is in integer format ,
      f:=i; //convert the integer to float
  end
  else
  begin
    //possible lua
    if fCustomTypeType=cttLuaScript then
      f:=ConvertDataToFloatLua(data, address)
    else
      f:=0;
  end;

  result:=f;
end;

procedure TCustomType.ConvertToData(f: single; output: pointer; address: ptruint);
begin
  ConvertFloatToData(f, output, address);
end;

function TCustomType.ConvertFromData(data: pointer; address: ptruint): single;
begin
  result:=ConvertDataToFloat(data, address);
end;

procedure TCustomType.ConvertToData(i: integer; output: pointer; address: ptruint);
begin
  ConvertIntegerToData(i, output, address);
end;

function TCustomType.ConvertFromData(data: pointer; address: ptruint): integer;
begin
  result:=ConvertDataToInteger(data, address);
end;

procedure TCustomType.unloadscript;
begin
  {$IFNDEF jni}
  if fCustomTypeType=cttAutoAssembler then
  begin
    routine:=nil;
    reverseroutine:=nil;

    if currentscript<>nil then
    begin
      autoassemble(currentscript,false, false, false, true, c, ce); //popupmessages is false so it won't complain if there is no disable section
      freeandnil(currentscript);
    end;
  end;
    {$ENDIF}

end;


procedure TCustomType.setScript(script:string; luascript: boolean=false);
var i: integer;
  s: tstringlist;
  error:pchar;

  //lua vars
  returncount: integer;

//  templua: Plua_State;

  ftn,tn: pchar;

  oldname: string;
  oldfunctiontypename: string;
  newpreferedalignment, oldpreferedalignment: integer;
  oldScriptUsesFloat, newScriptUsesFloat: boolean;
  oldScriptUsesCDecl, newScriptUsesCDecl: boolean;
  newroutine, oldroutine: pointer;
  newreverseroutine, oldreverseroutine: pointer;
  newbytesize, oldbytesize: integer;

{$IFNDEF jni}
  oldallocarray: TCEAllocArray;
{$ENDIF}
begin

  {$IFNDEF jni}
  oldname:=fname;
  oldfunctiontypename:=ffunctiontypename;
  oldroutine:=routine;
  oldreverseroutine:=reverseroutine;
  oldbytesize:=bytesize;
  oldpreferedalignment:=preferedalignment;
  oldScriptUsesFloat:=fScriptUsesFloat;
  oldScriptUsesCDecl:=fScriptUsesCDecl;

  setlength(oldallocarray, length(c));
  for i:=0 to length(c)-1 do
    oldallocarray[i]:=c[i];

  try
    //if anything goes wrong the old values get set back

    if not luascript then
    begin
      setlength(c,0);
      s:=tstringlist.create;
      try
        s.text:=script;

        if autoassemble(s,false, true, false, true, c, ce) then
        begin
          newpreferedalignment:=-1;
          newScriptUsesFloat:=false;
          newScriptUsesCDecl:=false;

          //find alloc "ConvertRoutine"
          for i:=0 to length(c)-1 do
          begin
            if uppercase(c[i].varname)='TYPENAME' then
              name:=pchar(c[i].address);

            if uppercase(c[i].varname)='CONVERTROUTINE' then
              newroutine:=pointer(c[i].address);

            if uppercase(c[i].varname)='BYTESIZE' then
              newbytesize:=pinteger(c[i].address)^;

            if uppercase(c[i].varname)='PREFEREDALIGNMENT' then
              newpreferedalignment:=pinteger(c[i].address)^;

            if uppercase(c[i].varname)='USESFLOAT' then
              newScriptUsesFloat:=pbyte(c[i].address)^<>0;

            if uppercase(c[i].varname)='CALLMETHOD' then
               newScriptUsesCDecl:=pbyte(c[i].address)^<>0;

            if uppercase(c[i].varname)='CONVERTBACKROUTINE' then
              newreverseroutine:=pointer(c[i].address);
          end;

          if newpreferedalignment=-1 then
            newpreferedalignment:=newbytesize;



          //still here
          unloadscript; //unload the old script

          //and now set the new values
          bytesize:=newbytesize;
          routine:=newroutine;
          reverseroutine:=newreverseroutine;

          preferedAlignment:=newpreferedalignment;
          fScriptUsesFloat:=newScriptUsesFloat;
          fScriptUsesCDecl:=newScriptUsesCDecl;

          fCustomTypeType:=cttAutoAssembler;
          if currentscript<>nil then
            freeandnil(currentscript);

          currentscript:=tstringlist.create;
          currentscript.text:=script;



        end;

      finally
        s.free;
      end;

    end
    else
    begin
      try
        lua_pop(luavm, lua_gettop(luavm));
        if lua_dostring(luavm, pchar(script))=0 then //success, lua script loaded
        begin
          returncount:=lua_gettop(luavm);
          if returncount<>3 then
            raise exception.create(rsOnlyReturnTypenameBytecountAndFunctiontypename);

          //-1=functiontypename
          //-2=bytecount
          //-3=typename
          ftn:=lua.lua_tostring(luavm,-1);
          bytesize:=lua_tointeger(luavm,-2);
          tn:=lua.lua_tostring(luavm,-3);

          if bytesize=0 then raise exception.create(rsBytesizeIs0);
          if ftn=nil then raise exception.create(rsInvalidFunctiontypename);
          if tn=nil then raise exception.create(rsInvalidTypename);

          name:=tn;
          functiontypename:=ftn;

        end
        else
        begin
          //something went wrong
          if lua_gettop(luavm)>0 then
          begin
            error:=lua.lua_tostring(luavm,-1);
            raise exception.create(error);
          end else raise exception.create(rsUndefinedError);
        end;

      finally
        lua_pop(luavm, lua_gettop(luavm));
      end;
      //still here so the script got loaded and passed the tests

      fCustomTypeType:=cttLuaScript;
      if currentscript=nil then
        currentscript:=tstringlist.create;

      currentscript.text:=script;


      lua_getglobal(LuaVM, pchar(lua_bytestovalue));
      lua_bytestovaluefunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);

      lua_getglobal(LuaVM, pchar(lua_valuetobytes));
      lua_valuetobytesfunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);

      lua_pop(LuaVM,lua_getTop(luavm));

    end;

  except
    on e: exception do
    begin
      //restore the old state if there is any
      fname:=oldname;
      ffunctiontypename:=oldfunctiontypename;
      routine:=oldroutine;
      reverseroutine:=oldreverseroutine;
      bytesize:=oldbytesize;
      preferedAlignment:=oldpreferedalignment;
      fScriptUsesFloat:=oldScriptUsesFloat;

      fScriptUsesCDecl:=oldScriptUsesCDecl;

      setlength(c,length(oldallocarray));
      for i:=0 to length(oldallocarray)-1 do
        c[i]:=oldallocarray[i];

      raise exception.create(e.Message); //and now raise the error
    end;
  end;
  {$ENDIF}
end;


constructor TCustomType.CreateTypeFromLuaScript(script: string);
begin
  inherited create;
  lua_bytestovaluefunctionid:=-1;
  lua_valuetobytesfunctionid:=-1;

  setScript(script,true);

  //still here so everything ok
  customtypes.Add(self);
  MaxCustomTypeSize:=max(MaxCustomTypeSize, bytesize);
end;


constructor TCustomType.CreateTypeFromAutoAssemblerScript(script: string);
begin
  inherited create;
  lua_bytestovaluefunctionid:=-1;
  lua_valuetobytesfunctionid:=-1;

  setScript(script);

  //still here so everything ok
  customtypes.Add(self);

  MaxCustomTypeSize:=max(MaxCustomTypeSize, bytesize);
end;

procedure TCustomType.remove;
var i: integer;
begin
  unloadscript;

  //remove self from array
  i:=customTypes.IndexOf(self);
  if i<>-1 then
    customTypes.Delete(i);



  //get a new max
  MaxCustomTypeSize:=0;
  for i:=0 to customTypes.count-1 do
    MaxCustomTypeSize:=max(MaxCustomTypeSize, TCustomType(customTypes[i]).bytesize);

  mainform.RefreshCustomTypes;
end;

procedure TCustomType.showDebugInfo;
var x,y: pointer;
begin

  {$IFNDEF jni}
  x:=@routine;
  y:=@reverseroutine;
  ShowMessage(format('routine=%p reverseroutine=%p',[x, y]));
  {$ENDIF}
end;

destructor TCustomType.destroy;
begin
  remove;
  inherited destroy;
end;

//lua
function registerCustomTypeLua(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  typename: string;
  bytecount: integer;
  f_bytestovalue: integer;
  bytestovalue: string;
  f_valuetobytes: integer;
  valuetobytes: string;

  isfloat: boolean;

  ct: TCustomType;
begin
  {$IFNDEF jni}
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=4 then
  begin
    typename:=Lua_ToString(L, 1);
    bytecount:=lua_tointeger(L, 2);

    f_bytestovalue:=0;
    f_valuetobytes:=0;

    if lua_isfunction(L, 3) then
    begin
      lua_pushvalue(L, 3);
      f_bytestovalue:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    if lua_isstring(L,3) then
    begin
      bytestovalue:=Lua_ToString(L, 3);
      lua_getglobal(L, pchar(bytestovalue));
      f_valuetobytes:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    begin
      lua_pop(L, lua_gettop(L));
      lua_pushstring(L,rsCTHParameter3IsNotAValidFunction);
      lua_error(L);
      exit;
    end;

    if lua_isfunction(L, 4) then
    begin
      lua_pushvalue(L, 4);
      f_valuetobytes:=luaL_ref(L,LUA_REGISTRYINDEX);

      //f_bytestovalue:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    if lua_isstring(L,4) then
    begin
      valuetobytes:=Lua_ToString(L, 4);
      lua_getglobal(LuaVM, pchar(valuetobytes));
      f_valuetobytes:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    begin
      lua_pop(L, parameters);
      lua_pushstring(L,rsCTHParameter4IsNotAValidFunction);
      lua_error(L);
      exit;
    end;

    if parameters>=5 then
      isfloat:=lua_toboolean(L,5)
    else
      isFloat:=false;

    lua_pop(L, parameters);

    ct:=GetCustomTypeFromName(typename); //see if one with this name altready exists.
    if ct=nil then //if not, create it
      ct:=TCustomType.Create;

    ct.fCustomTypeType:=cttLuaScript;
    ct.lua_bytestovaluefunctionid:=f_bytestovalue;
    ct.lua_valuetobytesfunctionid:=f_valuetobytes;
    ct.name:=typename;
    ct.bytesize:=bytecount;
    ct.scriptUsesFloat:=isfloat;


    customtypes.Add(ct);
    mainform.RefreshCustomTypes;

    luaclass_newClass(L, ct);
    result:=1;
  end
  else lua_pop(L, parameters);
  {$ENDIF}
end;

function registerCustomTypeAutoAssembler(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  typename: string='';
  bytecount: integer;
  script: string;
  ct: TCustomType;

  s: TStringList;
  i: integer;
begin
  {$IFNDEF jni}
  result:=0;
  bytecount:=1;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    typename:=Lua_ToString(L, 1);
    bytecount:=lua_tointeger(L, 2);
    script:=Lua_ToString(L, 3);
  end
  else
  if parameters=1 then
    script:=Lua_ToString(L, 1)
  else
  begin
    lua_pop(L, parameters);
    lua_pushstring(L,rsCTHInvalidNumberOfParameters);
    lua_error(L);
    exit;
  end;

  lua_pop(L, parameters);

  ct:=TCustomType.CreateTypeFromAutoAssemblerScript(script);
  if parameters=3 then //old version support
  begin
    ct.name:=typename;
    ct.bytesize:=bytecount;
  end;

  mainform.RefreshCustomTypes;
  luaclass_newClass(L, ct);
  result:=1;

  {$ENDIF}

end;


initialization
  customTypes:=Tlist.create;

finalization
  if customTypes<>nil then
    customtypes.free;

end.

