unit CustomTypeHandler;

{$mode delphi}
{
This class is used as a wrapper for different kinds of custom types
}

interface

uses
  {windows, }dialogs, Classes, SysUtils,cefuncproc, autoassembler, lua, lauxlib,
  lualib, math;

type TConversionRoutine=function(data: pointer):integer; stdcall;
type TReverseConversionRoutine=procedure(i: integer; output: pointer); stdcall;


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


    routine: TConversionRoutine;
    reverseroutine: TReverseConversionRoutine;

    c: TCEAllocArray;
    currentscript: tstringlist;
    fCustomTypeType: TCustomTypeType; //plugins set this to cttPlugin
    fScriptUsesFloat: boolean;



    procedure unloadscript;
    procedure setName(n: string);
    procedure setfunctiontypename(n: string);
  public

    bytesize: integer;
    preferedAlignment: integer;

    //these 4 functions are just to make it easier
    procedure ConvertToData(f: single; output: pointer); overload;
    function ConvertFromData(data: pointer): single; overload;
    procedure ConvertToData(i: integer; output: pointer); overload;
    function ConvertFromData(data: pointer): integer; overload;

    function ConvertDataToInteger(data: pointer): integer;
    function ConvertDataToIntegerLua(data: pbytearray): integer;
    procedure ConvertIntegerToData(i: integer; output: pointer);
    procedure ConvertIntegerToDataLua(i: integer; output: pbytearray);

    function ConvertDataToFloat(data: pointer): single;
    function ConvertDataToFloatLua(data: pbytearray): single;
    procedure ConvertFloatToData(f: single; output: pointer);
    procedure ConvertFloatToDataLua(f: single; output: pbytearray);



    function getScript:string;
    procedure setScript(script:string; luascript: boolean=false);
    constructor CreateTypeFromAutoAssemblerScript(script: string);
    constructor CreateTypeFromLuaScript(script: string);
    destructor destroy; override;

    procedure remove;  //call this instead of destroy
    procedure showDebugInfo;

    property name: string read fName write setName;
    property functiontypename: string read ffunctiontypename write setfunctiontypename; //lua
    property CustomTypeType: TCustomTypeType read fCustomTypeType;
    property script: string read getScript write setScript;
    property scriptUsesFloat: boolean read fScriptUsesFloat;
  end;
  PCustomType=^TCustomType;

function GetCustomTypeFromName(name:string):TCustomType; //global function to retrieve a custom type

function registerCustomTypeLua(L: PLua_State): integer; cdecl;
function registerCustomTypeAutoAssembler(L: PLua_State): integer; cdecl;

var customTypes: TList; //list holding all the custom types
    AllIncludesCustomType: boolean;
    MaxCustomTypeSize: integer;

implementation

uses mainunit, LuaHandler;

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
        raise exception.create(Format(
          rsACustomFunctionTypeWithNameAlreadyExists, [n]));
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

procedure TCustomType.ConvertIntegerToDataLua(i: integer; output: pbytearray);
var
  L: PLua_State;
  r: integer;
  c,b: integer;
begin
  l:=LuaVM;

  LuaCS.Enter;
  try
    if lua_valuetobytesfunctionid=-1 then
    begin
      lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(lua_valuetobytes));
      lua_valuetobytesfunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);
    end;
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, lua_valuetobytesfunctionid);

    lua_pushinteger(L, i);
    if lua_pcall(l,1,bytesize,0)=0 then
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


  finally
    LuaCS.Leave;
  end;
end;

procedure TCustomType.ConvertIntegerToData(i: integer; output: pointer);
var f: single;
begin

  if scriptUsesFloat then //convert to a float and pass that
  begin
    f:=i;
    i:=pdword(@f)^;
  end;

  if assigned(reverseroutine) then
    reverseroutine(i,output)
  else
  begin
    //possible lua
    if fCustomTypeType=cttLuaScript then
      ConvertIntegerToDataLua(i, output);
  end;
end;

function TCustomType.ConvertDataToIntegerLua(data: pbytearray): integer; //split up for speed
var
  L: PLua_State;
  i: integer;
begin
  l:=LuaVM;

  LuaCS.Enter;
  try
    result:=0;

    if lua_bytestovaluefunctionid=-1 then
    begin
      lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(lua_bytestovalue));
      lua_bytestovaluefunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);
    end;


   // messagebox(0,'going to call rawgeti','bla',0);
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, lua_bytestovaluefunctionid);


   // messagebox(0,'after call rawgeti','bla',0);


    for i:=0 to bytesize-1 do
      lua_pushinteger(L,data[i]);

    lua_call(L, bytesize,1);
    result:=lua_tointeger(L, -1);

    lua_pop(L,lua_gettop(l));
  finally
    LuaCS.Leave;
  end;

end;



function TCustomType.ConvertDataToInteger(data: pointer): integer;
var
  i: dword;
  f: single absolute i;
begin
  if assigned(routine) then
    result:=routine(data)
  else
  begin
    //possible lua
    if fCustomTypeType=cttLuaScript then
      result:=ConvertDataToIntegerLua(data)
    else
      result:=0;
  end;

  if fScriptUsesFloat then //the result is still in float state
  begin
    i:=result;
    result:=trunc(f);
  end;
end;


procedure TCustomType.ConvertFloatToDataLua(f: single; output: pbytearray);
//I REALLY doubt anyone in their right mind would use lua to encode a float as bytes, but it's here...
var
  L: PLua_State;
  r: integer;
  c,b: integer;
begin
  l:=LuaVM;

  LuaCS.Enter;
  try
    if lua_valuetobytesfunctionid=-1 then
    begin
      lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(lua_valuetobytes));
      lua_valuetobytesfunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);
    end;
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, lua_valuetobytesfunctionid);

    lua_pushnumber(L, f);
    if lua_pcall(l,1,bytesize,0)=0 then
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


  finally
    LuaCS.Leave;
  end;
end;


procedure TCustomType.ConvertFloatToData(f: single; output: pointer);
var i: integer;
begin

  i:=pdword(@f)^; //convert the f to a integer without conversion (reverseroutine takes an integer, but could be any 32-bit value really)

  if not scriptUsesFloat then //WHY even call this ?
    i:=trunc(f);

  if assigned(reverseroutine) then
    reverseroutine(i,output)
  else
  begin
    //possible lua
    if fCustomTypeType=cttLuaScript then
      ConvertFloatToDataLua(i, output);
  end;
end;

function TCustomType.ConvertDataToFloatLua(data: PByteArray): single;
//again, why would anyone use lua for this ?
var
  L: PLua_State;
  i: integer;
begin
  l:=LuaVM;

  LuaCS.Enter;
  try
    if lua_bytestovaluefunctionid=-1 then
    begin
      lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(lua_bytestovalue));
      lua_bytestovaluefunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);
    end;


    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, lua_bytestovaluefunctionid);



    for i:=0 to bytesize-1 do
      lua_pushinteger(L,data[i]);

    lua_call(L, bytesize,1);
    result:=lua_tonumber(L, -1);

    lua_pop(L,lua_gettop(l));
  finally
    LuaCS.Leave;
  end;

end;



function TCustomType.ConvertDataToFloat(data: pointer): single;
var
  i: dword;
  f: single absolute i;
begin
  if assigned(routine) then
  begin
    i:=routine(data);

    if not fScriptUsesFloat then //the result is in integer format ,
      f:=i; //convert the integer to float
  end
  else
  begin
    //possible lua
    if fCustomTypeType=cttLuaScript then
      f:=ConvertDataToFloatLua(data)
    else
      f:=0;
  end;

  result:=f;
end;

procedure TCustomType.ConvertToData(f: single; output: pointer);
begin
  ConvertFloatToData(f, output);
end;

function TCustomType.ConvertFromData(data: pointer): single;
begin
  result:=ConvertDataToFloat(data);
end;

procedure TCustomType.ConvertToData(i: integer; output: pointer);
begin
  ConvertIntegerToData(i, output);
end;

function TCustomType.ConvertFromData(data: pointer): integer;
begin
  result:=ConvertDataToInteger(data);
end;

procedure TCustomType.unloadscript;
begin
  if fCustomTypeType=cttAutoAssembler then
  begin
    routine:=nil;
    reverseroutine:=nil;

    if currentscript<>nil then
    begin
      autoassemble(currentscript,false, false, false, true, c); //popupmessages is false so it won't complain if there is no disable section
      freeandnil(currentscript);
    end;
  end;
end;

procedure TCustomType.setScript(script:string; luascript: boolean=false);
var i: integer;
  s: tstringlist;
  error:pchar;

  //lua vars
  returncount: integer;

  templua: Plua_State;

  ftn,tn: pchar;

  oldname: string;
  oldfunctiontypename: string;
  newpreferedalignment, oldpreferedalignment: integer;
  oldScriptUsesFloat, newScriptUsesFloat: boolean;
  newroutine, oldroutine: TConversionRoutine;
  newreverseroutine, oldreverseroutine: TReverseConversionRoutine;
  newbytesize, oldbytesize: integer;
  oldallocarray: TCEAllocArray;
begin
  oldname:=fname;
  oldfunctiontypename:=ffunctiontypename;
  oldroutine:=routine;
  oldreverseroutine:=reverseroutine;
  oldbytesize:=bytesize;
  oldpreferedalignment:=preferedalignment;
  oldScriptUsesFloat:=fScriptUsesFloat;

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

        if autoassemble(s,false, true, false, true, c) then
        begin
          newpreferedalignment:=-1;
          newScriptUsesFloat:=false;

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
      //create a new lua state and load this script
      templua:=luaL_newstate;
      if templua=nil then
        raise exception.create(rsFailureCreatingLuaObject);

      try
        if lua_dostring(templua, pchar(script))=0 then //success
        begin
          returncount:=lua_gettop(templua);
          if returncount<>3 then
            raise exception.create(
              rsOnlyReturnTypenameBytecountAndFunctiontypename);

          //-1=functiontypename
          //-2=bytecount
          //-3=typename
          ftn:=lua.lua_tostring(templua,-1);
          bytesize:=lua_tointeger(templua,-2);
          tn:=lua.lua_tostring(templua,-3);

          if bytesize=0 then raise exception.create(rsBytesizeIs0);
          if ftn=nil then raise exception.create(rsInvalidFunctiontypename);
          if tn=nil then raise exception.create(rsInvalidTypename);

          name:=tn;
          functiontypename:=ftn;

        end
        else
        begin
          //something went wrong
          if lua_gettop(templua)>0 then
          begin
            error:=lua.lua_tostring(templua,-1);
            raise exception.create(error);
          end else raise exception.create(rsUndefinedError);
        end;

      finally
        lua_close(templua);
      end;
      //still here so the script got loaded and passed the tests

      //now load the script into the actual vm
      if lua_dostring(LuaVM, pchar(script))<>0 then
      begin
        if lua_gettop(LuaVM)>0 then
        begin
          error:=lua.lua_tostring(LuaVM,-1);
          raise exception.create(error);
        end else raise exception.create(rsUndefinedError);
      end else lua_pop(LuaVM,3);


      fCustomTypeType:=cttLuaScript;
      if currentscript=nil then
        currentscript:=tstringlist.create;

      currentscript.text:=script;


      lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(lua_bytestovalue));
      lua_bytestovaluefunctionid:=luaL_ref(LuaVM,LUA_REGISTRYINDEX);

      lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(lua_valuetobytes));
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

      setlength(c,length(oldallocarray));
      for i:=0 to length(oldallocarray)-1 do
        c[i]:=oldallocarray[i];

      raise exception.create(e.Message); //and now raise the error
    end;
  end;
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
end;

procedure TCustomType.showDebugInfo;
var x,y: pointer;
begin
  x:=@routine;
  y:=@reverseroutine;
  ShowMessage(format('routine=%p reverseroutine=%p',[x, y]));
end;

destructor TCustomType.destroy;
begin
  remove;
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

  ct: TCustomType;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=4 then
  begin
    typename:=Lua_ToString(L, -4);
    bytecount:=lua_tointeger(L, -3);

    if lua_isfunction(L, -2) then
    begin
      lua_pushvalue(L, -2);
      f_bytestovalue:=luaL_ref(L,LUA_REGISTRYINDEX);

      //f_bytestovalue:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    if lua_isstring(L,-2) then
    begin
      bytestovalue:=Lua_ToString(L, -2);
      lua_getfield(L, LUA_GLOBALSINDEX, pchar(bytestovalue));
      f_valuetobytes:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    begin
      lua_pop(L, lua_gettop(L));
      lua_pushstring(L,'Parameter 3 is not a valid function');
      lua_error(L);
      exit;
    end;

    if lua_isfunction(L, -1) then
    begin
      lua_pushvalue(L, -1);
      f_valuetobytes:=luaL_ref(L,LUA_REGISTRYINDEX);

      //f_bytestovalue:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    if lua_isstring(L,-1) then
    begin
      valuetobytes:=Lua_ToString(L, -1);
      lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(valuetobytes));
      f_valuetobytes:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    begin
      lua_pop(L, parameters);
      lua_pushstring(L,'Parameter 4 is not a valid function');
      lua_error(L);
      exit;
    end;

    lua_pop(L, parameters);

    ct:=GetCustomTypeFromName(typename); //see if one with this name altready exists.
    if ct=nil then //if not, create it
      ct:=TCustomType.Create;

    ct.fCustomTypeType:=cttLuaScript;
    ct.lua_bytestovaluefunctionid:=f_bytestovalue;
    ct.lua_valuetobytesfunctionid:=f_valuetobytes;
    ct.name:=typename;
    ct.bytesize:=bytecount;

    customtypes.Add(ct);
    mainform.RefreshCustomTypes;
  end
  else lua_pop(L, parameters);
end;

function registerCustomTypeAutoAssembler(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  typename: string;
  bytecount: integer;
  script: string;
  ct: TCustomType;

  s: TStringList;
  i: integer;
begin
  result:=0;
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
    lua_pushstring(L,'Invalid number of parameters');
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

end;


initialization
  customTypes:=Tlist.create;

finalization
  if customTypes<>nil then
    customtypes.free;

end.

