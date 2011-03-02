unit CustomTypeHandler;

{$mode delphi}
{
This class is used as a wrapper for different kinds of custom types
}

interface

uses
  Classes, SysUtils,cefuncproc, autoassembler, lua, lauxlib, lualib, luahandler;

type TConversionRoutine=function(data: pointer):integer; stdcall;
type TReverseConversionRoutine=procedure(i: integer; output: pointer); stdcall;


type
  TCustomTypeType=(cttAutoAssembler, cttLuaScript, cttPlugin);
  TCustomType=class
  private
    fname: string;
    ffunctiontypename: string; //lua

    lua_bytestovaluefunctionid: integer;
    lua_bytestovalue: string; //help string that contains the functionname so it doesn't have to build up this string at runtime
    lua_valuetobytes: string;


    routine: TConversionRoutine;
    reverseroutine: TReverseConversionRoutine;

    c: TCEAllocArray;
    currentscript: tstringlist;
    fCustomTypeType: TCustomTypeType; //plugins set this to cttPlugin

    procedure unloadscript;
    procedure setName(n: string);
    procedure setfunctiontypename(n: string);
  public

    bytesize: integer;

    function ConvertDataToInteger(data: pointer): integer;
    function ConvertDataToIntegerLua(data: pbytearray): integer;
    procedure ConvertIntegerToData(i: integer; output: pointer);
    procedure ConvertIntegerToDataLua(i: integer; output: pbytearray);


    function getScript:string;
    procedure setScript(script:string; luascript: boolean=false);
    constructor CreateTypeFromAutoAssemblerScript(script: string);
    constructor CreateTypeFromLuaScript(script: string);
    destructor destroy; override;

    procedure remove;  //call this instead of destroy

    property name: string read fName write setName;
    property functiontypename: string read ffunctiontypename write setfunctiontypename; //lua
    property CustomTypeType: TCustomTypeType read fCustomTypeType;
    property script: string read getScript write setScript;
end;

function GetCustomTypeFromName(name:string):TCustomType; //global function to retrieve a custom type

var customTypes: TList; //list holding all the custom types

implementation

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
    lua_getfield(L, LUA_GLOBALSINDEX, pchar(lua_valuetobytes));
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
begin
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
    //lua_getfield(L, LUA_GLOBALSINDEX, pchar(lua_bytestovalue));
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, lua_bytestovaluefunctionid);


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
          //find alloc "ConvertRoutine"
          for i:=0 to length(c)-1 do
          begin
            if uppercase(c[i].varname)='TYPENAME' then
              name:=pchar(c[i].address);

            if uppercase(c[i].varname)='CONVERTROUTINE' then
              newroutine:=pointer(c[i].address);

            if uppercase(c[i].varname)='BYTESIZE' then
              newbytesize:=pinteger(c[i].address)^;

            if uppercase(c[i].varname)='CONVERTBACKROUTINE' then
              newreverseroutine:=pointer(c[i].address);
          end;

          //still here
          unloadscript; //unmload the old script

          //and now set the new values
          bytesize:=newbytesize;
          routine:=newroutine;
          reverseroutine:=newreverseroutine;

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

  setScript(script,true);

  //still here so everything ok
  customtypes.Add(self);
end;


constructor TCustomType.CreateTypeFromAutoAssemblerScript(script: string);
begin
  inherited create;

  setScript(script);

  //still here so everything ok
  customtypes.Add(self);
end;

procedure TCustomType.remove;
var i: integer;
begin
  unloadscript;

  //remove self from array
  i:=customTypes.IndexOf(self);
  if i<>-1 then
    customTypes.Delete(i);

end;

destructor TCustomType.destroy;
begin
  remove;
end;

initialization
  customTypes:=Tlist.create;

finalization
  if customTypes<>nil then
    customtypes.free;

end.

