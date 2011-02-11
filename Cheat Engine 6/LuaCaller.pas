unit LuaCaller;
{
The luaCaller is a class which contains often defined Events and provides an
interface for gui objects to directly call the lua functions with proper parameters
and results
}

{$mode delphi}

interface

uses
  Classes, SysUtils, ceguicomponents, forms, lua, lualib, lauxlib;

type
  TLuaCaller=class
    private
      function canRun: boolean;
      procedure pushFunction;
    public
      luaroutine: string;
      luaroutineindex: integer;
      owner: TPersistent;
      procedure NotifyEvent(sender: TObject);
      procedure CloseEvent(Sender: TObject; var CloseAction: TCloseAction);
      constructor create;
  end;

implementation

uses luahandler;

constructor TLuaCaller.create;
begin
  luaroutineindex:=-1;
end;

function TLuaCaller.canRun: boolean;
var baseOwner: TComponent;
begin
  baseOwner:=Tcomponent(owner);
  if baseOwner<>nil then
  begin
    while (not (baseOwner is TCustomForm)) and (baseowner.Owner<>nil) do //as long as the current base is not a form and it still has a owner
      baseOwner:=baseowner.owner;
  end;

  result:=(baseowner=nil) or (not ((baseOwner is TCEform) and (TCEForm(baseowner).designsurface<>nil) and (TCEForm(baseowner).designsurface.active)));
end;

procedure TLuaCaller.pushFunction;
begin
  if luaroutineindex=-1 then //get the index of the given routine
    lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(luaroutine))
  else
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, luaroutineindex)
end;

procedure TLuaCaller.NotifyEvent(sender: TObject);
begin
  Luacs.Enter;
  try

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);

      lua_pcall(Luavm, 1,0,0); //procedure(sender)
      lua_pop(Luavm, lua_gettop(Luavm));
    end;
  finally
    luacs.leave;
  end;
end;

procedure TLuaCaller.CloseEvent(Sender: TObject; var CloseAction: TCloseAction);
begin
  Luacs.Enter;
  try

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);


      lua_pcall(Luavm, 1,1,0); //procedure(sender)

      if lua_gettop(Luavm)>0 then
        CloseAction:=TCloseAction(lua_tointeger(LuaVM,-1));

      lua_pop(Luavm, lua_gettop(Luavm));
    end;
  finally
    luacs.leave;
  end;
end;

end.

