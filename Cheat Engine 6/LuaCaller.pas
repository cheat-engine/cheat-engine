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
      function ActivateEvent(sender: TObject; before, currentstate: boolean): boolean;
      constructor create;
      destructor destroy; override;
  end;

procedure CleanupLuaCall(event: TMethod);   //cleans up a luacaller class if it was assigned if it was set

implementation

uses luahandler, MainUnit;

procedure CleanupLuaCall(event: TMethod);
begin
  if (event.code<>nil) and (event.data<>nil) and (TObject(event.data) is TLuaCaller) then
    TLuaCaller(event.data).free;

end;

constructor TLuaCaller.create;
begin
  luaroutineindex:=-1;
end;

destructor TLuaCaller.destroy;
begin
  if luaroutineindex<>-1 then //deref
    luaL_unref(luavm, LUA_REGISTRYINDEX, luaroutineindex);
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


      if lua_pcall(Luavm, 1,1,0)=0 then //procedure(sender)
      begin
        if lua_gettop(Luavm)>0 then
          CloseAction:=TCloseAction(lua_tointeger(LuaVM,-1));
      end
      else
        closeAction:=caHide; //not implemented by the user

      if mainform.mustclose then
        closeaction:=cahide;

      lua_pop(Luavm, lua_gettop(Luavm));
    end
    else closeaction:=caHide;
  finally
    luacs.leave;
  end;
end;

function TLuaCaller.ActivateEvent(sender: tobject; before, currentstate: boolean): boolean;
begin
  result:=true;
  Luacs.Enter;
  try

    if canRun then
    begin
      PushFunction;
      lua_pushlightuserdata(Luavm, sender);
      lua_pushboolean(luavm, before);
      lua_pushboolean(luavm, currentstate);


      lua_pcall(Luavm, 3,1,0); //function(sender, before, currentstate):boolean

      if lua_gettop(Luavm)>0 then
        result:=lua_toboolean(LuaVM,-1);

      lua_pop(Luavm, lua_gettop(Luavm));
    end;
  finally
    luacs.leave;
  end;
end;

end.

