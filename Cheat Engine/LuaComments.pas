unit LuaComments;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure initializeLuaComments;

implementation
uses  lua, lauxlib, lualib, luahandler, luaobject, luaclass, pluginexports, disassemblerComments,AvgLvlTree;
type
  PCommentData=^TCommentData;
  TCommentData=record
    address: ptruint;
    interpretableAddress: pchar;
    comment: pchar;
    header: pchar;
    previous: PCommentData;
    next: PCommentData;
  end;



function comment_list(L: Plua_State): integer; cdecl;
var
 n: TAvgLvlTreeNode;
 c: PCommentData;
begin
  n:= dassemblercomments.commentstree.FindLowest;
  lua_createtable(L, 100,0);
  if n<>nil then
  begin
       c:=n.data;
       while c<>nil do
       begin
         if (c.comment<>nil) and (Trim(c.comment) <>'') then
           begin
                lua_pushinteger(L, c.address);    //push the index on the stack
                lua_pushstring(L,c.comment);      //push the value on the stack
                lua_settable(L, -3);              //make the entry
           end;
       c:=c.next;
      end;

  end;


  result := 1;

end;



procedure comments_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

end;

procedure initializeLuaComments;
begin
  lua_register(LuaVM, 'getAllComments', comment_list);
end;

initialization
  luaclass_register(TDisassemblerComments, comments_addMetaData);


end.

