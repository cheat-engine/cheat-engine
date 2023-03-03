unit LuaClass;

//Handles some of the common used class code


{$mode delphi}

interface

uses
  Classes, SysUtils, controls, lua, lauxlib, lualib, math, AvgLvlTree, syncobjs;

type TAddMetaDataFunction=procedure(L: PLua_state; metatable: integer; userdata: integer );

type
  TRecordEntry=record
    name: string;
    getf: lua_CFunction;
    setf: lua_CFunction;
  end;

  TRecordEntries=class
  private
    list: array of TRecordEntry;
    function getEntry(index: integer): TRecordEntry;
    procedure setEntry(index: integer; e: TRecordEntry);
    function getCount: integer;
  public
    procedure add(r: TRecordEntry);
    procedure clear;
    property Items[Index: Integer]: TRecordEntry read getEntry write setEntry; default;
    property Count: integer read getCount;
  end;

function luaclass_createMetaTable(L: Plua_State;garbagecollectable: boolean=false): integer;

procedure luaclass_addClassFunctionToTable(L: PLua_State; metatable: integer; userdata: integer; functionname: string; f: lua_CFunction);
procedure luaclass_addPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; getfunction: lua_CFunction; setfunction: lua_CFunction);
procedure luaclass_setDefaultArrayProperty(L: PLua_State; metatable: integer; userdata: integer; getf,setf: lua_CFunction);
procedure luaclass_setDefaultStringArrayProperty(L: PLua_State; metatable: integer; userdata: integer; getf,setf: lua_CFunction);


procedure luaclass_addArrayPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; getf: lua_CFunction; setf: lua_CFunction=nil);
procedure luaclass_addRecordPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; RecordEntries: TRecordEntries);

procedure luaclass_setAutoDestroy(L: PLua_State; metatable: integer; state: boolean);


function luaclass_getClassObject(L: PLua_state; paramstart: pinteger=nil; paramcount: pinteger=nil): pointer; //inline;

procedure luaclass_newClass(L: PLua_State; o: TObject; garbagecollectable: boolean=false); overload;
procedure luaclass_newClass(L: PLua_State; o: pointer; InitialAddMetaDataFunction: TAddMetaDataFunction; garbagecollectable: boolean=false); overload;
procedure luaclass_newClass(L: PLua_State; o: TObject; InitialAddMetaDataFunction: TAddMetaDataFunction; garbagecollectable: boolean=false); overload;
procedure luaclass_newClassFunction(L: PLua_State; InitialAddMetaDataFunction: TAddMetaDataFunction; garbagecollectable: boolean=false);



procedure luaclass_register(c: TClass; InitialAddMetaDataFunction: TAddMetaDataFunction);

procedure luaclass_pushClass(L: PLua_State; o: TObject); stdcall; //for plugins

implementation

uses LuaClassArray, LuaObject, LuaComponent, luahandler;

var lookuphelp: TPointerToPointerTree; //does not update after initialization (static)
    lookuphelp2: TPointerToPointerTree; //for classes that inherit from the main classes (can update after initialization, dynamic)
    lookuphelp2MREW: TMultiReadExclusiveWriteSynchronizer;
    objectcomparefunctionref: integer=0;

type
  TClasslistentry=record
    c: TClass;
    f: TAddMetaDataFunction;
  end;
  PClassListEntry=^TClassListEntry;

resourcestring
  rsInvalidClassObject='Invalid class object';


function TRecordEntries.getEntry(index: integer): TRecordEntry;
begin
  if index<length(list) then
    result:=list[index]
  else
  begin
    result.name:='';
    result.getf:=nil;
    result.setf:=nil;
  end;
end;

procedure TRecordEntries.setEntry(index: integer; e: TRecordEntry);
begin
  if index<length(list) then
    list[index]:=e;
end;

procedure TRecordEntries.add(r: TRecordEntry);
begin
  setlength(list, length(list)+1);
  list[length(list)-1]:=r;
end;

procedure TRecordEntries.clear;
begin
  setlength(list,0);
end;

function TRecordEntries.getCount: integer;
begin
  result:=length(list);
end;

procedure luaclass_register(c: TClass; InitialAddMetaDataFunction: TAddMetaDataFunction);
//registers the classes that are accessible by lua. Used by findBestClassForObject
var cle: PClasslistentry;
    t: TClass;
begin
  if lookuphelp=nil then
  begin
    lookuphelp:=TPointerToPointerTree.Create;
    lookuphelp2:=TPointerToPointerTree.create;
    lookuphelp2MREW:=TMultiReadExclusiveWriteSynchronizer.Create;
  end;

  getmem(cle, sizeof(TClasslistentry));

  cle.c:=c;
  cle.f:=InitialAddMetaDataFunction;

  lookuphelp.Values[c]:=cle;
end;

function findBestClassForObject(O: TObject): TAddMetaDataFunction;
var
  best: PClassListEntry;
  oclass: Tclass;
begin
  result:=nil;
  if o=nil then exit;

  oclass:=o.ClassType;

  best:=lookuphelp.Values[oclass];
  if best<>nil then exit(best^.f);

  //not a main type, check the child types
  lookuphelp2MREW.beginread;
  best:=lookuphelp2.values[oclass];
  lookuphelp2MREW.endread;
  if best<>nil then exit(best^.f);

  //find it in the static typestore
  oclass:=oclass.ClassParent;
  while oclass<>nil do
  begin
    best:=lookuphelp.Values[oclass];
    if best<>nil then
    begin
      //add to the secondary list
      lookuphelp2MREW.Beginwrite;
      lookuphelp2.values[o.classtype]:=best;
      lookuphelp2MREW.endwrite;
      exit(best^.f);
    end;
    oclass:=oclass.ClassParent;
  end;
end;

procedure luaclass_newClassFunction(L: PLua_State; InitialAddMetaDataFunction: TAddMetaDataFunction; garbagecollectable: boolean=false);
//converts the item at the top of the stack to a class object
var userdata, metatable: integer;
begin

  if Assigned(InitialAddMetaDataFunction) then
  begin
    userdata:=lua_gettop(L);

    metatable:=luaclass_createMetaTable(L, garbagecollectable);
    InitialAddMetaDataFunction(L, metatable, userdata);

    lua_setmetatable(L, userdata);
  end;
end;


procedure luaclass_newClass(L: PLua_State; o: pointer; InitialAddMetaDataFunction: TAddMetaDataFunction; garbagecollectable: boolean=false);
begin
  if (o<>nil) and (Assigned(InitialAddMetaDataFunction)) then
  begin
    lua_newuserdata(L, o);
    luaclass_newClassFunction(L, InitialAddMetaDataFunction);
  end
  else
    lua_pushnil(L);
end;

procedure luaclass_newClass(L: PLua_State; o: TObject; InitialAddMetaDataFunction: TAddMetaDataFunction; garbagecollectable: boolean=false);
begin
  luaclass_newClass(L, pointer(o), InitialAddMetaDataFunction, garbagecollectable);
end;




procedure luaclass_newClass(L: PLua_State; o: TObject; garbagecollectable: boolean=false); overload;
var InitialAddMetaDataFunction: TAddMetaDataFunction;
begin
  if o<>nil then
  begin
    InitialAddMetaDataFunction:=findBestClassForObject(o);
    luaclass_newClass(L, o, InitialAddMetaDataFunction, garbagecollectable);
  end
  else
    lua_pushnil(L);
end;

procedure luaclass_pushClass(L: PLua_State; o: TObject); stdcall; //for plugins
begin
  luaclass_newClass(L,o);
end;

function luaclass_getClassObject(L: PLua_state; paramstart: pinteger=nil; paramcount: pinteger=nil): pointer;// inline;
//called as first thing by class functions. This is in case a 6.2 code executed the function manually
var t: integer;
    u: pointer;
begin
  result:=nil;
  if lua_type(L, lua_upvalueindex(1))=LUA_TUSERDATA then
  begin
    u:=lua_touserdata(L, lua_upvalueindex(1));
    result:=ppointer(u)^;
    if assigned(paramstart) then
      paramstart^:=1;

    if assigned(paramcount) then
      paramcount^:=lua_gettop(L);
  end
  else
  if lua_gettop(L)>=1 then
  begin
    t:=lua_type(L, 1);
    if t in [LUA_TUSERDATA, LUA_TLIGHTUSERDATA] then
    begin
      u:=lua_touserdata(L, 1);
      if t=LUA_TUSERDATA then
        result:=ppointer(u)^
      else
        result:=u;

      if assigned(paramstart) then
        paramstart^:=2;

      if assigned(paramcount) then
        paramcount^:=lua_gettop(L)-1;
    end;
  end;

  if result=nil then
    raise exception.create(rsInvalidClassObject);
{  begin
    lua_pushstring(L, rsInvalidClassObject);
    lua_error(L);
  end;
  }
end;

procedure luaclass_setDefaultStringArrayProperty(L: PLua_State; metatable: integer; userdata: integer; getf, setf: lua_CFunction);
//this makes it so x[0], x[1], x[2],...,x.0 , x.1 , x.2,... will call these specific get/set handlers
begin
  lua_pushstring(L, '__defaultstringgetindexhandler');
  if assigned(getf) then
  begin
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, getf, 1);
  end
  else
    lua_pushnil(L);

  lua_settable(L, metatable);

  lua_pushstring(L, '__defaultstringsetindexhandler');
  if assigned(setf) then
  begin
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, setf, 1);
  end
  else
    lua_pushnil(L);

  lua_settable(L, metatable);

end;

procedure luaclass_setDefaultArrayProperty(L: PLua_State; metatable: integer; userdata: integer; getf, setf: lua_CFunction);
//this makes it so x[0], x[1], x[2],...,x.0 , x.1 , x.2,... will call these specific get/set handlers
begin
  lua_pushstring(L, '__defaultintegergetindexhandler');
  if assigned(getf) then
  begin
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, getf, 1);
  end
  else
    lua_pushnil(L);

  lua_settable(L, metatable);

  lua_pushstring(L, '__defaultintegersetindexhandler');
  if assigned(setf) then
  begin
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, setf, 1);
  end
  else
    lua_pushnil(L);

  lua_settable(L, metatable);

end;

procedure luaclass_addRecordPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; RecordEntries: TRecordEntries);
var t,metatable2: integer;
    i: integer;
begin
  lua_pushstring(L, propertyname);
  lua_newtable(L);
  t:=lua_gettop(L);

  metatable2:=luaclass_createMetaTable(L); //create a luaclass metatable for this new table

  lua_pushstring(L, '__norealclass');
  lua_pushboolean(L, true);
  lua_settable(L, metatable2); //tell this metatable that it's not a "real" class, so it won't have to get properties or component names


  for i:=0 to RecordEntries.count-1 do
    luaclass_addPropertyToTable(L, metatable2, userdata, RecordEntries[i].name, RecordEntries[i].getf, RecordEntries[i].setf);

  lua_setmetatable(L, t);  //pop the table from the stack and set it as metatatble to table T


  lua_settable(L, metatable); //pop the table and the string from the table and set that to the metatable
end;

procedure luaclass_addArrayPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; getf: lua_CFunction; setf: lua_CFunction=nil);
var t: integer;
begin
  lua_pushstring(L, propertyname);
  lua_newtable(L);

  t:=lua_gettop(L);

  luaclassarray_createMetaTable(L, userdata, getf, setf);
  lua_setmetatable(L, t);

  lua_settable(L, metatable);
end;

procedure luaclass_addPropertyToTable(L: PLua_State; metatable: integer; userdata: integer; propertyname: string; getfunction: lua_CFunction; setfunction: lua_CFunction);
var t: integer;
begin
  lua_pushstring(L, propertyname);
  lua_newtable(L);
  t:=lua_gettop(L);

  if assigned(getfunction) then
  begin
    lua_pushstring(L,'__get');
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, getfunction, 1);
    lua_settable(L, t);
  end;

  if assigned(setfunction) then
  begin
    lua_pushstring(L,'__set');
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, setfunction, 1);
    lua_settable(L, t);
  end;

  lua_settable(L, metatable);

  //add a lowercase variant if needed
  if propertyname[1] in ['A'..'Z'] then
  begin
    propertyname[1]:=lowercase(propertyname[1]);
    luaclass_addPropertyToTable(L, metatable, userdata, propertyname, getfunction, setfunction);
  end;
end;

procedure luaclass_addClassFunctionToTable(L: PLua_State; metatable: integer; userdata: integer; functionname: string; f: lua_CFunction);
begin
  lua_pushstring(L, functionname);
  lua_pushvalue(L, userdata);
  lua_pushcclosure(L, f, 1);
  lua_settable(L, metatable);

  if functionname<>'' then
  begin
    //add a secondary method where the name starts with a capital
    functionname[1]:=uppercase(functionname[1])[1];
    lua_pushstring(L, functionname);
    lua_pushvalue(L, userdata);
    lua_pushcclosure(L, f, 1);
    lua_settable(L, metatable);
  end;
end;

function luaclass_compare(L: PLua_State): integer; cdecl; //__eq
//parameters: (O1, O2)
//return nil or false for false
var o1, o2: TObject;
begin

  o1:=lua_ToCEUserData(L, 1);
  o2:=lua_ToCEUserData(L, 2);

  lua_pushboolean(L, o1=o2);
  result:=1;
end;

function luaclass_newindex(L: PLua_State): integer; cdecl; //set
//parameters: (self, key, newvalue)
var metatable: integer;
begin
  result:=0;
  lua_getmetatable(L, 1); //get the metatable of self
  metatable:=lua_gettop(L); //store the metatable index

  lua_pushvalue(L, 2); //push the key

  lua_gettable(L, -2); //metatable[key]
  if lua_istable(L ,-1) then
  begin
    lua_pushstring(L, '__set');
    lua_gettable(L, -2); //table['__set']

    if lua_isfunction(L, -1) then
    begin
      lua_pushvalue(L, 3); //push newvalue    (so stack now holds, function, newvalue)
      lua_call(L, 1, 0);
      exit;
    end;
  end;



  if lua_isnil(L, -1) then
  begin
    //not in the list
    lua_pop(L,1);

    //check if key is a number
    if lua_isnumber(L, 2) then
    begin
      //check if there is a __defaultintegergetindexhandler defined in the metatable
      lua_pushstring(L, '__defaultintegersetindexhandler');
      lua_gettable(L, metatable);
      if lua_isfunction(L,-1) then
      begin
        //yes
        lua_pushvalue(L, 2); //key
        lua_pushvalue(L, 3); //value
        lua_call(L, 2,0); //call __defaultintegersetindexhandler(key, value);
        exit;
      end
      else
        lua_pop(L,1);
    end;

    if lua_type(L, 2)=LUA_TSTRING then
    begin
      //check if there is a __defaultstringsetindexhandler defined in the metatable
      lua_pushstring(L, '__defaultstringsetindexhandler');
      lua_gettable(L, metatable);
      if lua_isfunction(L,-1) then
      begin
        lua_pushvalue(L, 2); //key
        lua_pushvalue(L, 3); //value
        lua_call(L, 2, 0); //call __defaultstringsetindexhandler(key, value)
        exit;
      end
      else
        lua_pop(L,1);
    end;
  end;

  //this entry was not in the list
  //Let's see if this is a published property or custom value
  lua_pushcfunction(L, lua_setProperty);
  lua_pushvalue(L, 1); //userdata
  lua_pushvalue(L, 2); //keyname
  lua_pushvalue(L, 3); //value
  lua_call(L,3,0);

end;

function luaclass_index(L: PLua_State): integer; cdecl; //get
//parameters: (self, key)
//called when a class object is indexed
//return the metatable element with this name

//wants to get the value of table[key] , but table isn'ty really a table
var i: integer;

    metatable, metatable2: integer;

    s: string;
    o: TObject;
begin
  //return the metatable element


  i:=lua_gettop(L);

  result:=0;

  if i=2 then
  begin
    lua_getmetatable(L, 1); //get the metatable from the table and push i on the stack
    metatable:=lua_gettop(L);

    lua_pushvalue(L, 2); //push the key on the stack


    //lua_rawget(L, metatable);
    lua_gettable(L, metatable); //get metatable[key]

    if lua_istable(L ,-1) then
    begin
      //perhaps an array
      //lua_getmetatable(L,-1);
      lua_pushstring(L, '__get');
      lua_gettable(L, -2);

      if lua_isfunction(L, -1) then
        lua_call(L, 0, 1)
      else //return the table that was stored in the metatable (so undo the result of getting __get)
        lua_pop(L,1);
    end
    else
    begin
      if lua_isnil(L, -1) then
      begin
        //this entry was not in the list
        lua_pop(L,1);

        //check if the key can be a number
        if lua_isnumber(L, 2) then
        begin
          //check if there is a __defaultintegergetindexhandler defined in the metatable
          lua_pushstring(L, '__defaultintegergetindexhandler');
          lua_gettable(L, metatable);
          if lua_isfunction(L,-1) then
          begin
            lua_pushvalue(L, 2); //key
            lua_call(L, 1, 1); //call __defaultintegergetindexhandler(key)
            result:=1;
            exit;
          end;
        end;

        lua_pushstring(L, '__norealclass');
        lua_gettable(L, metatable);
        if lua_isboolean(L, -1) and lua_toboolean(L, -1) then
        begin
          lua_pop(L,1);
          lua_pushnil(L);
          result:=1;
          exit;
        end;

        //Let's see if this is a published property
        lua_pushcfunction(L, lua_getProperty);
        lua_pushvalue(L, 1); //userdata
        lua_pushvalue(L, 2); //keyname
        lua_call(L,2,1);
        result:=1;

        if lua_isnil(L, -1) then
        begin
          //not a property
          lua_pop(L,1);

          o:=tobject(lua_touserdata(L,1)^);
          if o is TComponent then
          begin
            lua_pushcfunction(L, component_findComponentByName);
            lua_pushvalue(L, 1); //userdata
            lua_pushvalue(L, 2); //keyname
            lua_call(L, 2, 1); //component_findComponentByName

            if not lua_isnil(L,-1) then exit(1);

            //still here so not a component of the component

            lua_pop(L,1);
          end;

          if lua_type(L, 2)=LUA_TSTRING then
          begin
            //check if there is a __defaultstringgetindexhandler defined in the metatable
            lua_pushstring(L, '__defaultstringgetindexhandler');
            lua_gettable(L, metatable);
            if lua_isfunction(L,-1) then
            begin
              lua_pushvalue(L, 2); //key
              lua_call(L, 1, 1); //call __defaultstringgetindexhandler(key)
              exit(1);
            end
            else
              lua_pop(L,1);
          end;

        end;



      end;
    end;
    result:=1;
  end;

end;

function luaclass_garbagecollect(L: PLua_State): integer; cdecl; //gc
var autodestroy: boolean;
    o: tobject;
    mt: integer;
begin
  lua_getmetatable(L, 1);
  mt:=lua_gettop(L);
  lua_pushstring(L, '__autodestroy');
  lua_gettable(L, mt);
  autodestroy:=lua_toboolean(L, -1);
  if autodestroy then
  begin
    //kill it

    lua_pushstring(L, 'destroy');
    lua_gettable(L, mt);
    if lua_isfunction(L, -1) then
      lua_call(L, 0,0);

  end;

  result:=0;
end;



procedure luaclass_setAutoDestroy(L: PLua_State; metatable: integer; state: boolean);
begin
  lua_pushstring(L, '__autodestroy');
  lua_pushboolean(L, state);
  lua_settable(L, metatable);
end;

function luaclass_createMetaTable(L: Plua_State; garbagecollectable: boolean=false): integer;
//creates a table to be used as a metatable
//returns the stack index of the table
begin
  lua_newtable(L);
  result:=lua_gettop(L);

  luaclass_setAutoDestroy(L, result, garbagecollectable); //default do not destroy when garbage collected. Let the user do it

  //set the index method
  lua_pushstring(L, '__index');
  lua_pushcfunction(L, luaclass_index);
  lua_settable(L, result);

  lua_pushstring(L, '__newindex');
  lua_pushcfunction(L, luaclass_newindex);
  lua_settable(L, result);

  lua_pushstring(L, '__gc');
  lua_pushcfunction(L, luaclass_garbagecollect);
  lua_settable(L, result);

  lua_pushstring(L, '__eq');
  if objectcomparefunctionref=0 then //get it
  begin
    lua_pushcfunction(L, luaclass_compare);
    objectcomparefunctionref := luaL_ref(L, LUA_REGISTRYINDEX);
  end;

  lua_rawgeti(L, LUA_REGISTRYINDEX, objectcomparefunctionref);
  lua_settable(L, result);



end;




end.

