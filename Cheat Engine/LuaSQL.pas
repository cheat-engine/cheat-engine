unit LuaSQL;

{
routines for SQL access
example script:
c=createSQLite3Connection()
c.DatabaseName='e:\\something.sqlite3'
c.Connected=true

t=createSQLTransaction()
t.SQLConnection=c
t.Active=true

tn=c.getTableNames()
if #tn==0 then
  print("empty")

  c.ExecuteDirect([[
    CREATE TABLE something (
    'id' INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    'processname' char(256) NOT NULL,
    'rating' INTEGER);
  ]])
  tn=c.getTableNames()
  if #tn==0 then error('no table created') end

end

print("tables:")
local hassomething=false
local i
for i=1,#tn do
  print(tn[i])
  if tn[i]=='something' then hassomething=true end
end

if hassomething then
  pn=process
  if (pn==nil) or (p=='nil') then pn='no process'  end

  --check if pn is in the table, if so, increment rating, else add it
  q=createSQLQuery()
  q.SQLConnection=c
  q.SQLTransaction=t

  q.SQL.Text=string.format([[ Select * from something where processname='%s' ]],pn) --not secure, I know, just an example
  q.Active=true

  if q.RecordCount>0 then
    print("In the list")
    fields=q.Fields
    print("fieldcount="..fields.Count)

    local id=q.FieldByName('id').asInteger
    print("id="..id)

    local rating=q.FieldByName('rating')
    print("rating="..rating.asInteger)

    q.Active=false
    q.StatementType='stUpdate'
    q.SQL.Text=string.format([[update something set rating=rating+1 where id=%d]], id)
    q.ExecSQL()
                                   09
  else
    print("Not yet in the list")
    c.ExecuteDirect(string.format([[insert into something(processname,rating) values('%s',1)]],pn))
  end

  q.Active=false

  q.StatementType='stSelect'
  q.SQL.Text=[[select * from something]]
  q.Active=true

  count=1
  print(string.format("RecordCount=%d", q.RecordCount))
  while not q.EOF do
    print("line "..count)
    local i
    for i=0, q.Fields.Count-1 do
      print(string.format("  %s = %s", q.Fields[i].FieldName, q.Fields[i].Value))
    end

    q.next()
    count=count+1
  end

  q.Active=false
  q.destroy()

else
  error("something doesn't exist")
end

t.Commit()
t.Active=false
c.Connected=false

t.destroy()
c.destroy()

}

{$mode delphi}

interface

uses
  Classes, SysUtils, db, sqldb, sqlite3, sqlite3conn, odbcconn, BufDataset,
  LuaComponent, LuaClass, luaobject, lua, lualib, lauxlib, typinfo;

procedure initializeLuaSQL;

implementation

uses LuaHandler, LuaCollection, LuaCollectionItem, LuaByteTable;

function createSQLQuery(L: Plua_State): integer; cdecl;
var owner: TComponent;
begin
  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  luaclass_newClass(L, TSQLQuery.Create(owner));
  result:=1;

end;


function createSQLTransaction(L: Plua_State): integer; cdecl;
var owner: TComponent;
begin
  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  luaclass_newClass(L, TSQLTransaction.Create(owner));
  result:=1;
end;

function createSQLite3Connection(L: Plua_State): integer; cdecl;
var owner: TComponent;
begin
  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  luaclass_newClass(L, TSQLite3Connection.Create(owner));
  result:=1;
end;

function setSQLiteLibraryName(L: Plua_State): integer; cdecl;
begin
  if lua_gettop(L)=1 then
    SQLiteLibraryName:=Lua_ToString(L,1);

  result:=0;
end;

function sqlite3connection_createDB(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLite3Connection(luaclass_getClassObject(L)).CreateDB;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqlite3connection_dropDB(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLite3Connection(luaclass_getClassObject(L)).DropDB;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqlite3connection_getInsertID(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, TSQLite3Connection(luaclass_getClassObject(L)).GetInsertID);
  result:=1;
end;

function createODBCConnection(L: Plua_State): integer; cdecl;
var owner: TComponent;
begin
  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  luaclass_newClass(L, TODBCConnection.Create(owner));
  result:=1;
end;


function sqlconnection_startTransaction(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLConnection(luaclass_getClassObject(L)).StartTransaction;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqlconnection_endTransaction(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLConnection(luaclass_getClassObject(L)).EndTransaction;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqlconnection_executeDirect(L: Plua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    try
      TSQLConnection(luaclass_getClassObject(L)).ExecuteDirect(Lua_ToString(L,1));
    except
      on e:exception do
      begin
        lua_pushstring(L, e.message);
        lua_error(L);
      end;
    end;
  end;
end;

//procedure GetFieldNames(const TableName : string; List : TStrings); virtual;
function sqlconnection_getFieldNames(L: Plua_State): integer; cdecl;
var
  tablename: string;
  s: tstringlist;
  i: integer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    tablename:=Lua_ToString(L,1);
    try
      s:=tstringlist.create;
      try
        TSQLConnection(luaclass_getClassObject(L)).GetFieldNames(tablename,s);

        result:=1;
        lua_newtable(L);
        for i:=0 to s.Count-1 do
        begin
          lua_pushinteger(L,i+1);
          lua_pushstring(L,s[i]);
          lua_settable(L,-3);
        end;
      finally
        s.free;
      end;
    except
      on e:exception do
      begin
        lua_pushstring(L, e.message);
        lua_error(L);
      end;
    end;
  end;
end;


function sqlconnection_getTableNames(L: Plua_State): integer; cdecl;
var
  s: tstringlist;
  i: integer;
begin
  result:=0;

  try
    s:=tstringlist.create;
    try
      TSQLConnection(luaclass_getClassObject(L)).GetTableNames(s);

      result:=1;
      lua_newtable(L);
      for i:=0 to s.Count-1 do
      begin
        lua_pushinteger(L,i+1);
        lua_pushstring(L,s[i]);
        lua_settable(L,-3);
      end;
    finally
      s.free;
    end;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;

end;

function database_getTransactionCount(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, TDatabase(luaclass_getClassObject(L)).TransactionCount);
  result:=1;
end;

function customconnection_open(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TCustomConnection(luaclass_getClassObject(L)).Open;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function customconnection_close(L: Plua_State): integer; cdecl;
var force: boolean;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    force:=lua_toboolean(L,1)
  else
    force:=false;

  try
    TCustomConnection(luaclass_getClassObject(L)).Close(force);
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function dbtransaction_getDataBase(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L,TDBTransaction(luaclass_getClassObject(L)).DataBase);
  result:=1;
end;

function dbtransaction_setDataBase(L: Plua_State): integer; cdecl;
begin
  try
    TDBTransaction(luaclass_getClassObject(L)).DataBase:=lua_ToCEUserData(L,1);
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;

  result:=0;
end;

function dbtransaction_closeDataSets(L: Plua_State): integer; cdecl;
begin
  TDBTransaction(luaclass_getClassObject(L)).CloseDataSets;
  result:=0;
end;

function sqltransaction_commit(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLTransaction(luaclass_getClassObject(L)).Commit;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqltransaction_commitRetaining(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLTransaction(luaclass_getClassObject(L)).CommitRetaining;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqltransaction_rollback(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLTransaction(luaclass_getClassObject(L)).rollback;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqltransaction_rollbackRetaining(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLTransaction(luaclass_getClassObject(L)).rollbackRetaining;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqltransaction_startTransaction(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLTransaction(luaclass_getClassObject(L)).StartTransaction;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

function sqltransaction_endTransaction(L: Plua_State): integer; cdecl;
begin
  result:=0;
  try
    TSQLTransaction(luaclass_getClassObject(L)).EndTransaction;
  except
    on e:exception do
    begin
      lua_pushstring(L, e.message);
      lua_error(L);
    end;
  end;
end;

//-----------------------

function dataset_getBlockReadSize(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tdataset(luaclass_getClassObject(L)).BlockReadSize);
  result:=1;
end;

function dataset_setBlockReadSize(L: Plua_State): integer; cdecl;
begin
  result:=0;
  tdataset(luaclass_getClassObject(L)).BlockReadSize:=lua_tointeger(L,1);
end;

function dataset_getBOF(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).BOF);
  result:=1;
end;

function dataset_getCanModify(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).CanModify);
  result:=1;
end;

function dataset_getDefaultFields(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).DefaultFields);
  result:=1;
end;

function dataset_getEOF(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).EOF);
  result:=1;
end;

function dataset_getFieldCount(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tdataset(luaclass_getClassObject(L)).FieldCount);
  result:=1;
end;

function dataset_getFields(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, tdataset(luaclass_getClassObject(L)).Fields);
  result:=1;
end;

function dataset_getFound(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).Found);
  result:=1;
end;

function dataset_getModified(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).Modified);
  result:=1;
end;

function dataset_getIsUniDirectional(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).IsUniDirectional);
  result:=1;
end;

function dataset_getRecordCount(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tdataset(luaclass_getClassObject(L)).RecordCount);
  result:=1;
end;

function dataset_getRecNo(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tdataset(luaclass_getClassObject(L)).RecNo);
  result:=1;
end;

function dataset_setRecNo(L: Plua_State): integer; cdecl;
begin
  result:=0;
  tdataset(luaclass_getClassObject(L)).RecNo:=lua_tointeger(L,1);
end;

function dataset_getFieldValues(L: PLua_State): integer; cdecl;
var
  dataset: TDataSet;
  v: string;
begin
  result:=0;
  dataset:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    v:=Lua_ToString(L,1);

    lua_pushvariant(L, dataset.FieldValues[v]);
    result:=1;
  end;
end;

function dataset_getFilter(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, tdataset(luaclass_getClassObject(L)).Filter);
  result:=1;
end;

function dataset_setFilter(L: Plua_State): integer; cdecl;
begin
  result:=0;
  tdataset(luaclass_getClassObject(L)).Filter:=Lua_ToString(L,1);
end;

function dataset_getFiltered(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).Filtered);
  result:=1;
end;

function dataset_setFiltered(L: Plua_State): integer; cdecl;
begin
  result:=0;
  tdataset(luaclass_getClassObject(L)).Filtered:=lua_toboolean(L,1);
end;


function dataset_setFieldValues(L: PLua_State): integer; cdecl;
var
  dataset: TDataSet;
  v: string;
begin
  result:=0;
  dataset:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    v:=Lua_ToString(L,1);
    dataset.FieldValues[v]:=lua_tovariant(L,2);
  end;
end;

function dataset_getFilterOptions(L: Plua_State): integer; cdecl;
var ti: PTypeInfo;
begin
  ti:=typeinfo(TFilterOptions);

  lua_pushstring(L, SetToString(ti, integer(tdataset(luaclass_getClassObject(L)).FilterOptions),true));
  result:=1;
end;

function dataset_setFilterOptions(L: Plua_State): integer; cdecl;
var ti: PTypeInfo;
begin
  ti:=typeinfo(TFilterOptions);

  tdataset(luaclass_getClassObject(L)).FilterOptions:=TFilterOptions(StringToSet(ti,Lua_ToString(L,1)));
  result:=0;
end;

function dataset_getActive(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).Active);
  result:=1;
end;

function dataset_setActive(L: Plua_State): integer; cdecl;
begin
  result:=0;
  tdataset(luaclass_getClassObject(L)).Active:=lua_toboolean(L,1);
end;

function dataset_getAutoCalcFields(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).AutoCalcFields);
  result:=1;
end;

function dataset_setAutoCalcFields(L: Plua_State): integer; cdecl;
begin
  result:=0;
  tdataset(luaclass_getClassObject(L)).AutoCalcFields:=lua_toboolean(L,1);
end;

function dataset_append(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).Append;
  result:=0;
end;

function dataset_cancel(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).cancel;
  result:=0;
end;

function dataset_checkBrowseMode(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).CheckBrowseMode;
  result:=0;
end;

function dataset_clearFields(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).ClearFields;
  result:=0;
end;

function dataset_close(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).Close;
  result:=0;
end;

function dataset_controlsDisabled(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).ControlsDisabled);
  result:=1;
end;

function dataset_cursorPosChanged(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).CursorPosChanged;
  result:=0;
end;

function dataset_delete(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).Delete;
  result:=0;
end;

function dataset_disableControls(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).DisableControls;
  result:=0;
end;

function dataset_edit(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).Edit;
  result:=0;
end;

function dataset_enableControls(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).EnableControls;
  result:=0;
end;

function dataset_fieldByName(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, tdataset(luaclass_getClassObject(L)).FieldByName(lua_tostring(L,1)));
  result:=1;
end;

function dataset_findField(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, tdataset(luaclass_getClassObject(L)).FindField(lua_tostring(L,1)));
  result:=1;
end;

function dataset_findFirst(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).FindFirst);
  result:=1;
end;

function dataset_findLast(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).FindLast);
  result:=1;
end;

function dataset_findNext(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).FindNext);
  result:=1;
end;

function dataset_findPrior(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).FindPrior);
  result:=1;
end;

function dataset_first(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).First;
  result:=0;
end;

function dataset_insert(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).Insert;
  result:=0;
end;

function dataset_isEmpty(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).IsEmpty);
  result:=1;
end;

function dataset_last(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).last;
  result:=0;
end;

function dataset_locate(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tdataset(luaclass_getClassObject(L)).Locate(Lua_ToString(L,1), lua_tovariant(L,2), TLocateOptions(StringToSet(PTypeInfo(typeinfo(TLocateOptions)),lua_tovariant(L,3)))));
  result:=1;
end;

function dataset_lookup(L: Plua_State): integer; cdecl;
begin
  lua_pushvariant(L, tdataset(luaclass_getClassObject(L)).Lookup(Lua_ToString(L,1), lua_tovariant(L,2), lua_tostring(L,3)));
  result:=1;
end;

function dataset_moveBy(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tdataset(luaclass_getClassObject(L)).MoveBy(lua_tointeger(L,1)));
  result:=1;
end;

function dataset_next(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).next;
  result:=0;
end;

function dataset_open(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).open;
  result:=0;
end;

function dataset_post(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).post;
  result:=0;
end;

function dataset_prior(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).prior;
  result:=0;
end;

function dataset_refresh(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).refresh;
  result:=0;
end;

function dataset_updateCursorPos(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).UpdateCursorPos;
  result:=0;
end;

function dataset_updateRecord(L: Plua_State): integer; cdecl;
begin
  tdataset(luaclass_getClassObject(L)).UpdateRecord;
  result:=0;
end;

procedure dataset_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  component_addMetaData(L, metatable, userdata);

  luaclass_addPropertyToTable(L, metatable, userdata, 'BlockReadSize', dataset_getBlockReadSize, dataset_setBlockReadSize);
  luaclass_addPropertyToTable(L, metatable, userdata, 'BOF', dataset_getBOF, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'CanModify', dataset_getCanModify, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'DefaultFields', dataset_getDefaultFields, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'EOF', dataset_getEOF, nil);

  luaclass_addPropertyToTable(L, metatable, userdata, 'FieldCount', dataset_getFieldCount, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Fields', dataset_getFields, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Found', dataset_getFound, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Modified', dataset_getModified, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'IsUniDirectional', dataset_getIsUniDirectional, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'RecordCount', dataset_getRecordCount, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'RecNo', dataset_getRecNo, dataset_setRecNo);
  luaclass_addArrayPropertyToTable(L, metatable,userdata,'FieldValues',dataset_getFieldValues, dataset_setFieldValues);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Filter', dataset_getFilter, dataset_setFilter);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Filtered', dataset_getFiltered, dataset_setFiltered);
  luaclass_addPropertyToTable(L, metatable, userdata, 'FilterOptions', dataset_getFilterOptions, dataset_setFilterOptions);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Active', dataset_getActive, dataset_setActive); //
  luaclass_addPropertyToTable(L, metatable, userdata, 'AutoCalcFields', dataset_getAutoCalcFields, dataset_setAutoCalcFields);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'append', dataset_append);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'cancel', dataset_cancel);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'checkBrowseMode', dataset_checkBrowseMode);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clearFields', dataset_clearFields);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'close', dataset_close);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'controlsDisabled', dataset_controlsDisabled);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'cursorPosChanged', dataset_cursorPosChanged);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'delete', dataset_delete);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'disableControls', dataset_disableControls);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'edit', dataset_edit);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'enableControls', dataset_enableControls);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'fieldByName', dataset_FieldByName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'findField', dataset_FindField);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'findFirst', dataset_FindFirst);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'findLast', dataset_FindLast);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'findNext', dataset_FindNext);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'findPrior', dataset_FindPrior);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'first', dataset_First);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'insert', dataset_Insert);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'isEmpty', dataset_IsEmpty);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'last', dataset_Last);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'locate', dataset_locate);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'lookup', dataset_lookup);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'moveBy', dataset_moveBy);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'next', dataset_next);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'open', dataset_open);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'post', dataset_post);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'prior', dataset_prior);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'refresh', dataset_refresh);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'updateCursorPos', dataset_updateCursorPos);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'updateRecord', dataset_updateRecord);
end;

function dbdataset_getDatabase(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, tdbdataset(luaclass_getClassObject(L)).DataBase);
  result:=1;
end;

function dbdataset_setDatabase(L: Plua_State): integer; cdecl;
begin
  result:=0;
  tdbdataset(luaclass_getClassObject(L)).DataBase:=lua_ToCEUserData(L,1);
end;

function dbdataset_getTransaction(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, tdbdataset(luaclass_getClassObject(L)).Transaction);
  result:=1;
end;

function dbdataset_setTransaction(L: Plua_State): integer; cdecl;
begin
  result:=0;
  tdbdataset(luaclass_getClassObject(L)).Transaction:=lua_ToCEUserData(L,1);
end;


procedure dbdataset_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  dataset_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Database', dbdataset_getDatabase, dbdataset_setDatabase);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Transaction', dbdataset_getTransaction, dbdataset_setTransaction);
end;

function custombufdataset_getMaxIndexesCount(L: Plua_State): integer; cdecl;
begin
  result:=1;
  lua_pushinteger(L, TCustomBufDataset(luaclass_getClassObject(L)).MaxIndexesCount);
end;

function custombufdataset_setMaxIndexesCount(L: Plua_State): integer; cdecl;
begin
  result:=0;
  TCustomBufDataset(luaclass_getClassObject(L)).MaxIndexesCount:=lua_tointeger(L,1);
end;

function custombufdataset_getChangeCount(L: Plua_State): integer; cdecl;
begin
  result:=1;
  lua_pushinteger(L, TCustomBufDataset(luaclass_getClassObject(L)).ChangeCount);
end;

function custombufdataset_getReadOnly(L: Plua_State): integer; cdecl;
begin
  result:=1;
  lua_pushboolean(L, TCustomBufDataset(luaclass_getClassObject(L)).ReadOnly);
end;

function custombufdataset_setReadOnly(L: Plua_State): integer; cdecl;
begin
  result:=0;
  TCustomBufDataset(luaclass_getClassObject(L)).ReadOnly:=lua_toboolean(L,1);
end;

procedure custombufdataset_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  dbdataset_addMetaData(L, metatable, userdata);

  luaclass_addPropertyToTable(L, metatable, userdata, 'MaxIndexesCount', custombufdataset_getMaxIndexesCount, custombufdataset_setMaxIndexesCount);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ChangeCount', custombufdataset_getChangeCount, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'ReadOnly', custombufdataset_getReadOnly, custombufdataset_setReadOnly);
end;


function customsqlquery_getPrepared(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, TCustomSQLQuery(luaclass_getClassObject(L)).Prepared);
  result:=1;
end;

function customsqlquery_getSQLConnection(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, TCustomSQLQuery(luaclass_getClassObject(L)).SQLConnection);
  result:=1;
end;

function customsqlquery_setSQLConnection(L: Plua_State): integer; cdecl;
begin
  result:=0;
  TCustomSQLQuery(luaclass_getClassObject(L)).SQLConnection:=lua_ToCEUserData(L,1);
end;

function customsqlquery_getSQLTransaction(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, TCustomSQLQuery(luaclass_getClassObject(L)).SQLTransaction);
  result:=1;
end;

function customsqlquery_setSQLTransaction(L: Plua_State): integer; cdecl;
begin
  result:=0;
  TCustomSQLQuery(luaclass_getClassObject(L)).SQLTransaction:=lua_ToCEUserData(L,1);
end;

function customsqlquery_getChangeCount(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, TCustomSQLQuery(luaclass_getClassObject(L)).ChangeCount);
  result:=1;
end;

function customsqlquery_prepare(L: Plua_State): integer; cdecl;
begin
  TCustomSQLQuery(luaclass_getClassObject(L)).Prepare;
  result:=0;
end;

function customsqlquery_unprepare(L: Plua_State): integer; cdecl;
begin
  TCustomSQLQuery(luaclass_getClassObject(L)).unPrepare;
  result:=0;
end;

function customsqlquery_execSQL(L: Plua_State): integer; cdecl;
begin
  TCustomSQLQuery(luaclass_getClassObject(L)).ExecSQL;
  result:=0;
end;

function customsqlquery_rowsAffected(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, TCustomSQLQuery(luaclass_getClassObject(L)).rowsAffected);
  result:=1;
end;

function customsqlquery_paramByName(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, TCustomSQLQuery(luaclass_getClassObject(L)).ParamByName(Lua_ToString(L,1)));
  result:=1;
end;

procedure customsqlquery_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  custombufdataset_addMetaData(L, metatable, userdata);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Prepared', customsqlquery_getPrepared, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SQLConnection', customsqlquery_getSQLConnection, customsqlquery_setSQLConnection);
  luaclass_addPropertyToTable(L, metatable, userdata, 'SQLTransaction', customsqlquery_getSQLTransaction, customsqlquery_setSQLTransaction);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'prepare', customsqlquery_prepare);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'unprepare', customsqlquery_unprepare);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'execSQL', customsqlquery_execSQL);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'rowsAffected', customsqlquery_rowsAffected);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'paramByName', customsqlquery_paramByName);
end;


function sqlquery_getSchemaType(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, GetEnumName(typeinfo(TSchemaType), integer(tsqlquery(luaclass_getClassObject(L)).SchemaType)));
  result:=1;
end;

function sqlquery_getStatementType(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, GetEnumName(typeinfo(TStatementType), integer(tsqlquery(luaclass_getClassObject(L)).StatementType)));
  result:=1;
end;


procedure sqlquery_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customsqlquery_addMetaData(L, metatable, userdata);

  //TSQLQuery(0).Params;
  luaclass_addPropertyToTable(L, metatable, userdata, 'SchemaType', sqlquery_getSchemaType, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'StatementType', sqlquery_getStatementType, nil);


end;

function param_getAsBoolean(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tparam(luaclass_getClassObject(L)).AsBoolean);
  result:=1;
end;

function param_setAsBoolean(L: Plua_State): integer; cdecl;
begin
  tparam(luaclass_getClassObject(L)).AsBoolean:=lua_toboolean(L,1);
  result:=0;
end;

function param_getAsByteTable(L: Plua_State): integer; cdecl;
var tb: Tbytes;
begin
  tb:=tparam(luaclass_getClassObject(L)).AsBytes;
  LuaByteTable.CreateByteTableFromPointer(L, @tb[0], length(tb));
  result:=1;
end;

function param_setAsByteTable(L: Plua_State): integer; cdecl;
var
  maxsize: integer;
  tb: Tbytes;
begin
  maxsize:=lua_objlen(l, 1);
  setlength(tb, maxsize);
  LuaByteTable.readBytesFromTable(L,1,@tb[0],maxsize);
  tparam(luaclass_getClassObject(L)).AsBytes:=tb;

  result:=0;
end;

function param_getAsInteger(L: Plua_State): integer; cdecl;
begin
  lua_pushInteger(L, tparam(luaclass_getClassObject(L)).AsInteger);
  result:=1;
end;

function param_setAsInteger(L: Plua_State): integer; cdecl;
begin
  tparam(luaclass_getClassObject(L)).AsInteger:=lua_toInteger(L,1);
  result:=0;
end;

function param_getAsNumber(L: Plua_State): integer; cdecl;
begin
  lua_pushNumber(L, tparam(luaclass_getClassObject(L)).AsFloat);
  result:=1;
end;

function param_setAsNumber(L: Plua_State): integer; cdecl;
begin
  tparam(luaclass_getClassObject(L)).AsFloat:=lua_toNumber(L,1);
  result:=0;
end;

function param_getAsString(L: Plua_State): integer; cdecl;
begin
  lua_pushString(L, tparam(luaclass_getClassObject(L)).AsString);
  result:=1;
end;

function param_setAsString(L: Plua_State): integer; cdecl;
begin
  tparam(luaclass_getClassObject(L)).AsString:=lua_toString(L,1);
  result:=0;
end;

function param_getText(L: Plua_State): integer; cdecl;
begin
  lua_pushString(L, tparam(luaclass_getClassObject(L)).Text);
  result:=1;
end;

function param_setText(L: Plua_State): integer; cdecl;
begin
  tparam(luaclass_getClassObject(L)).Text:=lua_toString(L,1);
  result:=0;
end;

function param_isNull(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tparam(luaclass_getClassObject(L)).IsNull);
  result:=1;
end;

function params_addParam(L: Plua_State): integer; cdecl;
begin
  tparams(luaclass_getClassObject(L)).AddParam(tparam(lua_ToCEUserData(L,1)));
  result:=0;
end;

function params_getItem(L: PLua_State): integer; cdecl;
var
  params: TParams;
  index: integer;
begin
  result:=0;
  params:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_toInteger(L,1);
    luaclass_newClass(L, params.Items[index]);
    result:=1;
  end;
end;

function params_setItem(L: PLua_State): integer; cdecl;
var
  params: TParams;
  index: integer;
begin
  result:=0;
  params:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    index:=lua_toInteger(L,1);
    params.Items[index]:=lua_ToCEUserData(L,2);
  end;
end;

function field_getAsVariant(L: Plua_State): integer; cdecl;
begin
  try
    lua_pushvariant(L, tfield(luaclass_getClassObject(L)).Value);
    result:=1;
  except
    on e:exception do
    begin
      lua_pushstring(L,e.message);
      lua_error(L);
    end;
  end;
end;

function field_setAsVariant(L: Plua_State): integer; cdecl;
begin
  try
    tfield(luaclass_getClassObject(L)).Value:=lua_tovariant(L,1);
  except
    on e:exception do
    begin
      lua_pushstring(L,e.message);
      lua_error(L);
    end;
  end;
  result:=0;
end;

function field_getDataType(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, GetEnumName(typeinfo(TFieldType), integer(tfield(luaclass_getClassObject(L)).DataType)));
  result:=1;
end;

function field_getSize(L: Plua_State): integer; cdecl;
begin
  lua_pushInteger(L, tfield(luaclass_getClassObject(L)).Size);
  result:=1;
end;

function field_setSize(L: Plua_State): integer; cdecl;
begin
  tfield(luaclass_getClassObject(L)).Size:=lua_toInteger(L,1);
  result:=0;
end;


function field_getAsBoolean(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tfield(luaclass_getClassObject(L)).AsBoolean);
  result:=1;
end;

function field_setAsBoolean(L: Plua_State): integer; cdecl;
begin
  tfield(luaclass_getClassObject(L)).AsBoolean:=lua_toboolean(L,1);
  result:=0;
end;

function field_getAsByteTable(L: Plua_State): integer; cdecl;
var tb: Tbytes;
begin
  tb:=tfield(luaclass_getClassObject(L)).AsBytes;
  LuaByteTable.CreateByteTableFromPointer(L, @tb[0], length(tb));
  result:=1;
end;

function field_setAsByteTable(L: Plua_State): integer; cdecl;
var
  maxsize: integer;
  tb: Tbytes;
begin
  maxsize:=lua_objlen(l, 1);
  setlength(tb, maxsize);
  LuaByteTable.readBytesFromTable(L,1,@tb[0],maxsize);
  tfield(luaclass_getClassObject(L)).AsBytes:=tb;

  result:=0;
end;

function field_getAsInteger(L: Plua_State): integer; cdecl;
begin
  lua_pushInteger(L, tfield(luaclass_getClassObject(L)).AsInteger);
  result:=1;
end;

function field_setAsInteger(L: Plua_State): integer; cdecl;
begin
  tfield(luaclass_getClassObject(L)).AsInteger:=lua_toInteger(L,1);
  result:=0;
end;

function field_getAsNumber(L: Plua_State): integer; cdecl;
begin
  lua_pushNumber(L, tfield(luaclass_getClassObject(L)).AsFloat);
  result:=1;
end;

function field_setAsNumber(L: Plua_State): integer; cdecl;
begin
  tfield(luaclass_getClassObject(L)).AsFloat:=lua_toNumber(L,1);
  result:=0;
end;

function field_getAsString(L: Plua_State): integer; cdecl;
begin
  lua_pushString(L, tfield(luaclass_getClassObject(L)).AsString);
  result:=1;
end;

function field_setAsString(L: Plua_State): integer; cdecl;
begin
  tfield(luaclass_getClassObject(L)).AsString:=lua_toString(L,1);
  result:=0;
end;

function field_getText(L: Plua_State): integer; cdecl;
begin
  lua_pushString(L, tfield(luaclass_getClassObject(L)).Text);
  result:=1;
end;

function field_setText(L: Plua_State): integer; cdecl;
begin
  tfield(luaclass_getClassObject(L)).Text:=lua_toString(L,1);
  result:=0;
end;

function field_isNull(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, tfield(luaclass_getClassObject(L)).IsNull);
  result:=1;
end;


procedure field_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  component_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Value', field_getAsVariant, field_setAsVariant);
  luaclass_addPropertyToTable(L, metatable, userdata, 'DataType', field_getDataType, nil);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Size', field_getSize, field_setSize);

  luaclass_addPropertyToTable(L, metatable, userdata, 'AsBoolean', field_getAsBoolean, field_setAsBoolean);
  luaclass_addPropertyToTable(L, metatable, userdata, 'AsByteTable', field_getAsByteTable, field_setAsByteTable);
  luaclass_addPropertyToTable(L, metatable, userdata, 'AsInteger', field_getAsInteger, field_setAsInteger);
  luaclass_addPropertyToTable(L, metatable, userdata, 'AsNumber', field_getAsNumber, field_setAsNumber);
  luaclass_addPropertyToTable(L, metatable, userdata, 'AsString', field_getAsString, field_setAsString);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Text', field_getText, field_setText);
  luaclass_addPropertyToTable(L, metatable, userdata, 'IsNull', field_isNull, nil);
end;

function fields_getCount(L: Plua_State): integer; cdecl;
begin
  lua_pushInteger(L, tfields(luaclass_getClassObject(L)).Count);
  result:=1;
end;

function fields_getFields(L: PLua_State): integer; cdecl;
var
  fields: Tfields;
  index: integer;
begin
  result:=0;
  fields:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    index:=lua_toInteger(L,1);
    luaclass_newClass(L, fields.Fields[index]);
    result:=1;
  end;
end;

function fields_setFields(L: PLua_State): integer; cdecl;
var
  fields: Tfields;
  index: integer;
begin
  result:=0;
  fields:=luaclass_getClassObject(L);
  if lua_gettop(L)=2 then
  begin
    index:=lua_toInteger(L,1);
    fields.Fields[index]:=lua_ToCEUserData(L,2);
  end;
end;

function fields_add(L: Plua_State): integer; cdecl;
begin
  tfields(luaclass_getClassObject(L)).Add(tfield(lua_ToCEUserData(L,1)));
  result:=0;
end;

function fields_clear(L: Plua_State): integer; cdecl;
begin
  tfields(luaclass_getClassObject(L)).clear;
  result:=0;
end;

function fields_fieldByName(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, tfields(luaclass_getClassObject(L)).FieldByName(Lua_ToString(L,1)));
  result:=1;
end;

function fields_fieldByNumber(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, tfields(luaclass_getClassObject(L)).FieldByNumber(lua_tointeger(L,1)));
  result:=1;
end;

function fields_indexOf(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, tfields(luaclass_getClassObject(L)).IndexOf(tfield(lua_ToCEUserData(L,1))));
  result:=1;
end;

procedure fields_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  object_addMetaData(L, metatable, userdata);

  luaclass_addPropertyToTable(L, metatable, userdata, 'Count', fields_getCount, nil);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Fields', fields_getFields, fields_setFields);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, fields_getFields, fields_setFields);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'add', fields_add);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'clear', fields_clear);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'fieldByName', fields_fieldByName);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'fieldByNumber', fields_fieldByNumber);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'indexOf', fields_indexOf);
end;




procedure params_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  collection_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'addParam', params_addParam);
  luaclass_addArrayPropertyToTable(L, metatable, userdata, 'Items', params_getItem, params_setItem);
  luaclass_setDefaultArrayProperty(L, metatable, userdata, params_getItem, params_setItem);
end;



procedure param_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  collectionItem_addMetaData(L, metatable, userdata);

  luaclass_addPropertyToTable(L, metatable, userdata, 'AsBoolean', param_getAsBoolean, param_setAsBoolean);
  luaclass_addPropertyToTable(L, metatable, userdata, 'AsByteTable', param_getAsByteTable, param_setAsByteTable);
  luaclass_addPropertyToTable(L, metatable, userdata, 'AsInteger', param_getAsInteger, param_setAsInteger);
  luaclass_addPropertyToTable(L, metatable, userdata, 'AsNumber', param_getAsNumber, param_setAsNumber);
  luaclass_addPropertyToTable(L, metatable, userdata, 'AsString', param_getAsString, param_setAsString);
  luaclass_addPropertyToTable(L, metatable, userdata, 'Text', param_getText, param_setText);
  luaclass_addPropertyToTable(L, metatable, userdata, 'IsNull', param_isNull, nil);
end;

procedure dbtransaction_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  component_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'closeDataSets', dbtransaction_closeDataSets);
  luaclass_addPropertyToTable(L, metatable, userdata, 'DataBase', dbtransaction_getDataBase, dbtransaction_setDataBase);
end;


procedure sqltransaction_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  dbtransaction_addMetaData(L, metatable, userdata);

  luaclass_addClassFunctionToTable(L, metatable, userdata, 'commit', sqltransaction_commit);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'commitRetaining', sqltransaction_commitRetaining);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'rollback', sqltransaction_rollback);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'rollbackRetaining', sqltransaction_rollbackRetaining);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'startTransaction', sqltransaction_startTransaction);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endTransaction', sqltransaction_endTransaction);

  luaclass_addPropertyToTable(L, metatable, userdata, 'SQLConnection', dbtransaction_getDataBase, dbtransaction_setDataBase); //not a mistake. sqlconnection is the database
end;

procedure customconnection_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  component_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'open', customconnection_open);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'close', customconnection_close);
end;

procedure database_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  customconnection_addMetaData(L, metatable, userdata);
  luaclass_addPropertyToTable(L, metatable, userdata, 'TransactionCount', database_getTransactionCount, nil);
end;


procedure sqlconnection_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  database_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'startTransaction', sqlconnection_startTransaction);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'endTransaction', sqlconnection_endTransaction);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'executeDirect', sqlconnection_executeDirect);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getTableNames', sqlconnection_getTableNames);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getFieldNames', sqlconnection_getFieldNames);

end;

procedure odbcconnection_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  sqlconnection_addMetaData(L, metatable, userdata);
end;

procedure sqlite3connection_addMetaData(L: PLua_state; metatable: integer; userdata: integer );
begin
  sqlconnection_addMetaData(L, metatable, userdata);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'createDB', sqlite3connection_createDB);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'dropDB', sqlite3connection_dropDB);
  luaclass_addClassFunctionToTable(L, metatable, userdata, 'getInsertID', sqlite3connection_getInsertID);

end;


procedure initializeLuaSQL;
begin
  lua_register(LuaVM, 'createSQLite3Connection', createSQLite3Connection);
  lua_register(LuaVM, 'createODBCConnection', createODBCConnection);
  lua_register(LuaVM, 'createSQLTransaction', createSQLTransaction);

  lua_register(LuaVM, 'createSQLQuery', createSQLQuery);

  lua_register(LuaVM, 'setSQLiteLibraryName', setSQLiteLibraryName);



end;

initialization
  {$ifdef cpu64}
    SQLiteLibraryName:='.\win64\sqlite3.dll';
  {$else}
    SQLiteLibraryName:='.\win32\sqlite3.dll';
  {$endif}
  luaclass_register(TODBCConnection, odbcconnection_addMetaData);
  luaclass_register(TSQLite3Connection,  sqlite3connection_addMetaData);
  luaclass_register(TSQLConnection,  sqlconnection_addMetaData);
  luaclass_register(TDatabase,  database_addMetaData);
  luaclass_register(TCustomConnection,  customconnection_addMetaData);
  luaclass_register(TSQLTransaction,  sqltransaction_addMetaData);
  luaclass_register(TDBTransaction,  dbtransaction_addMetaData);

  luaclass_register(TSQLQuery,  sqlquery_addMetaData);
  luaclass_register(TCustomSQLQuery, customsqlquery_addMetaData);
  luaclass_register(TCustomBufDataset, custombufdataset_addMetaData);
  luaclass_register(TDBDataSet, dbdataset_addMetaData);
  luaclass_register(TDataset, dataset_addMetaData);

  luaclass_register(TParam, param_addMetaData);
  luaclass_register(TParams, params_addMetaData);

  luaclass_register(TField, field_addMetaData);
  luaclass_register(TFields, fields_addMetaData);




  registerclass(TSQLConnection);
  registerclass(TSQLite3Connection);
  registerclass(TODBCConnection);
  registerclass(TSQLTransaction);
  registerclass(TSQLQuery);
end.

