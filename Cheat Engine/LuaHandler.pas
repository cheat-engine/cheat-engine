unit LuaHandler;



{todo: Split up into smaller units. 9255 lines is becomming too big}
{todo2: dll injecting lua into a target process}


{Note:
Assume all strings passed between lua are in UTF8 format
}

{$mode delphi}

interface

uses
  {$ifdef darwin}
  mactypes, macport, LCLIntf, LCLType, macCreateRemoteThread, dynlibs,
  {$endif}
  {$ifdef windows}
  jwawindows, windows, ShellApi,
  {$endif}
  vmxfunctions, Classes, dialogs, SysUtils, lua, lualib,
  lauxlib, syncobjs, syncobjs2, CEFuncProc, NewKernelHandler, Graphics,
  controls, LuaCaller, forms, ExtCtrls, StdCtrls, comctrls, ceguicomponents,
  genericHotkey, luafile, xmplayer_server, ExtraTrainerComponents, customtimer,
  menus, XMLRead, XMLWrite, DOM, Clipbrd, typinfo, PEInfoFunctions,
  LCLProc, strutils, registry, md5, commonTypeDefs, LResources, Translations,
  variants, LazUTF8, zstream, MemoryQuery, LCLVersion
  {$ifdef darwin}
  ,macportdefines
  {$endif}, betterControls;


const MAXTABLERECURSIONLOOKUP=2;

var
  _LuaVM: Plua_State;  //Global lua state (generally not used by anything except creating threads)
  _LuaCS: Tcriticalsection; //critical section for the _LuaVM state

threadvar
  Thread_LuaVM: PLua_State;
  Thread_LuaRef: integer;


function lua_strtofloat(s: string): double;
function lua_strtoint(s: string): integer;

procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
function lua_pcall(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
function lua_dostring(L: Plua_State; const str: PChar): Integer;

function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl;
procedure luaL_unref(L: Plua_State; t, ref: Integer); cdecl;


procedure Lua_RegisterObject(name: string; o: TObject);
function CheckIfConditionIsMetContext(threadid: dword; context: PContext; script: string): boolean;
procedure LUA_DoScript(s: string);
function LUA_functioncall(routinetocall: string; parameters: array of const): integer;
procedure LUA_memrec_callback(memrec: pointer; routine: string);
procedure LUA_SetCurrentContextState(tid: dword; context: PContext; extraregs: boolean=false);
procedure LUa_GetNewContextState(context: PContext; extraregs: boolean=false);

function LUA_onBreakpoint(threadid: dword; context: PContext; functionAlreadyPushed: boolean=false): boolean;
procedure LUA_onNotify(functionid: integer; sender: tobject);
function Lua_ToString(L: Plua_State; i: Integer): string;
function lua_ToCEUserData(L: PLua_state; i: integer): pointer;
function lua_tovariant(L: PLua_state; i: integer): variant;
procedure lua_pushvariant(L: PLua_state; v: variant);
procedure lua_pushrect(L: PLua_state; r: TRect);
procedure lua_pushpoint(L: PLua_state; r: TPoint);
function lua_toRect(L: PLua_State; index: integer): TRect;
function lua_toPoint(L: PLua_State; index: integer): TPoint;
function lua_toaddress(L: PLua_state; i: integer; self: boolean=false): ptruint;
procedure lua_pushcontext(L: PLua_state; context: PContext);
procedure InitializeLuaScripts(noautorun: boolean=false);
procedure InitializeLua;
procedure InitLimitedLuastate(L: Plua_State);


function LuaValueToDescription(L: PLua_state; i: integer; recursivetablecount: integer=0): string;

function GetLuaState: PLUA_State; inline;
function LuaVM: PLUA_State; inline;



function lua_oldprintoutput:TStrings;
procedure lua_setPrintOutput(output: TStrings);

function lua_synchronize(L: Plua_State): integer; cdecl;

resourcestring
  rsPluginAddress = 'Plugin Address';
  rsThisTypeIsNotSupportedHere='This type is not supported here';
  rsIncorrectNumberOfParameters='Incorrect number of parameters';


implementation

uses autoassembler, MainUnit, MainUnit2, LuaClass, frmluaengineunit, plugin, pluginexports,
  formsettingsunit, MemoryRecordUnit, debuggertypedefinitions, symbolhandler,
  symbolhandlerstructs, types,
  frmautoinjectunit, simpleaobscanner, addresslist, memscan, foundlisthelper,
  cesupport, DBK32functions, sharedMemory, disassemblerComments, disassembler,
  LuaCanvas, LuaPen, LuaFont, LuaBrush, LuaPicture, LuaMenu, LuaDebug, LuaThread,
  LuaGraphic, LuaProgressBar, LuaOldD3DHook, LuaWinControl, LuaMemoryRecord,
  LuaForm, MemoryBrowserFormUnit, disassemblerviewunit, hexviewunit,
  CustomTypeHandler, LuaStructure, LuaRegion, LuaXMPlayer, LuaMemscan, LuaFoundlist,
  LuaRadioGroup, LuaRasterImage, LuaCheatComponent, LuaAddresslist, byteinterpreter,
  OpenSave, CEDebugger, DebugHelper, StructuresFrm2, Assemblerunit, LuaObject,
  LuaComponent, LuaControl, LuaStrings, LuaStringlist, LuaCustomControl,
  LuaGraphicControl, LuaPanel, LuaImage, LuaButton, LuaCheckbox, LuaGroupbox,
  LuaListbox, LuaCombobox, LuaTrackbar, LuaListcolumn, LuaEdit, LuaMemo, LuaCollection,
  LuaListColumns, LuaListItem, LuaListItems, LuaTimer, LuaListview, LuaGenericHotkey,
  LuaTableFile, LuaMemoryRecordHotkey, LuaMemoryView, LuaD3DHook, LuaDisassembler,
  LuaDissectCode, LuaByteTable, LuaBinary, lua_server, HotkeyHandler, LuaPipeClient,
  LuaPipeServer, LuaTreeview, LuaTreeNodes, LuaTreeNode, LuaCalendar, LuaSymbolListHandler,
  LuaCommonDialog, LuaFindDialog, LuaSettings, LuaPageControl, LuaStructureFrm,
  LuaInternet, SymbolListHandler, ProcessHandlerUnit, processlist,
  DebuggerInterface, WindowsDebugger, VEHDebugger, KernelDebuggerInterface,
  DebuggerInterfaceAPIWrapper, Globals, math, speedhack2, CETranslator, binutils,
  xinput, winsapi, frmExeTrainerGeneratorUnit, CustomBase85, FileUtil, networkConfig,
  LuaCustomType, Filehandler, LuaSQL, frmSelectionlistunit, cpuidUnit, LuaRemoteThread,
  LuaManualModuleLoader, pointervaluelist, frmEditHistoryUnit, LuaCheckListBox,
  LuaDiagram, frmUltimap2Unit, frmcodefilterunit, BreakpointTypeDef, LuaSyntax,
  LazLogger, LuaSynedit, LuaRIPRelativeScanner, LuaCustomImageList ,ColorBox,
  rttihelper, LuaDotNetPipe, LuaRemoteExecutor, windows7taskbar, debugeventhandler,
  tcclib, dotnethost, CSharpCompiler, LuaCECustomButton, feces, process;

  {$warn 5044 off}

resourcestring
  rsLUA_DoScriptWasNotCalledRomTheMainThread = 'LUA_DoScript was not called '
    +'from the main thread';
  rsUndefinedLuaError = 'Undefined lua error';
  rsCheatengineIsBeingAFag = 'Cheatengine is being a fag';

  rsInvalidFloat = 'Invalid floating point string:%s';
  rsInvalidInt = 'Invalid integer:%s';
  rsError = 'Error:';
  rsConditionalBreakpointError = 'Conditional breakpoint error';
  rsNonMainthreadLuaError = 'Lua error in a secondary thread';
  rsMainLuaError = 'main.lua error:';
  rsMainLuaError2 = 'main.lua error';
  rsError2 = ' error:';
  rsError3 = ' error';
  rsLUAPanic = 'LUA panic!';
  rsDebugsetBreakpointNeedsAtLeastAnAddress = 'debug_setBreakpoint needs at least an address';
  rsCreateMemScanNeedsAProgressbarOrNil = 'createMemScan needs a progressbar or nil. ';
  rsIsNotAProgressbar = ' is not a progressbar';
  rsDeallocateSharedMemoryIsNotImplemented = 'deallocateSharedMemory is not implemented (It''s not even in the list of available functions)';
  rsGetProcessListTheProvidedListObjectIsNotValid = 'getProcessList: the provided List object is not valid';
  rsGetThreadlistTheProvidedListObjectIsNotValid = 'getThreadlist: the provided List object is not valid';
  rsPlaySoundTheParameterMustBeATableFileOrAMemoryStream = 'playSound: The parameter must be a table file or a memory stream. Nothing else';
  rsNumberRequired = 'Number required';
  rsScriptCorruptedVar = '%s has corrupted the global %s variable. Many scripts will fail to load now';

var
  printoutput: TStrings;

  waitforsymbols: boolean=true;

  autorunpath: string;




function lua_oldprintoutput:TStrings;
begin
  result:=printoutput;
end;

procedure lua_setPrintOutput(output: TStrings);
begin
  printoutput:=output;
end;


function GetLuaState: PLUA_State; inline;
begin
  if Thread_LuaVM=nil then
  begin
    if (_luacs<>nil) and (_luavm<>nil) then
    begin
      _luacs.Enter;
      try
        if _luavm<>nil then
        begin
          Thread_LuaVM:=lua_newthread(_luavm);
          Thread_LuaRef:=luaL_ref(_luavm, LUA_REGISTRYINDEX);
        end;
      finally
        _luacs.leave;
      end;
    end;
  end;

  result:=Thread_LuaVM;
end;

function LuaVM: PLUA_State; inline;
begin
  result:=GetLuaState;
end;



procedure lua_register(L: Plua_State; const n: PChar; f: lua_CFunction);
//overriding the original lua_register to add both a lower and uppercase start
var s: string;
begin
  if luasyntaxStringHashList<>nil then
    luasyntaxStringHashList.Add(n);

  lua.lua_register(L, n, f);
  s:=n;
  s[1]:=chr(ord(s[1]) xor $20); //switch from uppercase to lowercase and lowercase to uppercase

  lua.lua_register(L, pchar(s), f);

  if luasyntaxStringHashList<>nil then
    luasyntaxStringHashList.Add(s);
end;

var luarefcs: TCriticalSection;
function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl;
begin
  luarefcs.Enter;
  result:=lauxlib.luaL_ref(l, t);
  luarefcs.leave;
end;

procedure luaL_unref(L: Plua_State; t, ref: Integer); cdecl;
begin
  luarefcs.enter;
  lauxlib.luaL_unref(l, t, ref);
  luarefcs.leave;
end;

//todo: let the user define a default error function
function lua_pcall(L: Plua_State; nargs, nresults, errf: Integer): Integer; cdecl;
var
  error: string;
  usesluaengineform: boolean;
begin
  try
    if lua_isfunction(L, (-nargs)-1)=false then
    begin
      lua_pop(L,nargs+1);

      outputdebugstring(pchar('lua_pcall with invalid parameter'));
      //MessageBoxA(0, pchar('lua_pcall with invalid parameter'), pchar('Lua: Not a function'), MB_OK);
      exit(LUA_ERRRUN);
    end;

    result:=lua.lua_pcall(L, nargs, nresults, errf);
  except
    on e: exception do
    begin
      lua_pop(L, lua_gettop(L));
      result:=LUA_ERRRUN;
      lua_pushstring(l, e.Message);

      if (GetCurrentThreadId=MainThreadID) and (e.Message='Access violation') and mainform.miEnableLCLDebug.checked then
      begin
        DebugLn('Lua Exception: '+e.Message);
        lazlogger.DumpExceptionBackTrace;
      end;
    end;
  end;

  if (result=LUA_ERRRUN) and (errf=0) then //an error occured and no error handler was specified
  begin
    if GetCurrentThreadId=MainThreadID then
    begin


      //lua_Debug
      error:=Lua_ToString(l, -1);
      if (error<>'') then
      begin
        usesluaengineform:=false;
        if printoutput=nil then
        begin
          if frmLuaEngine=nil then
            frmLuaEngine:=TfrmLuaEngine.Create(application);

          printoutput:=frmLuaEngine.mOutput.Lines;
          usesluaengineform:=true;
        end;

        printoutput.add(rsError+error);

        if (frmLuaEngine<>nil) and usesluaengineform and (frmLuaEngine.cbShowOnPrint.checked) then
          frmLuaEngine.show;

        if usesluaengineform then
          printoutput:=nil;

        lua_pop(L, lua_gettop(L));
      end;
    end
    else
    begin
      //MessageBoxA(0, pchar(Lua_ToString(l, -1)), pchar(rsNonMainthreadLuaError), MB_OK);
    end;
  end;
end;

function lua_dostring(L: Plua_State; const str: PChar): Integer;
begin
  Result := luaL_loadstring(L, str);

  if Result = 0 then
    Result := lua_pcall(L, 0, LUA_MULTRET, 0);
end;

function lua_isstring(L: PLua_state; i: integer): boolean;
begin
  result := lua_type(L,i)=LUA_TSTRING;
end;

function Lua_ToString(L: Plua_State; i: Integer): string;
var r: pchar;
begin
  if lua_islightuserdata(L, i) then
    Result := inttohex(ptruint(lua_touserdata(L, i)),1)
  else
  begin
    r := lua.lua_tostring(l,i);
    if r<>nil then
      result:=r
    else
      result:='';
  end;


//unclear should there be a result:=Utf8ToAnsi(s); ?
end;

function lua_toRect(L: PLua_State; index: integer): TRect;
var i: integer;
begin
  ZeroMemory(@result,sizeof(trect));

  i:=lua_absindex(L,index);

  if lua_istable(L, i) then
  begin
    lua_pushstring(L,'Left');
    lua_gettable(L, i);
    result.Left:=lua_tointeger(L,-1);
    lua_pop(L,1);

    lua_pushstring(L,'Top');
    lua_gettable(L, i);
    result.Top:=lua_tointeger(L,-1);
    lua_pop(L,1);

    lua_pushstring(L,'Right');
    lua_gettable(L, i);
    result.Right:=lua_tointeger(L,-1);
    lua_pop(L,1);

    lua_pushstring(L,'Bottom');
    lua_gettable(L, i);
    result.Bottom:=lua_tointeger(L,-1);
    lua_pop(L,1);
  end;

end;

procedure lua_pushrect(L: PLua_state; r: TRect);
begin
  lua_createtable(L, 0,4);
  lua_pushstring(L, 'Left');
  lua_pushinteger(L, r.left);
  lua_settable(L, -3);

  lua_pushstring(L, 'Top');
  lua_pushinteger(L, r.top);
  lua_settable(L, -3);

  lua_pushstring(L, 'Right');
  lua_pushinteger(L, r.right);
  lua_settable(L, -3);

  lua_pushstring(L, 'Bottom');
  lua_pushinteger(L, r.bottom);
  lua_settable(L, -3);
end;


function lua_toPoint(L: PLua_State; index: integer): TPoint;
var i: integer;
begin
  result.x:=0;
  result.y:=0;

  i:=lua_absindex(L,index);
  if lua_istable(L, i) then
  begin
    lua_pushstring(L,'x');
    lua_gettable(L,i);

    if lua_isnil(L,-1) then
    begin
      lua_pop(L,1);
      lua_pushinteger(L,1);
      lua_gettable(L,i);
    end;

    result.x:=lua_tointeger(L,-1);
    lua_pop(L,1);

    lua_pushstring(L,'y');
    lua_gettable(L,i);
    if lua_isnil(L,-1) then
    begin
      lua_pop(L,1);
      lua_pushinteger(L,1);
      lua_gettable(L,i);
    end;

    result.y:=lua_tointeger(L,-1);
    lua_pop(L,1);
  end;
end;

procedure lua_pushpoint(L: PLua_state; r: TPoint);
begin
  lua_createtable(L, 0,2);
  lua_pushstring(L,'x');
  lua_pushinteger(L,r.x);
  lua_settable(L, -3);

  lua_pushstring(L,'y');
  lua_pushinteger(L,r.y);
  lua_settable(L, -3);
end;

procedure lua_pushvariant(L: PLua_state; v: variant);
begin
  case (tvardata(v).vtype and vartypemask) of
    varempty: lua_pushnil(L);
    varnull: lua_pushnil(L);
    varsmallint, varinteger, vardecimal, varshortint, varbyte, varword, varlongword, varint64, varqword: lua_pushinteger(L, v);
    varsingle, vardouble: lua_pushnumber(L, v);
    varboolean: lua_pushboolean(L, v);
    varstring, varustring: lua_pushstring(L, v);
    else
      lua_pushstring(L, v);
  end;
end;

function lua_tovariant(L: PLua_state; i: integer): variant;
begin
  case lua_type(L, i) of
    LUA_TNONE          : result:=nil;
    LUA_TNIL           : result:=nil;
    LUA_TBOOLEAN       : result:=lua_toboolean(L, i);
    LUA_TNUMBER        : result:=lua_tonumber(L, i);
    LUA_TSTRING        : result:=Lua_ToString(L, i);
    else
      raise exception.create(rsThisTypeIsNotSupportedHere);

  end;
end;

function lua_ToCEUserData(L: PLua_state; i: integer): pointer;
//Cheat Engine implements two types of userdata. the legacy LightUserData used in 6.2- and the Heavy UserData in 6.3+
//Heavy UserData is a pointer with a pointer to the real object, while lightuserdata is just a pointer to the object
begin
  result:=lua_touserdata(L,i);

  if (result<>nil) and (lua_isheavyuserdata(L, i)) then   //once the conversion is done this if check if it's userdata can go as it will always be userdata
    result:=ppointer(result)^;
end;


function lua_toaddress(L: PLua_state; i: integer; self: boolean=false): ptruint; inline;
begin
  if lua_type(L,i)=LUA_TSTRING then
  begin
    if self then
      result:=selfsymhandler.getAddressFromNameL(lua_tostring(L,i),waitforsymbols)
    else
      result:=symhandler.getAddressFromNameL(lua_tostring(L,i),waitforsymbols)
  end
  else
    result:=lua_tointeger(L,i);
end;

function LuaValueToDescription(L: PLua_state; i: integer; recursivetablecount: integer=0): string;
var index, count: integer;
  fieldname: string;
  valuedesc: string;
  o: tobject;

  tablepad: string;

  stackstart: integer;
begin
  result:='';


  if not lua_isnil(L, i) then
  begin
    if lua_isuserdata(L, i) then
    begin
      stackstart:=lua_gettop(L);

      o:=lua_ToCEUserData(L, i);

      try
        if o is TObject then
        begin
          result:='Object of type '+o.ClassName;
          if o is TControl then
            result:=result+#13#10+tcontrol(o).Name;
        end;
      except
      end;

      index:=lua_gettop(l);
      lua_settop(l, stackstart);
    end
    else
    if lua_iscfunction(L, i) then
      result:='native function'
    else
    if lua_isfunction(L, i) then
      result:='function'
    else
    if lua_istable(L, i) then
    begin
      result:='table';

      if recursivetablecount<MAXTABLERECURSIONLOOKUP then
      begin
        tablepad:=DupeString('   ',recursivetablecount);

        stackstart:=lua_gettop(l);


        result:=result+#13#10+tablepad+'['+#13#10;;
        count:=10;
        lua_pushvalue(L, i);
        index:=lua_gettop(L);
        lua_pushnil(L);  //first key (nil)
        while lua_next(L, index)<>0 do
        begin
          count:=count-1;
          if count<0 then
          begin

            result:=result+tablepad+'...'+#13#10;;
            break;
          end;

          if lua_type(L,-2)=LUA_TSTRING then
            fieldname:=Lua_ToString(L, -2)
          else
            fieldname:=inttostr(lua_tointeger(L, -2));


          valuedesc:=LuaValueToDescription(L, -1, recursivetablecount+1);

          result:=result+tablepad+'   '+fieldname+' = '+valuedesc+#13#10;


          lua_pop(L, 1); //pop the value, keep the key
        end;

//        lua_pop(L,1); //pop the pushvalue from before



        lua_settop(l, stackstart);

        result:=result+tablepad+']';
      end;
    end
    else
    if lua_isboolean(L, i) then
      result:=BoolToStr(lua_toboolean(L,i),'true','false')
    else
      result:=Lua_ToString(L, i);
  end
  else
    result:='nil';
end;

procedure InitializeLuaScripts(noautorun: boolean=false);
var f: string;
  i,r: integer;
  pc: pchar;
  DirInfo: TSearchRec;
  mainformwasset: boolean=true;
  addresslistwasset: boolean=true;


begin
  lua_getglobal(LuaVM,'MainForm');
  if lua_isnil(LuaVM,-1) then
  begin
    MessageDlg('MainForm is undefined. Invalid CE Build', mtError, [mbok],0);
    mainformwasset:=false;
  end;
  lua_pop(LuaVM,1);

  lua_getglobal(LuaVM,'AddressList');
  if lua_isnil(LuaVM,-1) then
  begin
    MessageDlg('AddressList is undefined. Invalid CE Build', mtError, [mbok],0);
    addresslistwasset:=false;
  end;
  lua_pop(LuaVM,1);


  f:='main.lua';
  {$ifdef darwin}
  f:=extractfiledir(extractfiledir(Application.ExeName))+'/Lua/main.lua';
  {$endif}
  if not FileExists(f) then //perhaps in the cedir
  begin
    f:=CheatEngineDir+'main.lua';
    if not FileExists(f) then
    begin
      //try the defines only then
      f:='defines.lua';
      if not FileExists(f) then
      begin
        f:=CheatEngineDir+'defines.lua';
        if not FileExists(f) then
          exit;
      end;
    end;
  end;

  //file exists


  try
    if lua_dofile(luavm, pchar(f))<>0 then
    begin
      i:=lua_gettop(luavm);
      if i>0 then
      begin
        pc:=lua_tolstring(luavm, -1,nil);
        if pc<>nil then
          showmessage(rsMainLuaError+pc)
        else
          showmessage(rsMainLuaError2);
      end
      else showmessage(rsMainLuaError2);

    end;


  finally
    lua_pop(LuaVM, lua_gettop(luavm)); //reset stack
  end;

  //autorun folder

  if noautorun=false then
  begin


    ZeroMemory(@DirInfo,sizeof(TSearchRec));
    r := FindFirst(autorunpath+'*.lua', FaAnyfile, DirInfo);

    while (r = 0) do
    begin
      if (DirInfo.Attr and FaVolumeId <> FaVolumeID) then
      begin
        if ((DirInfo.Attr and FaDirectory) <> FaDirectory) then
        begin
          i:=lua_dofile(luavm, pchar( UTF8ToWinCP(autorunpath+DirInfo.name)));
          if i<>0 then //error
          begin
            i:=lua_gettop(luavm);
            if i>0 then
            begin
              pc:=lua_tolstring(luavm, -1,nil);
              if pc<>nil then
                showmessage(DirInfo.name+rsError2+pc)
              else
                showmessage(DirInfo.name+rsError3);
            end
            else showmessage(DirInfo.name+rsError3);
          end;

          //reset stack
          lua_pop(LuaVM, lua_gettop(luavm));

          if mainformwasset then
          begin
            lua_getglobal(LuaVM,'MainForm');
            if lua_isnil(LuaVM,-1) then
            begin
              MessageDlg(format(rsScriptCorruptedVar, [autorunpath+DirInfo.name, 'MainForm']), mtError,[mbOK],0);
              mainformwasset:=false;
            end;
            lua_pop(LuaVM,1);
          end;

          if addresslistwasset then
          begin
            lua_getglobal(LuaVM,'AddressList');
            if lua_isnil(LuaVM,-1) then
            begin
              MessageDlg(format(rsScriptCorruptedVar, [autorunpath+DirInfo.name, 'AddressList']), mtError,[mbOK],0);
              addresslistwasset:=false;
            end;
            lua_pop(LuaVM,1);
          end;

        end;
      end;
      r := FindNext(DirInfo);
    end;
    FindClose(DirInfo);
  end;



  if translationfilepath<>'' then
  begin
    if FileExists(translationfilepath+'init.lua') then
    begin
      lua_dofile(luavm, pchar(translationfilepath+'init.lua'));
      lua_pop(LuaVM, lua_gettop(luavm));

      if mainformwasset then
      begin
        lua_getglobal(LuaVM,'MainForm');
        if lua_isnil(LuaVM,-1) then
        begin
          MessageDlg(format(rsScriptCorruptedVar, [translationfilepath+'init.lua', 'MainForm']), mtError,[mbOK],0);
          mainformwasset:=false;
        end;
        lua_pop(LuaVM,1);
      end;

      if addresslistwasset then
      begin
        lua_getglobal(LuaVM,'AddressList');
        if lua_isnil(LuaVM,-1) then
        begin
          MessageDlg(format(rsScriptCorruptedVar, [translationfilepath+'init.lua', 'AddressList']), mtError,[mbOK],0);
          addresslistwasset:=false;
        end;
        lua_pop(LuaVM,1);
      end;
    end;
  end;

  lua_settop(LuaVM,0);
end;

function lua_strtofloat(s: string): double;
var stackpos: integer;
  s2: integer;
begin

  try
    stackpos:=lua_gettop(luavm);
    if lua_dostring(luavm, pchar('return '+s) )=0 then
    begin
      s2:=lua_gettop(luavm);
      if (s2-stackpos)>0 then
        result:=lua_tonumber(luavm, stackpos-s2)
      else
        raise exception.create(Format(rsInvalidFloat, [s]));
    end
    else
      raise exception.create(Format(rsInvalidFloat, [s]));

  finally
    lua_settop(luavm, stackpos);
  end;
end;

function lua_strtoint(s: string): integer;
var stackpos: integer;
  s2: integer;
begin
  try
    stackpos:=lua_gettop(luavm);
    if lua_dostring(luavm, pchar('return '+s) )=0 then
    begin
      s2:=lua_gettop(luavm);
      if (s2-stackpos)>0 then
      begin
        if lua_isnil(luavm,-1)=false then
          result:=lua_tointeger(luavm, stackpos-s2)
        else
          raise exception.create(rsInvalidInt);
      end
      else
        raise exception.create(Format(rsInvalidInt, [s]));
    end
    else
      raise exception.create(Format(rsInvalidInt, [s]));

  finally
    lua_settop(luavm, stackpos);
  end;

end;

procedure Lua_RegisterObject(name: string; o: TObject);
var s: integer;
begin
  s:=lua_gettop(LuaVM);
  luaclass_newClass(LuaVM, o);
  lua_setglobal(LuaVM, pchar(name));

  lua_settop(LuaVM, s);
end;

function LUA_onBreakpoint(threadid: dword; context: PContext; functionAlreadyPushed: boolean=false): boolean;
var p: integer;
begin
  result:=false;
  if context=nil then exit;

  try
    try
      LUA_SetCurrentContextState(threadid, context);

      if not functionAlreadyPushed then
      begin
        lua_pop(LuaVM, lua_gettop(luavm)); //clear it just to be sure

        lua_getglobal(luavm, pchar('debugger_onBreakpoint'));
        p:=lua_gettop(luavm);
        if p=0 then exit;
      end;

      if lua_isfunction(luavm, -1) then //extra check
      begin
        if lua_pcall(LuaVM, 0, 1, 0)=0 then
        begin
          p:=lua_gettop(luavm);

          if (p=1) then //only 1 parameter returned
            result:=lua_tointeger(luavm, -1)<>0;  //return the result is not 0


          lua_pop(LuaVM, lua_gettop(luavm)); //clear stack

          //set new state if changes where made
          LUA_GetNewContextState(context);
        end;
      end;
    except
    end;
  finally
    lua_pop(LuaVM, lua_gettop(luavm));
  end;
end;

procedure lua_pushcontext(L: PLua_state; context: PContext);
var
  t: integer;
  i: integer;
begin
  lua_newtable(L);
  t:=lua_gettop(L);

  lua_pushstring(L, 'ContextFlags');
  lua_pushinteger(L,context^.ContextFlags);
  lua_settable(L,t);

  lua_pushstring(L, 'CS');
  lua_pushinteger(L,context^.SegCs);
  lua_settable(L,t);

  lua_pushstring(L, 'DS');
  lua_pushinteger(L,context^.SegDs);
  lua_settable(L,t);

  lua_pushstring(L, 'ES');
  lua_pushinteger(L,context^.SegEs);
  lua_settable(L,t);

  lua_pushstring(L, 'FS');
  lua_pushinteger(L,context^.SegFs);
  lua_settable(L,t);

  lua_pushstring(L, 'GS');
  lua_pushinteger(L,context^.SegGs);
  lua_settable(L,t);

  lua_pushstring(L, 'SS');
  lua_pushinteger(L,context^.SegSs);
  lua_settable(L,t);

  lua_pushstring(L, 'EFlags');
  lua_pushinteger(L,context^.EFlags);
  lua_settable(L,t);

  {$ifdef cpu64}
  lua_pushstring(L, 'RAX');
  lua_pushinteger(L,context^.Rax);
  lua_settable(L,t);

  lua_pushstring(L, 'RBX');
  lua_pushinteger(L,context^.Rbx);
  lua_settable(L,t);

  lua_pushstring(L, 'RCX');
  lua_pushinteger(L,context^.Rcx);
  lua_settable(L,t);

  lua_pushstring(L, 'RDX');
  lua_pushinteger(L,context^.Rdx);
  lua_settable(L,t);

  lua_pushstring(L, 'RSI');
  lua_pushinteger(L,context^.Rsi);
  lua_settable(L,t);

  lua_pushstring(L, 'RDI');
  lua_pushinteger(L,context^.Rdi);
  lua_settable(L,t);

  lua_pushstring(L, 'RBP');
  lua_pushinteger(L,context^.Rbp);
  lua_settable(L,t);

  lua_pushstring(L, 'RSP');
  lua_pushinteger(L,context^.Rsp);
  lua_settable(L,t);

  lua_pushstring(L, 'RIP');
  lua_pushinteger(L,context^.Rip);
  lua_settable(L,t);

  lua_pushstring(L, 'R8');
  lua_pushinteger(L,context^.R8);
  lua_settable(L,t);

  lua_pushstring(L, 'R9');
  lua_pushinteger(L,context^.R9);
  lua_settable(L,t);

  lua_pushstring(L, 'R10');
  lua_pushinteger(L,context^.R10);
  lua_settable(L,t);

  lua_pushstring(L, 'R11');
  lua_pushinteger(L,context^.R11);
  lua_settable(L,t);

  lua_pushstring(L, 'R12');
  lua_pushinteger(L,context^.R12);
  lua_settable(L,t);

  lua_pushstring(L, 'R13');
  lua_pushinteger(L,context^.R13);
  lua_settable(L,t);

  lua_pushstring(L, 'R14');
  lua_pushinteger(L,context^.R14);
  lua_settable(L,t);

  lua_pushstring(L, 'R15');
  lua_pushinteger(L,context^.R15);
  lua_settable(L,t);
  {$endif}

  if processhandler.is64Bit=false then
  begin
    lua_pushstring(L, 'EAX');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rax{$else}eax{$endif} and $ffffffff);
    lua_settable(L,t);

    lua_pushstring(L, 'EBX');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rbx{$else}ebx{$endif} and $ffffffff);
    lua_settable(L,t);

    lua_pushstring(L, 'ECX');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rcx{$else}ecx{$endif} and $ffffffff);
    lua_settable(L,t);

    lua_pushstring(L, 'EDX');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rdx{$else}edx{$endif} and $ffffffff);
    lua_settable(L,t);

    lua_pushstring(L, 'ESI');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rsi{$else}esi{$endif} and $ffffffff);
    lua_settable(L,t);

    lua_pushstring(L, 'EDI');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rdi{$else}edi{$endif} and $ffffffff);
    lua_settable(L,t);

    lua_pushstring(L, 'EBP');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rbp{$else}ebp{$endif} and $ffffffff);
    lua_settable(L,t);

    lua_pushstring(L, 'ESP');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rsp{$else}esp{$endif} and $ffffffff);
    lua_settable(L,t);

    lua_pushstring(L, 'EIP');
    lua_pushinteger(L,context^.{$ifdef cpu64}Rip{$else}eip{$endif} and $ffffffff);
    lua_settable(L,t);
  end;

  lua_pushstring(L, 'DR0');
  lua_pushinteger(L,context^.DR0);
  lua_settable(L,t);

  lua_pushstring(L, 'DR1');
  lua_pushinteger(L,context^.DR1);
  lua_settable(L,t);

  lua_pushstring(L, 'DR2');
  lua_pushinteger(L,context^.DR2);
  lua_settable(L,t);

  lua_pushstring(L, 'DR3');
  lua_pushinteger(L,context^.DR3);
  lua_settable(L,t);

  lua_pushstring(L, 'DR6');
  lua_pushinteger(L,context^.DR6);
  lua_settable(L,t);

  lua_pushstring(L, 'DR7');
  lua_pushinteger(L,context^.DR7);
  lua_settable(L,t);


  for i:=0 to 7 do
  begin
    lua_pushstring(L,'FP'+inttostr(i));
    {$ifdef cpu32}
    CreateByteTableFromPointer(L, @context^.FloatSave.RegisterArea[10*i], 10);
    {$else}
    CreateByteTableFromPointer(L, @context^.FltSave.FloatRegisters[i], 10);
    {$endif}
    lua_settable(L,t);
  end;

  //xmm regs

  for i:=0 to 15 do
  begin
    if (i>=8) and (not processhandler.is64Bit) then break;

    lua_pushstring(L,'XMM'+inttostr(i));

    {$ifdef cpu32}
    CreateByteTableFromPointer(luavm, @context^.ext.XMMRegisters[i], 16);
    {$else}
    CreateByteTableFromPointer(luavm, @context^.FltSave.XmmRegisters[i], 16);
    {$endif}
    lua_settable(L,t);
  end;

end;

procedure LUA_SetCurrentContextState(tid: dword; context: PContext; extraregs: boolean=false);
var i: integer;
begin
  lua_pushinteger(luavm, tid);
  lua_setglobal(luavm, 'THREADID');

  if processhandler.SystemArchitecture=archX86 then
  begin
    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rax{$else}eax{$endif});
    lua_setglobal(luavm, 'RAX');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rax{$else}eax{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EAX');

    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rbx{$else}ebx{$endif});
    lua_setglobal(luavm, 'RBX');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rbx{$else}ebx{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EBX');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rcx{$else}ecx{$endif});
    lua_setglobal(luavm, 'RCX');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rcx{$else}ecx{$endif} and $ffffffff);
    lua_setglobal(luavm, 'ECX');

    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rdx{$else}edx{$endif});
    lua_setglobal(luavm, 'RDX');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rdx{$else}edx{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EDX');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rsi{$else}esi{$endif});
    lua_setglobal(luavm, 'RSI');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rsi{$else}esi{$endif} and $ffffffff);
    lua_setglobal(luavm, 'ESI');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rdi{$else}edi{$endif});
    lua_setglobal(luavm, 'RDI');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rdi{$else}edi{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EDI');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}Rbp{$else}ebp{$endif});
    lua_setglobal(luavm, 'RBP');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}RBP{$else}eBP{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EBP');


    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}RSP{$else}eSP{$endif});
    lua_setglobal(luavm, 'RSP');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}RSP{$else}eSP{$endif} and $ffffffff);
    lua_setglobal(luavm, 'ESP');

    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}RIP{$else}eIP{$endif});
    lua_setglobal(luavm, 'RIP');
    {$endif}
    lua_pushinteger(luavm, context^.{$ifdef cpu64}RIP{$else}eIP{$endif} and $ffffffff);
    lua_setglobal(luavm, 'EIP');

    lua_pushinteger(luavm, context^.EFlags);
    lua_setglobal(luavm, 'EFLAGS');



    {$ifdef cpu64}
    lua_pushinteger(luavm, context^.r8);
    lua_setglobal(luavm, 'R8');

    lua_pushinteger(luavm, context^.r9);
    lua_setglobal(luavm, 'R9');

    lua_pushinteger(luavm, context^.r10);
    lua_setglobal(luavm, 'R10');

    lua_pushinteger(luavm, context^.r11);
    lua_setglobal(luavm, 'R11');

    lua_pushinteger(luavm, context^.r12);
    lua_setglobal(luavm, 'R12');

    lua_pushinteger(luavm, context^.r13);
    lua_setglobal(luavm, 'R13');

    lua_pushinteger(luavm, context^.r14);
    lua_setglobal(luavm, 'R14');

    lua_pushinteger(luavm, context^.r15);
    lua_setglobal(luavm, 'R15');
    {$endif}

    if extraregs then //default off as it's a bit slow
    begin
      for i:=0 to 7 do
      begin
        {$ifdef cpu32}
        CreateByteTableFromPointer(luavm, @context^.FloatSave.RegisterArea[10*i], 10);
        {$else}
        CreateByteTableFromPointer(luavm, @context^.FltSave.FloatRegisters[i], 10);
        {$endif}
        lua_setglobal(luavm, pchar('FP'+inttostr(i)));
      end;

      //xmm regs

      for i:=0 to 15 do
      begin
        if (i>=8) and (not processhandler.is64Bit) then break;

        {$ifdef cpu32}
        CreateByteTableFromPointer(luavm, @context^.ext.XMMRegisters[i], 16);
        {$else}
        CreateByteTableFromPointer(luavm, @context^.FltSave.XmmRegisters[i], 16);
        {$endif}
        lua_setglobal(luavm, pchar('XMM'+inttostr(i)));
      end;
    end;
  end
  else
  begin
    //todo: ARM
  end;
end;

procedure LUA_GetNewContextState(context: PContext; extraregs: boolean=false);
var
  i: integer;
  t: integer;
begin
  lua_getglobal(luavm, 'EFLAGS');
  context.EFLAGS:=lua_tointeger(luavm, -1);
  lua_pop(luavm,1);

  if not processhandler.is64bit then
  begin
    lua_getglobal(luavm, 'EAX');
    context.{$ifdef cpu64}rax{$else}eax{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'EBX');
    context.{$ifdef cpu64}rbx{$else}ebx{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'ECX');
    context.{$ifdef cpu64}rcx{$else}ecx{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'EDX');
    context.{$ifdef cpu64}rdx{$else}edx{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'ESI');
    context.{$ifdef cpu64}rsi{$else}esi{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'EDI');
    context.{$ifdef cpu64}rdi{$else}edi{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'EBP');
    context.{$ifdef cpu64}rbp{$else}ebp{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'ESP');
    context.{$ifdef cpu64}rsp{$else}esp{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);


    lua_getglobal(luavm, 'EIP');
    context.{$ifdef cpu64}rip{$else}eip{$endif}:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

  end
  else
  begin
  {$ifdef cpu64}
    lua_getglobal(luavm, 'RAX');
    context.RAX:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'RBX');
    context.RBX:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'RCX');
    context.RCX:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'RDX');
    context.RDX:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'RSI');
    context.RSI:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'RDI');
    context.RDI:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'RBP');
    context.RBP:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'RSP');
    context.RSP:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'RIP');
    context.RIP:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'R8');
    context.R8:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'R9');
    context.R9:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'R10');
    context.R10:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'R11');
    context.R11:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'R12');
    context.R12:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'R13');
    context.R13:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'R14');
    context.R14:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);

    lua_getglobal(luavm, 'R15');
    context.R15:=lua_tointeger(luavm, -1);
    lua_pop(luavm,1);
  {$endif}
  end;

  if extraregs then
  begin
    for i:=0 to 7 do
    begin
      lua_getglobal(luavm, pchar('FP'+inttostr(i)));
      if not lua_isnil(luavm, -1) then
      begin
        t:=lua_gettop(LuaVM);
        {$ifdef cpu32}
        readBytesFromTable(luavm, t, @context.FloatSave.RegisterArea[10*i], 10);
        {$else}
        readBytesFromTable(luavm, t, @context.FltSave.FloatRegisters[i], 10);
        {$endif}
      end;
      lua_pop(luavm,1);
    end;

    for i:=0 to 15 do
    begin
      if (i>=8) and (not processhandler.is64Bit) then break;

      lua_getglobal(luavm, pchar('XMM'+inttostr(i)));
      if not lua_isnil(luavm, -1) then
      begin
        t:=lua_gettop(LuaVM);
        {$ifdef cpu32}
        readBytesFromTable(luavm, t, @context.ext.XMMRegisters[i], 16);
        {$else}
        readBytesFromTable(luavm, t, @context.FltSave.XmmRegisters[i], 16);
        {$endif}
      end;

    end;
  end;
end;

procedure LUA_DoScript(s: string);
var i: integer;
  pc: pchar;

  stack: integer;
begin
  //if GetCurrentThreadId<>MainThreadID then raise exception.create(rsLUA_DoScriptWasNotCalledRomTheMainThread);

  stack:=lua_gettop(luavm);
  try

    i:=lua_dostring(luavm, pchar(s));
    if i<>0 then
    begin
      pc:=lua.lua_tostring(luavm, -1);
      if pc<>nil then
        raise Exception.Create(pc)
      else
        raise exception.create(rsUndefinedLuaError);

    end;
  finally
    lua_settop(luavm, stack);
  end;
end;

procedure LUA_onNotify(functionid: integer; sender: tobject);
begin
  try
    lua_rawgeti(Luavm, LUA_REGISTRYINDEX, functionid);

    lua_pushlightuserdata(Luavm, sender);
    lua_pcall(Luavm, 1, 0, 0);
  finally
    lua_pop(luavm, lua_gettop(luavm)); //clear the stack
  end;
end;

procedure LUA_memrec_callback(memrec: pointer; routine: string);
var m: TMemoryrecord;
  p: integer;
begin
  try
    m:=memrec;

    lua_getglobal(luavm, pchar(routine));

    p:=lua_gettop(luavm);
    if p<>0 then
    begin
      if lua_isfunction(luavm, -1) then
      begin
        lua_pushlightuserdata(luavm, memrec);
        lua_pcall(luavm, 1, 0, 0);
      end;


    end;
  finally
    lua_pop(luavm,lua_gettop(luavm));
  end;
end;

procedure lua_setbasictableentry(L: Plua_State; tableindex: integer; entryname: string; data: Variant);
var x: integer;
begin
  lua_pushstring(L, entryname);

  x:=vartype(data) and vartypemask;
  case x of
    varsmallint, varinteger, varint64, varqword, varlongword, varword, varbyte, varshortint: lua_pushinteger(L, data);
    varsingle, vardouble: lua_pushnumber(L, data);
    varboolean: lua_pushboolean(L, data);
    varstring,varustring,varolestr: lua_pushstring(L, data);

  end;

  lua_settable(L, tableindex);
end;

function LUA_functioncall(routinetocall: string; parameters: array of const): integer;
var i,e: integer;
  c: string;
  p: integer;
  oldstack: integer;
  l: Plua_State;
begin
 // OutputDebugString(inttohex(qword(GetCurrentThreadId),1)+':LUA_functioncall calling '+routinetocall);
 { if GetCurrentThreadId<>MainThreadID then
  begin
    OutputDebugString('Not main thread');
    l:=lua_newthread(luavm);
  end
  else  }
    l:=luavm;


  result:=-1;
  oldstack:=lua_gettop(l);

 // OutputDebugString('LUA_functioncall: oldstack='+inttostr(oldstack));

 // if luacs.TryEnter then
  begin
    try
      //check if the routine exists
    //  OutputDebugString('LUA_functioncall: calling getglobal');

      lua_getglobal(l, pchar(routinetocall));

     // OutputDebugString('LUA_functioncall: after getglobal');

      p:=lua_gettop(l);
     // OutputDebugString('LUA_functioncall: newstack='+inttostr(p));

      if p<>oldstack then
      begin
        if lua_isfunction(l, -1) then
        begin
          //OutputDebugString('LUA_functioncall: function exists');
          //OutputDebugString('LUA_functioncall: length(parameters)='+inttostr(length(parameters)));

          //routine exists, fill in the parameters
          for i:=0 to length(parameters)-1 do
          begin
            case parameters[i].VType of
              system.vtInteger : lua_pushinteger(L, parameters[i].VInteger);
              system.vtBoolean: lua_pushboolean(L, parameters[i].VBoolean);
              system.vtChar:
              begin
                c:=parameters[i].VChar;
                lua_pushstring(L, c);
              end;
              system.vtExtended: lua_pushnumber(L, parameters[i].VExtended^);
              system.vtString: lua_pushstring(L, pchar(parameters[i].VString));
              system.vtPointer: lua_pushlightuserdata(L, parameters[i].VPointer);
              system.vtPChar: lua_pushstring(L, parameters[i].VPChar);
              system.vtObject: luaclass_newClass(L, parameters[i].VObject); //lua_pushlightuserdata(L, pointer(parameters[i].VObject));
              system.vtClass: lua_pushlightuserdata(L, pointer(parameters[i].VClass));
              system.vtWideChar, vtPWideChar, vtVariant, vtInterface,
                vtWideString: lua_pushstring(L, rsCheatengineIsBeingAFag);
              system.vtAnsiString: lua_pushstring(L, pchar(parameters[i].VAnsiString));
              system.vtCurrency: lua_pushnumber(L, parameters[i].VCurrency^);
              system.vtInt64:
              begin
                if (parameters[i].VInt64^<=$ffffffff) then
                  lua_pushinteger(L, parameters[i].VInt64^)
                else
                  lua_pushlightuserdata(L, pointer(parameters[i].VInt64^));
              end;
              system.vtQWord:
              begin
                if (parameters[i].VQWord^<=$ffffffff) then
                  lua_pushinteger(L, parameters[i].VQWord^)
                else
                  lua_pushlightuserdata(L, pointer(parameters[i].VQWord^));
              end;
            end;

          end;

         // OutputDebugString('Lua_functioncall: Calling lua_pcall');
          lua_pcall(L, length(parameters), 1, 0);
         // OutputDebugString('Lua_functioncall: returned from lua_pcall');
          i:=lua_gettop(L);
          if i>0 then //it has a parameter
            result:=lua_tointeger(L, -1);
        end;


      end;


    finally
     // OutputDebugString('Lua_functioncall exit');
      lua_settop(L, oldstack);
 //     luacs.leave;
    end;

  end;
end;

 {
procedure LUA_callback(routine: string; parameters: tvararray);
var m: TMemoryrecord;
  p: integer;
begin
  LuaCS.Enter;
  try
    m:=memrec;

    lua_getfield(luavm, LUA_GLOBALSINDEX, pchar(routine));

    p:=lua_gettop(luavm);
    if p<>0 then
    begin
      if lua_isfunction(luavm, -1) then
      begin
        lua_pushlightuserdata(luavm, memrec);
        lua_pcall(luavm, 1, 0, 0);
      end;


    end;
  finally
    lua_pop(luavm,lua_gettop(luavm));
    luacs.Leave;
  end;
end; }

function CheckIfConditionIsMetContext(threadid: dword; context: PContext; script: string): boolean;
{
precondition: script returns a value (so already has the 'return ' part appended for single line scripts)
}
var i: integer;
begin
  result:=false;
  try
    LUA_SetCurrentContextState(threadid, context);

    if lua_dostring(luavm, pchar(script))=0 then
    begin
      i:=lua_gettop(LuaVM);
      if i=1 then //valid return
        result:=lua_toboolean(LuaVM, -1);
    end;
  finally
    lua_pop(LuaVM, lua_gettop(luavm));
  end;
end;

function LuaPanic(L: Plua_State): Integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  raise exception.create(rsLUAPanic);
end;


function lua_sleep(L: PLua_State): integer; cdecl;
var
  parameters: integer;
begin
  parameters:=lua_gettop(L);

  result:=0;


  if parameters=1 then
    sleep(lua_tointeger(L, -1));

  lua_pop(L, parameters);
end;

function print2(param: pointer): pointer;
var
  usesluaengineform: boolean;
  l: tstringlist;
  i: integer;
begin
  if application.Terminated then exit;
  usesluaengineform:=false;

  if printoutput=nil then
  begin
    if frmLuaEngine=nil then
      frmLuaEngine:=TfrmLuaEngine.Create(MemoryBrowser);

    printoutput:=frmLuaEngine.mOutput.Lines;
    usesluaengineform:=true;
  end;

  l:=tstringlist.create;
  l.text:=pchar(param);
  for i:=0 to l.Count-1 do
    printoutput.add(l[i]);

  l.free;

  if (frmLuaEngine<>nil) and usesluaengineform and (frmLuaEngine.cbShowOnPrint.checked) then
    frmLuaEngine.show;

  if usesluaengineform then
    printoutput:=nil;

  result:=nil;
end;

function print(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  s: string;

  str: string;
  i: integer;
begin
  if MainThreadID<>GetCurrentThreadId then
  begin
    //print is threadsafe because it always syncs
    lua_pushcfunction(L,@print);
    lua_insert(L,1);
    exit(lua_synchronize(L));
  end;


  parameters:=lua_gettop(L);
  if parameters=0 then exit(0);

  str:='';
  for i:=-parameters to -1 do
  begin
    if lua_isuserdata(L,i) then
      s:=inttohex(ptruint(lua_ToCEUserData(L, i)),8)
    else
      s:=lua_tostring(L, i);

    str:=str+s+' ';
  end;

  if str<>'' then
  begin
    print2(@str[1]);
    //pluginsync(print2, @str[1]);
  end;

  lua_pop(L, parameters);
  lua_pushstring(L, str);
  result:=1;
end;

function inputQuery_lua(L: PLua_State): integer; cdecl;
var
  caption, prompt, value: string;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(l);
  if parameters=3 then
  begin
    caption:=Lua_ToString(L, 1);
    prompt:=Lua_ToString(L, 2);
    value:=Lua_ToString(L, 3);
    if InputQuery(caption, prompt, value) then
    begin
      result:=1;
      lua_pushstring(L, value);
    end;
  end;

end;

function showMessage_lua(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  s: string;
begin
  parameters:=lua_gettop(L);
  if parameters=0 then exit(0);

  if lua_islightuserdata(l,-1) then
    s:=inttohex(ptruint(lua_touserdata(L, -1)),8)
  else
    s:=lua_tostring(L, -1);

  ShowMessage(s);

//  ce_showmessage(pchar(s));

  lua_pop(L, parameters);
  result:=0;
end;

function readSmallIntegerEx(L: PLua_State; processhandle: thandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;
  signed: boolean;

  v: smallInt;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters>=1 then
    begin
      //ShowMessage(inttostr(lua_type(L, 1)));

      address:=lua_toaddress(L,1, processhandle=GetCurrentProcess);

      if parameters>=2 then
        signed:=lua_toboolean(L,2)
      else
        signed:=false;

      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        if signed then 
          lua_pushinteger(L, v)
        else
          lua_pushinteger(L, word(v));

        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readSmallIntegerLocal(L: PLua_State): integer; cdecl;
begin
  result:=readSmallIntegerEx(L, GetCurrentProcess);
end;

function readSmallInteger(L: PLua_State): integer; cdecl;
begin
  result:=readSmallIntegerEx(L, ProcessHandle);
end;

function readShortIntegerEx(L: PLua_State; processhandle: thandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;
  signed: boolean;

  v: ShortInt;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters>=1 then
    begin
      address:=lua_toaddress(L,1, processhandle=GetCurrentProcess);

      if parameters>=2 then
        signed:=lua_toboolean(L,2)
      else
        signed:=false;

      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        if signed then
          lua_pushinteger(L, v)
        else
          lua_pushinteger(L, byte(v));

        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readShortIntegerLocal(L: PLua_State): integer; cdecl;
begin
  result:=readShortIntegerEx(L, GetCurrentProcess);
end;

function readShortInteger(L: PLua_State): integer; cdecl;
begin
  result:=readShortIntegerEx(L, ProcessHandle);
end;

function readIntegerEx(L: PLua_State; processhandle: thandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;
  signed: boolean;

  v: integer;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters>=1 then
    begin
      //ShowMessage(inttostr(lua_type(L, 1)));
      address:=lua_toaddress(L,1, processhandle=GetCurrentProcess);


      if parameters>=2 then
        signed:=lua_toboolean(L,2)
      else
        signed:=false;

      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        if signed then 
          lua_pushinteger(L, v)
        else
          lua_pushinteger(L, dword(v));

        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readIntegerLocal(L: PLua_State): integer; cdecl;
begin
  result:=readIntegerEx(L, GetCurrentProcess);
end;

function readInteger(L: PLua_State): integer; cdecl;
begin
  result:=readIntegerEx(L, ProcessHandle);
end;

function readQwordEx(L: PLua_State; processhandle: thandle): integer; cdecl;
var
  parameters: Qword;
  address: ptruint;

  v: Qword;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=1 then
    begin
      //ShowMessage(inttostr(lua_type(L, -1)));

      address:=lua_toaddress(L,-1,processhandle=GetCurrentProcess);


      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        lua_pushinteger(L, v);
        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readQwordLocal(L: PLua_State): integer; cdecl;
begin
  result:=readQwordEx(L, GetCurrentProcess);
end;

function readQword(L: PLua_State): integer; cdecl;
begin
  result:=readQwordEx(L, ProcessHandle);
end;

function readPointerLocal(L: PLua_State): integer; cdecl;
begin
{$ifdef cpu64}
  result:=readQwordLocal(L);
{$else}
  result:=readIntegerLocal(L);
{$endif}
end;

function readPointer(L: PLua_State): integer; cdecl;
begin
  if processhandler.is64Bit then
    result:=readQword(L)
  else
    result:=readInteger(L);
end;

function readFloatEx(L: PLua_State; ProcessHandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: single;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=1 then
    begin
      address:=lua_toaddress(L,-1,processhandle=GetCurrentProcess);

      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        lua_pushnumber(L, v);
        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readFloatLocal(L: PLua_State): integer; cdecl;
begin
  result:=readFloatEx(L, GetCurrentProcess);
end;

function readFloat(L: PLua_State): integer; cdecl;
begin
  result:=readFloatEx(L, ProcessHandle);
end;

function readDoubleEx(L: PLua_State; ProcessHandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: double;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=1 then
    begin
      address:=lua_toaddress(L,-1,processhandle=GetCurrentProcess);

      lua_pop(L, parameters);

      v:=0;
      if ReadProcessMemory(processhandle, pointer(address), @v, sizeof(v), r) then
      begin
        lua_pushnumber(L, v);
        result:=1;
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readDoubleLocal(L: PLua_State): integer; cdecl;
begin
  result:=readDoubleEx(L, GetCurrentProcess);
end;

function readDouble(L: PLua_State): integer; cdecl;
begin
  result:=readDoubleEx(L, processhandle);
end;

function readStringEx(L: PLua_State; ProcessHandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: pchar;
  w: pwidechar absolute v;
  s: string;
  r: PtrUInt;
  maxsize: integer;

  usewidechar: boolean;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters>=1 then
    begin
      address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

      if parameters>=2 then
        maxsize:=lua_tointeger(L,2)
      else
        maxsize:=50;

      if parameters>=3 then
      begin
        usewidechar:=lua_toboolean(L,3);
        maxsize:=maxsize+1;
      end
      else
        usewidechar:=false;

      lua_pop(L, parameters);

      getmem(v,maxsize+1);
      try
        r:=0;
        ReadProcessMemory(processhandle, pointer(address), v, maxsize, r);

        if (r>0) and (r<=maxsize) then
        begin
          v[maxsize]:=#0;

          if (r+1)<=maxsize then
            v[r+1]:=#0;

          if usewidechar then
          begin
            v[maxsize-1]:=#0;
            if (r+2)<=maxsize then
              v[r+2]:=#0;

            s:=w;
          end
          else
            s:=v;


          lua_pushstring(L, s);
          result:=1;
        end;


      finally
        freememandnil(v);
      end;

    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function readStringLocal(L: PLua_State): integer; cdecl;
begin
  result:=readStringEx(L, GetCurrentProcess);
end;

function readString(L: PLua_State): integer; cdecl;
begin
  result:=readStringEx(L, processhandle);
end;

function writeShortIntegerEx(L: PLua_State; processhandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: shortint;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

      v:=lua_tointeger(L, 2);

      lua_pop(L, parameters);
      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeShortIntegerLocal(L: PLua_State): integer; cdecl;
begin
  result:=writeShortIntegerEx(L, GetCurrentProcess);
end;

function writeShortInteger(L: PLua_State): integer; cdecl;
begin
  result:=writeShortIntegerEx(L, processhandle);
end;

function writeSmallIntegerEx(L: PLua_State; processhandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: smallInt;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

      v:=lua_tointeger(L, 2);

      lua_pop(L, parameters);
      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeSmallIntegerLocal(L: PLua_State): integer; cdecl;
begin
  result:=writeSmallIntegerEx(L, GetCurrentProcess);
end;

function writeSmallInteger(L: PLua_State): integer; cdecl;
begin
  result:=writeSmallIntegerEx(L, processhandle);
end;

function writeIntegerEx(L: PLua_State; processhandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: integer;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

      v:=lua_tointeger(L, 2);

      lua_pop(L, parameters);
      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeIntegerLocal(L: PLua_State): integer; cdecl;
begin
  result:=writeIntegerEx(L, GetCurrentProcess);
end;

function writeInteger(L: PLua_State): integer; cdecl;
begin
  result:=writeIntegerEx(L, processhandle);
end;

function writeQwordEx(L: PLua_State; processhandle: THandle): integer; cdecl;
var
  parameters: Qword;
  address: ptruint;

  v: Qword;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

      v:=lua_tointeger(L, 2);

      lua_pop(L, parameters);
      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeQwordLocal(L: PLua_State): integer; cdecl;
begin
  result:=writeQwordEx(L, GetCurrentProcess);
end;

function writeQword(L: PLua_State): integer; cdecl;
begin
  result:=writeQwordEx(L, processhandle);
end;

function writePointerLocal(L: PLua_State): integer; cdecl;
begin
{$ifdef cpu64}
  result:=writeQwordLocal(L);
{$else}
  result:=writeIntegerLocal(L);
{$endif}
end;

function writePointer(L: PLua_State): integer; cdecl;
begin
  if processhandler.is64bit then
    result:=writeQword(L)
  else
    result:=writeInteger(L);
end;

function writeFloatEx(L: PLua_State; processhandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: single;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters>=2 then
    begin
      address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

      v:=lua_tonumber(L, 2);

      lua_pop(L, parameters);


      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeFloatLocal(L: PLua_State): integer; cdecl;
begin
  result:=writeFloatEx(L, GetCurrentProcess);
end;

function writeFloat(L: PLua_State): integer; cdecl;
begin
  result:=writeFloatEx(L, processhandle);
end;


function writeDoubleEx(L: PLua_State; processhandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: double;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters=2 then
    begin
      address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

      v:=lua_tonumber(L, 2);

      lua_pop(L, parameters);

      lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @v, sizeof(v), r));
      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeDoubleLocal(L: PLua_State): integer; cdecl;
begin
  result:=writeDoubleEx(L, GetCurrentProcess);
end;

function writeDouble(L: PLua_State): integer; cdecl;
begin
  result:=writeDoubleEx(L, processhandle);
end;

function writeStringEx(L: PLua_State; processhandle: THandle): integer; cdecl;
var
  parameters: integer;
  address: ptruint;

  v: pchar;
  usewidechar: boolean;

  w: widestring;
  r: PtrUInt;
begin
  result:=0;
  try
    parameters:=lua_gettop(L);
    if parameters>=2 then
    begin
      address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

      v:=lua.lua_tostring(L, 2);

      if parameters>=3 then
        usewidechar:=lua.lua_toboolean(L, 3)
      else
        usewidechar:=false;

      lua_pop(L, parameters);

      if usewidechar then
      begin
        //convert the ansi sring to a widestring
        w:=v;
        lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), @w[1], length(w)*2, r));
      end
      else
        lua_pushboolean(L, WriteProcessMemory(processhandle, pointer(address), v, length(v), r));

      result:=1;
    end;
  except
    result:=0;
    lua_pop(L, lua_gettop(L));
  end;
end;

function writeStringLocal(L: PLua_State): integer; cdecl;
begin
  result:=writeStringEx(L, GetCurrentProcess);
end;

function writeString(L: PLua_State): integer; cdecl;
begin
  result:=writeStringEx(L, processhandle);
end;

function readBytesEx(processhandle: THandle; L: PLua_State): integer; cdecl;
var parameters: integer;
  addresstoread: ptruint;
  bytestoread: integer;
  i: integer;
  bytes: array of byte;
  x: PtrUInt;
  tableversion: boolean;
begin
  tableversion:=false;
  result:=0;
  parameters:=lua_gettop(L);

  if lua_isstring(L, 1) then
  begin
    if processhandle=GetCurrentProcess then
      addresstoread:=selfsymhandler.getAddressFromNameL(lua_tostring(L,1), waitforsymbols)
    else
      addresstoread:=symhandler.getAddressFromNameL(lua_tostring(L,1), waitforsymbols);
  end
  else
    addresstoread:=lua_tointeger(L,1);

  if parameters>1 then
  begin
    bytestoread:=lua_tointeger(L,2);

    if parameters>2 then
      tableversion:=lua_toboolean(L, 3);
  end
  else
    bytestoread:=1;

  lua_pop(L, parameters);

  setlength(bytes,bytestoread);
  ZeroMemory(@bytes[0], bytestoread);
  x:=0;
  ReadProcessMemory(processhandle, pointer(addresstoread), @bytes[0], bytestoread, x);

  if (x>0) and (x<=bytestoread) then
  begin
    if tableversion then
    begin
      lua_createtable(L, x,0);
      for i:=0 to x-1 do
      begin
        lua_pushinteger(L, i+1);
        lua_pushinteger(L, bytes[i]);
        lua_settable(L, -3);
      end;
      result:=1;
    end
    else
    begin
      while lua_checkstack(L,x)=false do
        x:=x div 2;

      for i:=0 to x-1 do
        lua_pushinteger(L,bytes[i]);

      result:=x;
    end;
  end;
end;


function writeBytesEx(processhandle: THandle; L: PLua_State): integer;
var
  parameters, parameters2: integer;
  bytes: array of byte;
  i,j: integer;
  bytecount: integer;
  address: ptruint;
  x: PtrUInt;
  oldprotect: dword;
  b: byte;
  vpe: boolean;
begin
  parameters:=lua_gettop(L);
  if parameters=0 then exit(0);



  address:=lua_toaddress(L,1,processhandle=GetCurrentProcess);

  bytecount:=0;
  if lua_istable(L, 2) then
  begin
    parameters2:=lua_objlen(L, 2);
    setlength(bytes, parameters2);



    for i:=1 to parameters2 do
    begin
      lua_pushinteger(L,i);
      lua_gettable(L, 2);

      if lua_isnumber(L,-1) then
      begin
        j:=lua_tointeger(L,-1);
        bytes[bytecount]:=j;
        inc(bytecount);
      end;
      lua_pop(L,1);
    end;


  end
  else
  begin
    setlength(bytes,parameters-1);

    bytecount:=0;
    for i:=(-parameters)+1 to -1 do
    begin
      b:=lua_tointeger(L,i);
      bytes[bytecount]:=b;
      inc(bytecount);
    end;

  end;

  x:=0;
  if SystemSupportsWritableExecutableMemory or SkipVirtualProtectEx then
    vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle, pointer(address), bytecount, PAGE_EXECUTE_READWRITE, oldprotect)
  else
  begin
    if processid<>GetCurrentProcessId then
      ntsuspendProcess(processhandle);
    vpe:=(SkipVirtualProtectEx=false) and VirtualProtectEx(processhandle, pointer(address), bytecount, PAGE_READWRITE, oldprotect);
  end;
  WriteProcessMemory(processhandle, pointer(address), @bytes[0], bytecount, x);
  if vpe then VirtualProtectEx(processhandle, pointer(address), bytecount, oldprotect, oldprotect);

  if (not (SystemSupportsWritableExecutableMemory or SkipVirtualProtectEx)) and (processid<>GetCurrentProcessId) then
    ntresumeProcess(processhandle);


  lua_pop(L, parameters);
  lua_pushinteger(L, x);    //return the number of bytes written

  result:=1;  //return 1 value
end;

function writeBytes(L: PLua_state): integer; cdecl;
begin
  result:=writeBytesEx(processhandle, L);
end;

function readBytes(L: PLua_State): integer; cdecl;
begin
  result:=readBytesEx(processhandle, L);
end;

function writeBytesLocal(L: PLua_state): integer; cdecl;
begin
  result:=writebytesEx(getcurrentprocess, L);
end;

function readBytesLocal(L: PLua_State): integer; cdecl;
begin
  result:=readbytesEx(getcurrentprocess, L);
end;


function lua_createSection(L: PLua_State): integer; cdecl;
{$IFDEF windows}
var
  n: NTSTATUS;
  s: THandle;
  size: LARGE_INTEGER;
{$ENDIF}
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=1 then
  begin
    size.QuadPart:=lua_tointeger(L,1);
    s:=0;
    n:=ZwCreateSection(@s,SECTION_ALL_ACCESS,nil, @size, PAGE_EXECUTE_READWRITE,SEC_COMMIT,0);

   // STATUS_MAPPED_FILE_SIZE_ZERO
    if Succeeded(n) then
    begin
      lua_pushinteger(L,s);
      result:=1;
    end
    else
    begin
      lua_pushnil(L);
      lua_pushinteger(L,n);
      result:=2;
    end;
  end;
  {$ENDIF}
end;

function lua_MapViewOfSection(L: PLua_State): integer; cdecl;
{$IFDEF windows}
var
  basep: pointer;
  sh: thandle;
  base: pointer;

  offset: LARGE_INTEGER;
  n: NTSTATUS;
  si: SECTION_INHERIT;
  viewsize: LARGE_INTEGER;
{$ENDIF}
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=1 then
  begin
    sh:=lua_tointeger(L,1);

    if lua_Gettop(L)>=2 then
    begin
      if lua_isstring(L, 2) then
        base:=pointer(symhandler.getAddressFromNameL(lua_tostring(L,2), waitforsymbols))
      else
        base:=pointer(lua_tointeger(L,2));
    end
    else
      base:=nil;


    basep:=@base;



    offset.QuadPart:=0;
    viewsize.QuadPart:=0;

    n:=ZwMapViewOfSection(sh,processhandle, @base,0,0,nil,@viewsize,2,0,PAGE_EXECUTE_READWRITE);
    if succeeded(n) then
    begin
      lua_pushinteger(L,ptruint(base));
      result:=1;
    end
    else
    begin
      lua_pushnil(L);
      lua_pushinteger(L, n);
      result:=2;
    end;
  end;
  {$ENDIF}




end;

function lua_unMapViewOfSection(L: PLua_State): integer; cdecl;
var parameters: integer;
    address: ptruint;
begin
  {$IFDEF windows}
  result:=1;
  parameters:=lua_gettop(L);
  if parameters=0 then begin lua_pushboolean(L, false); exit; end;

  address:=lua_toaddress(L,1);

  lua_pop(L, parameters);
  lua_pushinteger(L, ZwUnmapViewOfSection(processhandle, pointer(address)));
  {$ELSE}
  result:=0;
  {$ENDIF}
end;



function deAllocEx(processhandle: THandle; L: PLua_State): integer; cdecl;
var parameters: integer;
    address: ptruint;
    size: integer;
begin
  result:=1;
  parameters:=lua_gettop(L);
  if parameters=0 then begin lua_pushboolean(L, false); exit; end;

  address:=lua_toaddress(L,1, processhandle=GetCurrentProcess);
  size:=lua_tointeger(L,2);

  lua_pop(L, parameters);
  lua_pushboolean(L, virtualfreeex(processhandle,pointer(address),0,MEM_RELEASE));
end;

function deAlloc_lua(L: PLua_State): integer; cdecl;
begin
  result:=deAllocEx(processhandle, L);
end;

function deAllocLocal_lua(L: PLua_State): integer; cdecl;
begin
  result:=deAllocEx(getcurrentprocess, L);
end;

function AutoAssembleCheck_lua(L: PLua_State): integer; cdecl;
var
  script: tstringlist;
  enable: boolean;
  targetself: boolean;
begin
  if lua_gettop(L)=0 then
  begin
    lua_pushboolean(L,false);
    lua_pushstring(L,'No parameters given');
    exit(2);
  end;

  script:=tstringlist.create;
  try
    script.text:=Lua_ToString(L,1);
    if lua_gettop(L)>=2 then
      enable:=lua_toboolean(L,2)
    else
      enable:=true;

    if lua_gettop(L)>=3 then
      targetself:=lua_toboolean(L,3)
    else
      targetself:=false;

    try
      lua_pushboolean(L,autoassemble(script,false,enable,true,targetself));
      exit(1);
    except
      on e:exception do
      begin
        lua_pushboolean(L,false);
        lua_pushstring(L,e.message);
        exit(2);
      end;
    end;

  finally
    script.free;
  end;
end;

function autoAssemble_lua(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  code: TStringlist=nil;

  disableinfo: TDisableInfo;

  r: boolean;
  targetself: boolean;


  i: integer;

  secondaryResultTable: integer;
  tableIndex, tableIndex2: integer;
  disableInfoIndex: integer;
  enable: boolean;

  name: string;
  address: ptruint;


begin
  result:=1;
  enable:=true;


  parameters:=lua_gettop(L);
  if parameters=0 then
  begin
    lua_pushboolean(L, false);
    exit;
  end;

  code:=tstringlist.create;

  disableinfo:=TDisableInfo.create;

  try
    code.text:=lua_tostring(L, 1);
    if (parameters>1) and lua_isboolean(L,2) then
      targetself:=lua_toboolean(L, 2)
    else
      targetself:=false;

    disableInfoIndex:=0;
    if (parameters>1) and lua_istable(L,2) then
      disableInfoIndex:=2
    else
    if (parameters>2) and lua_istable(L,3) then
      disableInfoIndex:=3;



    if disableInfoIndex>0 then
    begin
      try
        if lua_istable(L, disableInfoIndex) then
        begin
          enable:=false;
          lua_pushstring(L,'allocs');
          lua_gettable(L,disableInfoIndex);
          if lua_isnil(L,-1)=false then
          begin
            if lua_istable(L,-1)=false then raise exception.create('Corrupt disableInfo section at the allocs side');
            //enum all the entries

            lua_pushnil(L);   //allocs table at -2
            while lua_next(L, -2)<>0 do
            begin
              i:=length(disableinfo.allocs);
              setlength(disableinfo.allocs,i+1);

              disableinfo.Allocs[i].varname:=Lua_ToString(L,-2);

              tableindex:=lua_gettop(L);

              lua_pushstring(L,'address');
              lua_gettable(L,tableindex);
              if lua_isnumber(L,-1)=false then raise exception.create('Corrupt disableInfo section at '+disableinfo.allocs[i].varname+'.address');
              disableinfo.allocs[i].address:=lua_tointeger(L,-1);
              lua_pop(L,1);

              lua_pushstring(L,'size');
              lua_gettable(L,tableindex);
              if lua_isnumber(L,-1)=false then raise exception.create('Corrupt disableInfo section at '+disableinfo.Allocs[i].varname+'.size');
              disableinfo.Allocs[i].size:=lua_tointeger(L,-1);
              lua_pop(L,1);

              lua_pushstring(L,'prefered');
              lua_gettable(L,tableindex);
              if lua_isnil(L,-1)=false then
              begin
                if lua_isnumber(L,-1)=false then raise exception.create('Corrupt disableInfo section at '+disableinfo.Allocs[i].varname+'.prefered');
                disableinfo.Allocs[i].prefered:=lua_tointeger(L,-1);
              end;
              lua_pop(L,1);

              lua_pop(L,1);
            end;
          end;
          lua_pop(L,1);

          lua_pushstring(L,'registeredsymbols');
          lua_gettable(L,disableInfoIndex);
          if not lua_isnil(L,-1) then
          begin
            if lua_istable(L,-1)=false then raise exception.create('Corrupt disableInfo section at the registeredsymbols side');
            lua_pushnil(L);
            while lua_next(L, disableInfoIndex+1)<>0 do
            begin
              disableinfo.registeredsymbols.Add(Lua_ToString(L,-1));
              lua_pop(L,1);
            end;
          end;
          lua_pop(L,1);


          lua_pushstring(L,'ccodesymbols');
          lua_gettable(L,disableInfoIndex);
          if not lua_isnil(L,-1) then
          begin
            disableinfo.ccodesymbols.free;
            disableinfo.ccodesymbols:=lua_ToCEUserData(L,-1);
            lua_pop(L,1);
          end;

          lua_pushstring(L,'exceptionlist');
          lua_gettable(L,disableInfoIndex);
          if not lua_isnil(L,-1) then
          begin
            if lua_istable(L,-1)=false then raise exception.create('Corrupt disableInfo section at the exceptionlist side');

            setlength(disableinfo.exceptions, lua_objlen(L,-1));

            for i:=1 to length(disableinfo.exceptions) do
            begin
              lua_pushinteger(L,i);
              lua_gettable(L,-2);
              disableinfo.exceptions[i-1]:=lua_tointeger(L,-1);
              lua_pop(L,1);
            end;
          end;
          lua_pop(L,1); //pop exceptionlist

          lua_pushstring(L,'symbols');
          lua_gettable(l,disableInfoIndex);
          if not lua_isnil(L,-1) then
          begin
            if lua_istable(L,-1)=false then raise exception.create('Corrupt disableInfo section at the symbols side');
            lua_pushnil(L);
            while lua_next(L, -2)<>0 do
            begin
              name:=Lua_ToString(L,-2);
              address:=lua_tointeger(L,-1);
              disableinfo.allsymbols.AddObject(name, tobject(address));
              lua_pop(L,1);
            end;
          end;
          lua_pop(L,1);
        end
        else raise exception.create('Not a valid disableInfo variable');

      except
        on e:exception do
        begin
          lua_pushboolean(L,false);
          lua_pushstring(L,e.Message);
          exit(2);
        end;
      end;
    end;

    try
      r:=autoassemble(code, false, enable, false, targetself, disableinfo);
    except
      on e:exception do
      begin
        lua_pushboolean(L,false);
        lua_pushstring(L,e.Message);
        exit(2);
      end;
    end;



    lua_pop(L, parameters);
    lua_pushboolean(L, r);


    if r and enable then
    begin
      result:=2;
      lua_newtable(L);
      secondaryResultTable:=lua_gettop(L); //should be 2

      lua_pushstring(L,'allocs');
      lua_newtable(L);
      tableIndex:=lua_gettop(L);

      for i:=0 to length(disableinfo.Allocs)-1 do
      begin
        lua_pushstring(L, disableinfo.Allocs[i].varname);
        lua_newtable(L);
        tableindex2:=lua_gettop(L);

        lua_setbasictableentry(L, tableindex2, 'address',disableinfo.Allocs[i].address);
        lua_setbasictableentry(L, tableindex2, 'size',disableinfo.Allocs[i].size);
        if disableinfo.Allocs[i].prefered<>0 then
          lua_setbasictableentry(L, tableindex2, 'prefered',disableinfo.Allocs[i].prefered);

        lua_settable(L, tableindex);
      end;
      lua_settable(l,secondaryResultTable);

      lua_pushstring(L,'registeredsymbols');
      lua_newtable(L);
      tableIndex:=lua_gettop(L);

      for i:=0 to disableinfo.registeredsymbols.Count-1 do
      begin
        lua_pushinteger(L,i+1);
        lua_pushstring(L, disableinfo.registeredsymbols[i]);
        lua_settable(L, tableIndex);
      end;

      lua_settable(L, secondaryResultTable);

      //ccode symbollist
      if (disableinfo.ccodesymbols.count>0) then
      begin
        disableinfo.donotfreeccodedata:=true; //will return later
        lua_pushstring(L,'ccodesymbols');
        luaclass_newClass(L,disableinfo.ccodesymbols);
        lua_settable(L, secondaryResultTable);
      end;

      lua_pushstring(L,'exceptionlist');
      lua_newtable(L);
      tableIndex:=lua_gettop(L);

      for i:=0 to length(disableinfo.exceptions)-1 do
      begin
        lua_pushinteger(L,i+1);
        lua_pushinteger(L, disableinfo.exceptions[i]);
        lua_settable(L, tableIndex);
      end;
      lua_settable(L, secondaryResultTable);

      lua_pushstring(L,'symbols');
      lua_newtable(L);
      tableindex:=lua_gettop(L);
      for i:=0 to disableinfo.allsymbols.count-1 do
      begin
        name:=disableinfo.allsymbols[i];
        address:=ptruint(disableinfo.allsymbols.Objects[i]);
        lua_pushstring(L,name);
        lua_pushinteger(L,address);
        lua_settable(L, tableindex);
      end;

      lua_settable(L, secondaryResultTable);
    end;
  finally
    if code<>nil then
      code.free;

    if disableinfo<>nil then
      disableinfo.free;
  end;

end;

function lua_assemble(L: PLua_State): integer; cdecl;
var
  address: ptruint;
  line: string;
  pref: TassemblerPreference;
  skiprangecheck: boolean;

  r: TAssemblerBytes;
begin
  if lua_gettop(L)>=1 then
  begin
    try
      line:=Lua_ToString(L,1);
      if lua_gettop(L)>=2 then
        address:=lua_toaddress(L,2)
      else
        address:=0;


      if lua_gettop(L)>=3 then
        pref:=TassemblerPreference(lua_tointeger(L,3))
      else
        pref:=apNone;

      if lua_gettop(L)>=4 then
        skiprangecheck:=lua_toboolean(L,4)
      else
        skiprangecheck:=false;

      if Assemble(line,address,r, pref,skiprangecheck) then
      begin
        CreateByteTableFromPointer(L,@r[0],length(r));
        exit(1);
      end
      else
      begin
        lua_pushnil(L);
        exit(1);
      end;
    except
      on e: exception do
      begin
        lua_pushnil(L);
        lua_pushstring(L,e.Message);
        exit(2);
      end;
    end;


  end
  else
  begin
    lua_pushnil(L);
    lua_pushstring(L,'invalid parameters');
    exit(2);
  end;

end;

function getPixel(L: PLua_State): integer; cdecl;
var t:TCanvas;
  parameters: integer;
  r: dword;
  x,y: integer;
begin
  result:=0; //return 0 paramaters
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    x:=lua_tointeger(L, -2); //x
    y:=lua_tointeger(L, -1); //y
    lua_pop(L, 2);

    try
      t:=TCanvas.create;
      try
        t.Handle:=getdc(0);
        r:=t.Pixels[x,y];

        lua_pushinteger(L,r); //push the color on the stack
        result:=1; //tell lue I put 1 parameter on the stack

        ReleaseDC(0,t.handle);
      finally
        t.free;
      end;
    except
    end;
  end else lua_pop(L, parameters);
end;

function getMousePos(L: PLua_State): integer; cdecl;
var t:TCanvas;
  parameters: integer;
  cp: Tpoint;
begin
  result:=0; //return 0 parameters
  parameters:=lua_gettop(L);
  if parameters=0 then
  begin
    cp:=mouse.CursorPos;
    lua_pushinteger(L, cp.x);
    lua_pushinteger(L, cp.y);
    result:=2;   //return 2 parameters
  end else lua_pop(L, parameters);
end;

function setMousePos(L: PLua_State): integer; cdecl;
var t:TCanvas;
  parameters: integer;
  cp: Tpoint;
begin
  result:=0; //return 0 parameters
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    cp.x:=lua_tointeger(L, -2); //x
    cp.y:=lua_tointeger(L, -1); //y
    lua_pop(L, 2);

    mouse.CursorPos:=cp;
  end else lua_pop(L, parameters);
end;

function createTableEntry(L: PLua_State): integer; cdecl;
var parameters: integer;
  r: pointer;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack

  r:=ce_createTableEntry;
  luaclass_newClass(L, r);
  result:=1;
end;

function getTableEntry(L: PLua_State): integer; cdecl;
var parameters: integer;
  description: pchar;
  r: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    description:=lua.lua_tostring(L,-1); //description

    lua_pop(L, parameters);  //clear stack

    r:=ce_getTableEntry(description);
    if r<>nil then
    begin
      luaclass_newClass(L, r);
      result:=1;
    end;
  end else lua_pop(L, parameters);
end;



function isKeyPressed(L: PLua_State): integer; cdecl;
var parameters: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  r:=false;
  key:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,-1) then  //char given instead of keycode
    begin
      keyinput:=lua.lua_tostring(L,-1);
      if keyinput<>nil then
        key:=ord(keyinput[0]);
    end
    else

    if lua_isnumber(L,-1) then //keycode
      key:=lua_tointeger(L,-1);

    lua_pop(L, parameters); //parameters have been fetched, clear stack

    if key<>0 then
    begin
      {$IFDEF windows}
      if key>=VK_PAD_A then
      begin
        lua_pushboolean(L, HotkeyHandler.IsKeyPressed(key));
        exit(1);
      end;
      {$ENDIF}


      w:=GetAsyncKeyState(key);
      r:=(w and 1)=1;

      if not r then
        r:=((w shr 15) and 1)=1;

      lua_pushboolean(L, r);
      result:=1;
    end;

  end else lua_pop(L, parameters);

end;


function lua_mouse_event(L: PLua_State): integer; cdecl;
var
  flags, x,y,data: dword;
  extrainfo: ptruint;
  c: integer;
begin
  result:=0;

  {$IFDEF windows}
  c:=lua_gettop(L);
  if c=0 then exit(0); //flags is important, the rest can be ignored

  flags:=lua_tointeger(L, 1);
  x:=lua_tointeger(L, 2);
  y:=lua_tointeger(L, 3);
  data:=lua_tointeger(L, 4);
  extrainfo:=lua_tointeger(L, 5);

  mouse_event(flags, x, y, data, extrainfo);
  {$ENDIF}

end;

function keyDown(L: PLua_State): integer; cdecl;
var parameters: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  {$IFDEF windows}
  r:=false;
  key:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,-1) then  //char given isntead of keycode
    begin
      keyinput:=lua.lua_tostring(L,-1);
      if keyinput<>nil then
        key:=ord(keyinput[0]);
    end
    else
    if lua_isnumber(L,-1) then //keycode
      key:=lua_tointeger(L,-1);


    if key<>0 then
      keybd_event(key, 0,0,0);

  end;
  lua_pop(L, parameters);
  {$ENDIF}
end;


function keyUp(L: PLua_State): integer; cdecl;
var parameters: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  {$IFDEF windows}
  r:=false;
  key:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,-1) then  //char given isntead of keycode
    begin
      keyinput:=lua.lua_tostring(L,-1);
      if keyinput<>nil then
        key:=ord(keyinput[0]);
    end
    else
    if lua_isnumber(L,-1) then //keycode
      key:=lua_tointeger(L,-1);


    if key<>0 then
      keybd_event(key, 0,KEYEVENTF_KEYUP,0);

  end;
  lua_pop(L, parameters);
  {$ENDIF}
end;

function doKeyPress(L: PLua_State): integer; cdecl;
var parameters: integer;
  keyinput: pchar;
  key: integer;
  w: word;
  r: boolean;
begin
  result:=0;
  {$IFDEF windows}
  r:=false;
  key:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    if lua_isstring(L,1) then  //char given instead of keycode
    begin
      keyinput:=lua.lua_tostring(L,1);
      if keyinput<>nil then
        key:=ord(keyinput[0]);
    end
    else
    if lua_isnumber(L,1) then //keycode
      key:=lua_tointeger(L,1);


    if key<>0 then
    begin
      keybd_event(key, 0, 0, 0);
      windows.sleep(110);
      keybd_event(key, 0, KEYEVENTF_KEYUP, 0);
    end;

  end;
  lua_pop(L, parameters);
{$ENDIF}
end;

function getProcessIDFromProcessName(L: PLua_state): integer; cdecl;
var parameters: integer;
  pname: pchar;
  pid: dword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    pname:=lua.lua_tostring(L, -1);
    lua_pop(L, parameters);

    pid:=ce_getProcessIDFromProcessName(pname);
    if pid<>0 then
    begin
      lua_pushinteger(L, pid);
      result:=1;
    end;


  end else lua_pop(L, parameters);
end;

function openProcess(L: PLua_state): integer; cdecl;
var parameters: integer;
  pname: pchar;
  pid: dword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    if lua_isstring(L,1) then
    begin
      pname:=lua.lua_tostring(L,1);
      pid:=ce_getProcessIDFromProcessName(pname);
    end
    else
      pid:=lua_tointeger(L,1);

    lua_pop(L, parameters);

    if pid<>0 then
      ce_openProcess(pid);

    if (ProcessHandle<>0) and (processid=pid) then
    begin
      lua_pushboolean(L, true);
      result:=1;
    end;

  end else lua_pop(L, parameters);
end;

function beep(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  SysUtils.Beep;
  result:=0;
end;

function pause(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  ce_pause;
  result:=0;
end;

function unpause(L: PLua_state): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L)); //clear the stack
  ce_unpause;
  result:=0;
end;


function debugProcess(L: PLua_state): integer; cdecl;
var parameters: integer;
  debuggerinterface: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
    debuggerinterface:=lua_tointeger(L, -1)
  else
    debuggerinterface:=0;

  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_debugProcess(debuggerinterface);
end;

function debug_getCurrentDebuggerInterface(L:PLua_State): integer; cdecl;
begin

  if debuggerthread<>nil then
  begin
    {$IFDEF windows}
    if (CurrentDebuggerInterface is TWindowsDebuggerInterface) then
      lua_pushinteger(L, 1);

    if (CurrentDebuggerInterface is TVEHDebugInterface) then
      lua_pushinteger(L, 2);

    if (CurrentDebuggerInterface is TKernelDebugInterface) then
      lua_pushinteger(L, 3);
    {$ENDIF}

    {$ifdef darwin}
    lua_pushinteger(L, 4);
    {$endif}

    result:=1;
  end
  else
    result:=0;
end;

function debug_getBreakpointList(L: Plua_State): integer; cdecl;
var
  al: TAddressArray;
  t: integer;
  i: integer;
begin
  result:=0;
  if debuggerthread<>nil then
  begin
    debuggerthread.getBreakpointAddresses(al);
    lua_newtable(L);
    t:=lua_gettop(L);


    for i:=0 to length(al)-1 do
    begin
      lua_pushinteger(L, i+1);
      lua_pushinteger(L, al[i]);
      lua_settable(L, t);
    end;

    result:=1;
  end;

end;

function lua_checkSynchronize(L: Plua_State): integer; cdecl;
var timeout: integer=0;
begin
  result:=0;
  if lua_Gettop(L)>0 then
    timeout:=lua_tointeger(L,1);

  CheckSynchronize(timeout);
end;

function lua_queue(L: Plua_State): integer; cdecl;
var lc: TLuaCaller;
  f: integer;
  routine: string;
begin
  result:=0;
  lc:=nil;
  if lua_isfunction(L,1) then
  begin
    lua_pushvalue(L, 1);
    f:=luaL_ref(L,LUA_REGISTRYINDEX);

    lc:=TLuaCaller.create;
    lc.luaroutineIndex:=f;
  end
  else
  if lua_isstring(L,1) then
  begin
    routine:=lua_tostring(L,1);
    lc:=TLuaCaller.create;
    lc.luaroutine:=routine;
  end;

  if lc<>nil then
  begin
    lc.syncvm:=l;
    if lua_gettop(L)>=2 then
    begin
      lc.synchronizeparam:=2;
      lc.synchronizeparamcount:=lua_gettop(l)-1;
    end
    else
      lc.synchronizeparam:=0;

    tthread.ForceQueue(TThread.CurrentThread, lc.queue);

    result:=0;
  end;
end;

function lua_getCurrentThreadID(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, qword(GetCurrentThreadId));
  exit(1);
end;

function lua_synchronize(L: Plua_State): integer; cdecl;
var lc: TLuaCaller;
  f: integer;
  routine: string;
begin
  result:=0;
  lc:=nil;
  if lua_isfunction(L,1) then
  begin
    lua_pushvalue(L, 1);
    f:=luaL_ref(L,LUA_REGISTRYINDEX);

    lc:=TLuaCaller.create;
    lc.luaroutineIndex:=f;
  end
  else
  if lua_isstring(L,1) then
  begin
    routine:=lua_tostring(L,1);
    lc:=TLuaCaller.create;
    lc.luaroutine:=routine;
  end;

  if lc<>nil then
  begin
    lc.syncvm:=l;
    if lua_gettop(L)>=2 then
    begin
      lc.synchronizeparam:=2;
      lc.synchronizeparamcount:=lua_gettop(l)-1;
    end
    else
      lc.synchronizeparam:=0;

    tthread.Synchronize(nil, lc.synchronize);

    result:=1;
  end;
end;

function inMainThread(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, MainThreadID=GetCurrentThreadId);
  result:=1;
end;

function debug_isDebugging(L: Plua_State): integer; cdecl;
begin
  lua_pushboolean(L, debuggerthread<>nil);
  result:=1;
end;



function debug_canBreak(L: Plua_State): integer; cdecl;
var list: TAddressArray;
begin
  if debuggerthread<>nil then
  begin
    debuggerthread.getBreakpointAddresses(list);

    lua_pushboolean(L, (length(list)>0) or ((debuggerthread.CurrentThread<>nil) and (debuggerthread.CurrentThread.isHandled)) );
  end
  else
    lua_pushboolean(L, false);

  result:=1;
end;

function debug_isBroken(L: PLua_state): integer; cdecl;
var r: boolean;
begin
  r:=(debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) and (debuggerthread.CurrentThread.isHandled);
  lua_pushboolean(L, r);
  result:=1;
end;

function debug_isStepping(L: PLua_state): integer; cdecl;
var r: boolean;
begin
  r:=(debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) and (debuggerthread.CurrentThread.isSingleStepping);
  lua_pushboolean(L, r);
  result:=1;
end;

function debug_setBreakpointForThread(L: Plua_State): integer; cdecl;
var
  threadid: dword;
  parameters: integer;
  address: ptruint;
  size: integer;
  trigger: TBreakpointTrigger;
  method: TBreakpointMethod;

  lc: TLuaCaller;

  bpe: TBreakpointEvent;
begin
  lc:=nil;

  trigger:=bptExecute;
  result:=0;
  size:=1;
  parameters:=lua_gettop(L);
  if parameters=0 then
    raise exception.create(rsDebugsetBreakpointNeedsAtLeastAnAddress);

  threadid:=lua_tointeger(L,1);
  address:=lua_toaddress(L,2);

  if parameters>=3 then
  begin
    if lua_isfunction(L,3) then //address, function type
    begin
      lua_pushvalue(L,3);
      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=luaL_ref(L,LUA_REGISTRYINDEX);
    end
    else
    begin
      if lua_isnumber(L, 3) then
        size:=lua_tointeger(L, 3)
      else
      begin //function name as string
        lc:=TLuaCaller.create;
        lc.luaroutine:=Lua_ToString(L,3);
      end;
    end;
  end;

  method:=preferedBreakpointMethod;

  if lc=nil then  //address, size OPTIONAL, trigger OPTIONAL, method, functiontocall OPTIONAL
  begin
    if parameters>=4 then
      trigger:=TBreakpointTrigger(lua_tointeger(L,4))
    else
      trigger:=bptExecute;

    if parameters>=5 then
    begin
      if lua_isnumber(L, 5) then //address, size OPTIONAL, trigger OPTIONAL, method
      begin
        method:=TBreakpointMethod(lua_tointeger(L,5));
      end
      else
      begin
        //addresss, size, trigger, function
        if lua_isfunction(L,5) then //address, function type
        begin
          lua_pushvalue(L,5);
          lc:=TLuaCaller.create;
          lc.luaroutineIndex:=luaL_ref(L,LUA_REGISTRYINDEX);
        end
        else
        begin
          lc:=TLuaCaller.create;
          lc.luaroutine:=Lua_ToString(L,5);
        end;
      end;
    end;


    if lc=nil then
    begin
      if parameters>=6 then
      begin
        if lua_isfunction(L,6) then //address, function type
        begin
          lua_pushvalue(L,6);
          lc:=TLuaCaller.create;
          lc.luaroutineIndex:=luaL_ref(L,LUA_REGISTRYINDEX);
        end
        else
        begin
          lc:=TLuaCaller.create;
          lc.luaroutine:=Lua_ToString(L,6);
        end;

      end;
    end;
  end;

  try
    if lc<>nil then
      bpe:=TBreakpointEvent(lc.BreakpointEvent)
    else
      bpe:=nil;

    if startdebuggerifneeded(false) then
    begin
      case trigger of
        bptAccess: debuggerthread.SetOnAccessBreakpoint(address, size, method, threadid, bpe);
        bptWrite: debuggerthread.SetOnWriteBreakpoint(address, size, method, threadid, bpe);
        bptExecute: debuggerthread.SetOnExecuteBreakpoint(address, method,false, threadid, bpe);
      end;

      MemoryBrowser.hexview.update;
      Memorybrowser.disassemblerview.Update;
    end;

  except
  end;


  lua_pop(L, lua_gettop(L)); //clear the stack
end;

function debug_setBreakpoint(L: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L,0);
  lua_insert(L,1);
  result:=debug_setBreakpointForThread(L);
end;

function debug_removeBreakpoint(L: Plua_State): integer; cdecl;
var parameters: integer;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    address:=lua_toaddress(L,1);

    lua_pushboolean(L, ce_debug_removeBreakpoint(address));
    result:=1;
  end;

  lua_pop(L, lua_gettop(L)); //clear the stack
end;

function debug_breakThread(L: Plua_State): integer; cdecl;
var
  threadid: dword;
  threadlist: TList;
  i: integer;
begin
  result:=0;

  if lua_gettop(L)>0 then
  begin
    threadid:=lua_tointeger(L,1);
    if not startdebuggerifneeded(false) then exit;

    if debuggerthread<>nil then
    begin
      //find the thread

      threadlist:=debuggerthread.lockThreadlist;
      try
        for i:=0 to threadlist.count-1 do
        begin
          if TDebugThreadHandler(threadlist[i]).ThreadId=threadid then
          begin
            TDebugThreadHandler(threadlist[i]).breakThread;
            exit(0);
          end;
        end;
      finally
        debuggerthread.unlockThreadlist;
      end;
    end;

  end;


end;

function debug_continueFromBreakpoint(L: Plua_State): integer; cdecl;
var parameters: integer;
  method: TContinueOption;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    method:=TContinueOption(lua_tointeger(L, -1));
    ce_debug_continuefrombreakpoint(method);
  end;

  lua_pop(L, lua_gettop(L)); //clear the stack
end;

function debug_addThreadToNoBreakList(L: Plua_State): integer; cdecl;
begin
  result:=1;
  if debuggerthread<>nil then
  begin
    CurrentDebuggerInterface.AddToNoBreakList(lua_tointeger(L,1));
    lua_pushboolean(L, true);
  end
  else
    lua_pushboolean(L, false);
end;

function debug_removeThreadFromNoBreakList(L: Plua_State): integer; cdecl;
begin
  result:=1;
  if debuggerthread<>nil then
  begin
    CurrentDebuggerInterface.RemoveFromNoBreakList(lua_tointeger(L,1));
    lua_pushboolean(L, true);
  end
  else
    lua_pushboolean(L, false);
end;

var isclosing: boolean;
function closeCE(L: Plua_state): integer; cdecl;
var th: thandle;
  b: boolean;
begin
  if isclosing=false then
  begin
    isclosing:=true;

    ce_closeCE; //cleanup

    {$ifdef windows}
    ExitProcess(0);
    {$else}
    application.Terminate;
    {$endif}

  end;
  result:=0;
end;

function hideAllCEWindows(L: Plua_State): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_hideAllCEWindows;
end;

function unhideMainCEwindow(L: Plua_State): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L)); //clear the stack

  ce_unhideMainCEwindow;
end;




function createLabel(L: Plua_State): integer; cdecl;
var parameters: integer;
  f,p: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    f:=lua_toceuserdata(L, -1);
    p:=ce_createLabel(f);

    lua_pop(L, lua_gettop(L));

    luaclass_newClass(L, p);
    result:=1;
  end else lua_pop(L, lua_gettop(L));
end;

function messageDialog(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  message: string;
  title: string;
  dialogtype: TMsgDlgType;
  buttontype: integer;

  r: integer;

  i: integer;
  b: TMsgDlgButtons;

  dialogtypeindex: integer;
begin
  result:=0;
  dialogtypeindex:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    title:='';

    if parameters>=2 then
    begin
      if lua_type(L,2)=LUA_TSTRING then
      begin
        if parameters>=3 then
          dialogtypeindex:=3;

        title:=Lua_ToString(L,1);
        message:=lua_tostring(L,2);
      end
      else
      begin
        message:=lua_tostring(L,1);
        if parameters>=2 then
          dialogtypeindex:=2;
      end;
    end
    else
      message:=lua_tostring(L,1);

    if dialogtypeindex=0 then
      dialogtype:=mtConfirmation
    else
      dialogtype:=TMsgDlgType(lua_tointeger(L,dialogtypeindex));

    b:=[];

    if dialogtypeindex>0 then
    begin
      for i:=dialogtypeindex+1 to parameters do
      begin
        buttontype:=lua_tointeger(L,i);
        case buttontype of
          0:  b:=b+[mbYes];
          1:  b:=b+[mbNo];
          2:  b:=b+[mbOK];
          3:  b:=b+[mbCancel];
          4:  b:=b+[mbAbort];
          5:  b:=b+[mbRetry];
          6:  b:=b+[mbIgnore];
          7:  b:=b+[mbAll];
          8:  b:=b+[mbNoToAll];
          9:  b:=b+[mbYesToAll];
          10: b:=b+[mbHelp];
          11: b:=b+[mbClose];
          else b:=b+[mbyes];
        end;
      end;
    end;

    if b=[] then
      b:=[mbOk];

    lua_pop(L, parameters);


    if title<>'' then
      r:=messageDlg(title,message,dialogtype,b,0)
    else
      r:=messageDlg(message, dialogtype, b,0);

    lua_pushinteger(L,r);
    result:=1;

  end else lua_pop(L, parameters);
end;

function speedhack_getSpeed(L: PLua_State): integer; cdecl;
begin
  lua_pushnumber(L, speedhack.getSpeed);
  result:=1;
end;

function speedhack_setSpeed(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  speed: single;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    speed:=lua_tonumber(L,-1);
    ce_speedhack_setSpeed(speed);
  end;
  lua_pop(L, parameters);
end;

function injectDLL(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  filename: string;
  skipsymbolwait: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    filename:=lua_tostring(L,1);
    try
      OutputDebugString('Lua: injectLibrary('+filename+')');

      if parameters>1 then
        skipsymbolwait:=lua_toboolean(L,2)
      else
        skipsymbolwait:=false;

      cefuncproc.injectdll(filename,'');

      if skipsymbolwait=false then
      begin
        symhandler.reinitialize;
        symhandler.waitForExports;
      end;

    except
      on e:exception do
      begin
        lua_pushboolean(L, false);
        lua_pushstring(L, e.Message);
        exit(2);
      end;
    end;

    //still here
    result:=1;
    lua_pushboolean(L, true);
  end;
end;




function getAutoAttachList(L: Plua_State): integer; cdecl;
var f: pointer;
  parameters: integer;
  visible: boolean;
begin
  result:=1;
  lua_pop(L, lua_gettop(L));

  f:=ce_getAutoAttachList();
  luaclass_newClass(L, f);
  result:=1;
end;



function generateAPIHookScript_lua(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  address: string;
  addressTo: string;
  addresstogetnewcalladdress: string;
  ext: string='';
  self: boolean=false;
  script: tstringlist;
  enable,disable: Tstringlist;
begin
  address:='';
  addressTo:='';
  addresstogetnewcalladdress:='';

  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=2 then
  begin
    address:=lua_tostring(L, 1);
    addressTo:=lua_tostring(L, 2);

    if parameters>=3 then
      addresstogetnewcalladdress:=lua_tostring(L, 3);

    if parameters>=4 then
      ext:=lua_tostring(L, 4);

    if parameters>=5 then
      self:=lua_toboolean(L,5);

    lua_pop(L, lua_gettop(L));

    script:=tstringlist.create;
    enable:=tstringlist.create;
    disable:=tstringlist.create;
    try
      script.add('[enable]');
      script.add('');
      script.add('[disable]');
      script.add('');

      generateAPIHookScript(script, address, addressto, addresstogetnewcalladdress,ext,self);

      getEnableOrDisableScript(script, enable, true);
      getEnableOrDisableScript(script, disable, false);

      lua_pushstring(L, pchar(enable.text));
      lua_pushstring(L, pchar(disable.text));
      result:=2;
    finally
      script.free;
      enable.free;
      disable.free;
    end;
  end;

end;

function createProcess(L: PLua_state): integer; cdecl;
var parameters: integer;
  path,params: string;
  debug: boolean;
  breakonentrypoint: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  path:='';
  params:='';
  debug:=false;
  breakonentrypoint:=false;

  if parameters>0 then
    path:=lua_tostring(L, 1);

  if parameters>1 then
    params:=lua_tostring(L, 2);

  if parameters>2 then
    debug:=lua_toboolean(L, 3);

  if parameters>3 then
    breakonentrypoint:=lua_toboolean(L, 4);

  if path<>'' then
    ce_createProcess(pchar(path), pchar(params), debug, breakonentrypoint);

  lua_pop(L, lua_gettop(L));
end;

function AOBScanModuleUnique(L: PLua_state): integer; cdecl;
var
  module: string;
  scanstring: string;
  protectionflags: string;
  alignmentparam: string;
  alignmenttype: TFastScanMethod;
  list: tstringlist;
  r: ptruint;
  parameters: integer;
begin
  parameters:=lua_gettop(L);

  if parameters>=2 then
  begin
    module:=Lua_ToString(L,1);
    scanstring:=Lua_ToString(L,2);
    if parameters>=3 then
      protectionflags:=Lua_ToString(L, 3)
    else
      protectionflags:='*X*W*C';

    if parameters>=4 then
      alignmenttype:=TFastScanMethod(lua_tointeger(L, 4))
    else
      alignmenttype:=fsmNotAligned;

    if parameters>=5 then
      alignmentparam:=Lua_ToString(L, 5)
    else
      alignmentparam:='1';

    if scanstring='' then
    begin
      lua_pushstring(L,'Invalid parameter. Must be a string');
      lua_pushnil(L);
      result:=2;
    end;

    r:=findaobInModule(module, scanstring, protectionflags, alignmenttype,alignmentparam,true);
    if r=0 then
      lua_pushnil(L)
    else
      lua_pushinteger(L,r);

    result:=1;
  end
  else
  begin
    lua_pushstring(L,'Not enough parameters');
    lua_pushnil(L);
    result:=2;
  end;
end;

function AOBScanUnique(L: PLua_state): integer; cdecl;
var
  scanstring: string;
  protectionflags: string;
  alignmentparam: string;
  alignmenttype: TFastScanMethod;
  list: tstringlist;
  r: ptruint;
  parameters: integer;
begin
  lua_pushstring(L,'');
  lua_insert(L,1);

  exit(AOBScanModuleUnique(L));
end;



function AOBScan(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  i,b: integer;
  scanstring: string;
  protectionflags: string;
  alignmentparam: string;
  alignmenttype: TFastScanMethod;
  list: tstringlist;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters=0 then exit;

  protectionflags:='';
  alignmenttype:=fsmNotAligned;
  alignmentparam:='1';

  if (parameters>=1) and (lua_isstring(L,1)) then
  begin
    //it's a scanstring, optional call
    scanstring:=Lua_ToString(L, 1);
    if parameters>=2 then
      protectionflags:=Lua_ToString(L, 2);

    if parameters>=3 then
      alignmenttype:=TFastScanMethod(lua_tointeger(L, 3));

    if parameters>=4 then
      alignmentparam:=Lua_ToString(L, 4);


  end
  else
  begin
    //buildup the scanstring
    scanstring:='';
    for i:=-parameters to -1 do
    begin
      b:=lua_tointeger(L,i);

      if (b>255) then scanstring:=scanstring+'* '
      else
      if b=0 then
      begin
        if not lua_isnumber(L,i) then
          scanstring:=scanstring+'* '
        else
          scanstring:=scanstring+'00 '
      end
      else scanstring:=scanstring+inttohex(b,2)+' ';
    end;
  end;
  lua_pop(L, lua_gettop(L));


  list:=tstringlist.create;
  if getaoblist(scanstring, list, protectionflags, alignmenttype, alignmentparam) then
  begin
    result:=1;
    luaclass_newClass(L, list);
  end
  else
  begin
    list.free;
  end;

end;




function getOpenedProcessID(L: PLua_state): integer; cdecl;
begin
  lua_pushinteger(L, processid);
  result:=1;
end;

function getOpenedProcessHandle(L: PLua_state): integer; cdecl;
begin
  lua_pushinteger(L, processhandle);
  result:=1;
end;

function getSymbolInfo(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  symbolname: string;
  mi: TModuleInfo;

  si: TCESymbolInfo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    symbolname:=Lua_ToString(L, 1);
    lua_pop(L, lua_gettop(l));

    if symhandler.GetSymbolInfo(symbolname, si) then
    begin
      pushSymbol(L, @si);
      result:=1;
    end;
  end;
end;

function getModuleSize(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  modulename: string;
  mi: TModuleInfo;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    modulename:=Lua_ToString(L, 1);
    lua_pop(L, lua_gettop(l));

    if symhandler.getmodulebyname(modulename, mi) then
    begin
      lua_pushinteger(L, mi.basesize);
      result:=1;
    end;
  end;
end;

function getAddressSafe(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;

  local,shallow,e: boolean;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    if lua_type(L,1)=LUA_TNUMBER then
    begin
      lua_pushinteger(L,lua_tointeger(L,1));
      exit(1);
    end;

    s:=Lua_ToString(L, 1);

    if parameters>=2 then
      local:=lua_toboolean(L, 2)
    else
      local:=false;

    if parameters>=3 then
      shallow:=lua_toboolean(L, 3)
    else
      shallow:=false;

    lua_pop(L, lua_gettop(l));

    try
      if not local then
        lua_pushinteger(L,symhandler.getAddressFromName(s, waitforsymbols, e, nil, shallow))
      else
        lua_pushinteger(L,selfsymhandler.getAddressFromName(s, waitforsymbols, e, nil, shallow));

      if e then
        result:=0
      else
        result:=1;
    except
      exit(0);
    end;


  end;
end;


function getAddress(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;

  local: boolean;

begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    if lua_type(L,1)=LUA_TNUMBER then
    begin
      lua_pushinteger(L,lua_tointeger(L,1));
      exit(1);
    end;

    s:=Lua_ToString(L, 1);

    if parameters>=2 then
      local:=lua_toboolean(L, 2)
    else
      local:=false;


    lua_pop(L, lua_gettop(l));

    try
      if not local then
        lua_pushinteger(L,symhandler.getAddressFromNameL(s, waitforsymbols))
      else
        lua_pushinteger(L,selfsymhandler.getAddressFromNameL(s, waitforsymbols));
    except
      on e:exception do
      begin
        {$ifdef windows}
          {$ifdef cpu64}
            lua_pushstring(L,e.Message);
            lua_error(L);
          {$else}
            raise;
          {$endif}
        {$else}
          lua_pushnil(L);
          lua_pushstring(L, e.message);
          exit(2);
        {$endif}
      end;

    end;

    result:=1;
  end
  else
  lua_pop(L, lua_gettop(l));
end;

function getNameFromAddress(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;
  address: ptruint;
  modulenames: boolean=true;
  symbols: boolean=true;
  sections: boolean=false;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    address:=lua_toaddress(L,1);

    if (parameters>=2) and (not lua_isnil(L,2)) then modulenames:=lua_toboolean(L,2);
    if (parameters>=3) and (not lua_isnil(L,3)) then symbols:=lua_toboolean(L,3);
    if (parameters>=4) and (not lua_isnil(L,4)) then sections:=lua_toboolean(L,4);

    lua_pop(L, lua_gettop(l));

    lua_pushstring(L,symhandler.getNameFromAddress(address, symbols, modulenames, sections));
    result:=1;
  end
  else lua_pop(L, lua_gettop(l));
end;

function inModule(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    address:=lua_toaddress(L,1);

    lua_pop(L, lua_gettop(l));

    lua_pushboolean(L,symhandler.inModule(address));
    result:=1;
  end
  else lua_pop(L, lua_gettop(l));
end;

function inSystemModule(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: string;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    address:=lua_toaddress(L,1);

    lua_pop(L, lua_gettop(l));

    lua_pushboolean(L,symhandler.inSystemModule(address));
    result:=1;
  end;
end;

function getCommonModuleList(L: PLua_state): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(l));

  luaclass_newClass(L,symhandler.getCommonModuleList);
  result:=1;
end;

function reinitializeDotNetSymbolhandler(L:PLua_state): integer; cdecl;
var modulename: string;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    modulename:=Lua_ToString(L, 1)
  else
    modulename:='';

  symhandler.reinitializeDotNetSymbols(modulename);
end;

function reinitializeSymbolhandler(L: PLua_state): integer; cdecl;
var waittilldone: boolean;
begin
  if lua_gettop(L)>=1 then
    waittilldone:=lua_toboolean(L,1)
  else
    waittilldone:=true;

  lua_pop(L, lua_gettop(L));
  result:=0;


  symhandler.reinitialize(true);

  if waitTillDone then
    symhandler.waitforsymbolsloaded;
end;

function waitForSections(L: PLua_state): integer; cdecl;
begin
  symhandler.waitForSections;
  result:=0;
end;

function waitForExports(L: PLua_state): integer; cdecl;
begin
  symhandler.waitforExports;
  result:=0;
end;


function waitForDotNet(L: PLua_state): integer; cdecl;
begin
  symhandler.waitforDotNet;
  result:=0;
end;

function waitForPDB(L: PLua_state): integer; cdecl;
begin
  symhandler.waitforpdb;
  result:=0;
end;

function searchPDBWhileLoading(L: PLua_state): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
    symhandler.searchPDBWhileLoading(lua_toboolean(L,1));

  result:=0;
end;

function reinitializeSelfSymbolhandler(L: PLua_state): integer; cdecl;
var waittilldone: boolean;
begin
  if lua_gettop(L)>=1 then
    waittilldone:=lua_toboolean(L,1)
  else
    waittilldone:=true;

  lua_pop(L, lua_gettop(L));
  result:=0;


  selfsymhandler.reinitialize(true);

  if waitTillDone then
    selfsymhandler.waitforsymbolsloaded;
end;

function enumModules(L:PLua_state): integer; cdecl;
var
  i: integer;
  ml: tstringlist;
  tableindex: integer;
  entryindex: integer;

  ths: THandle;
  me32: TModuleEntry32;

  is64bitmodule: boolean;

  pid: integer;

  st: qword;
  tt: qword;

 { ar: lua_debug;
  s: pchar;}
begin
 { if MainThreadID=GetCurrentThreadId then
  begin
    ZeroMemory(@ar,sizeof(ar));
    lua_getstack(L, 1,@ar);
    lua_getinfo(L,'S',@ar);

    showmessage(ar.source);
  end;}

  result:=0;

  if lua_gettop(L)=1 then
    pid:=lua_tointeger(L,1)
  else
    pid:=processid;

  tt:=GetTickCount64;
  st:=GetTickCount64;
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE or TH32CS_SNAPMODULE32, pid);
 // OutputDebugString('CreateToolhelp32Snapshot took '+inttostr(GetTickCount64-st)+' ms.');
  if ths<>0 then
  begin
    lua_newtable(L);
    tableindex:=lua_gettop(L);

    me32.dwSize:=sizeof(MODULEENTRY32);



    i:=1;
    st:=GetTickCount64;
    if module32first(ths,me32) then
    repeat
     // OutputDebugString('module32first/next took '+inttostr(GetTickCount64-st)+' ms.');
      lua_newtable(L);
      entryindex:=lua_gettop(L);

      lua_pushstring(L, 'Name');
      lua_pushstring(L, extractfilename(me32.szExePath));
      lua_settable(L, entryindex);

      lua_pushstring(L, 'Address');
      lua_pushinteger(L, ptruint(me32.modBaseAddr));
      lua_settable(L, entryindex);

      is64bitmodule:=processhandler.is64Bit;

      {$ifdef windows}
      if (processhandler.isNetwork) or (peinfo_is64bitfile(me32.szExePath, is64bitmodule)=false) then
        is64bitmodule:=processhandler.is64Bit; //fallback on an assumption
      {$endif}

      lua_pushstring(L, 'Is64Bit');
      lua_pushboolean(L, is64bitmodule);
      lua_settable(L, entryindex);

      lua_pushstring(L, 'PathToFile');
      lua_pushstring(L, me32.szExePath);
      lua_settable(L, entryindex);


      lua_pushinteger(L, i);
      lua_pushvalue(L, entryindex);
      lua_settable(L, tableindex);

      lua_pop(L, 1); //remove the current entry table

      inc(i);
      st:=GetTickCount64;
    until Module32Next(ths, me32)=false;

    lua_pushvalue(L, tableindex); //shouldn't be needed, but let's make sure
    result:=1;

    closehandle(ths);

  end;

  //OutputDebugString('enumModules took '+inttostr(GetTickCount64-tt)+' ms.');
end;




function getSettingsForm(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  luaclass_newClass(l, formSettings);
end;

function getMemoryViewForm(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  luaclass_newClass(l, MemoryBrowser);
end;


function getMainForm(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  luaclass_newClass(l, mainform);
end;




function getAddressList(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  luaclass_newClass(l, mainform.addresslist);
end;

function getFreezeTimer(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  luaclass_newClass(l, mainform.FreezeTimer);
end;

function getUpdateTimer(L: PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pop(L, lua_gettop(l));
  luaclass_newClass(l, mainform.UpdateTimer);
end;

function setGlobalKeyPollInterval(L: PLua_state): integer; cdecl;
begin
  if lua_gettop(L)=1 then
    hotkeyPollInterval:=lua_tointeger(L, -1);

  result:=0;
end;

function setGlobalDelayBetweenHotkeyActivation(L: PLua_state): integer; cdecl;
begin
  if lua_gettop(L)=1 then
    hotkeyIdletime:=lua_tointeger(L, -1);

  result:=0;
end;



function inheritsFromObject(L: PLua_state): integer; cdecl;
var x: TObject;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    x:=lua_toceuserdata(L, 1);
    lua_pop(L, lua_gettop(l));

    if x<>nil then
    begin
      result:=1;
      lua_pushboolean(l, (x is TObject));
    end;

  end;
end;

function inheritsFromComponent(L: PLua_state): integer; cdecl;
var x: TObject;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    x:=lua_toceuserdata(L, 1);
    lua_pop(L, lua_gettop(l));

    if x<>nil then
    begin
      result:=1;
      lua_pushboolean(l, (x is TComponent));
    end;

  end;
end;

function inheritsFromControl(L: PLua_state): integer; cdecl;
var x: TObject;
  r: boolean;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    x:=lua_toceuserdata(L, 1);
    lua_pop(L, lua_gettop(l));

    if x<>nil then
    begin
      result:=1;
      r:=x is TControl;
      lua_pushboolean(l, r);
    end;

  end;
end;

function inheritsFromWinControl(L: PLua_state): integer; cdecl;
var x: TObject;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    x:=lua_toceuserdata(L, 1);
    lua_pop(L, lua_gettop(l));

    if x<>nil then
    begin
      result:=1;
      lua_pushboolean(l, (x is TWinControl));
    end;

  end;
end;


function createToggleBox(L: Plua_State): integer; cdecl;
var
  ToggleBox: TCEToggleBox;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_toceuserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  ToggleBox:=TCEToggleBox.Create(owner);
  if owner<>nil then
    ToggleBox.Parent:=owner;

  luaclass_newClass(L, ToggleBox);
  result:=1;
end;


function opendialog_execute(L: Plua_State): integer; cdecl; //6.2- version only
var
  opendialog: TOpenDialog;
begin
  opendialog:=luaclass_getClassObject(L);
  if opendialog.execute then
    lua_pushnil(L)
  else
    lua_pushstring(L, opendialog.FileName);

  result:=1;
end;

function createOpenDialog(L: Plua_State): integer; cdecl;
var
  o: TOpenDialog;
begin
  result:=0;

  if lua_gettop(L)>=1 then
    luaclass_newClass(L, TOpenDialog.create(lua_toceuserdata(L, 1)))
  else
    luaclass_newClass(L, TOpenDialog.create(nil));

  result:=1;
end;

function createSaveDialog(L: Plua_State): integer; cdecl;
var
  o: TSaveDialog;
begin
  result:=0;

  if lua_gettop(L)>=1 then
    luaclass_newClass(L, TSaveDialog.create(lua_toceuserdata(L, 1)))
  else
    luaclass_newClass(L, TSaveDialog.create(nil));

  result:=1;
end;

function CreateSelectDirectoryDialog(L: Plua_State): integer; cdecl;
var
  o: TSaveDialog;
begin
  result:=0;

  if lua_gettop(L)>=1 then
    luaclass_newClass(L, TSelectDirectoryDialog.create(lua_toceuserdata(L, 1)))
  else
    luaclass_newClass(L, TSelectDirectoryDialog.create(nil));

  result:=1;
end;


function createMemoryStream(L: Plua_State): integer; cdecl;
begin
  luaclass_newClass(L, TMemoryStream.create);
  result:=1;
end;

function createFileStream(L: Plua_State): integer; cdecl;
var
  filename: string;
  mode: word;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    filename:=lua_tostring(L, 1);
    if lua_gettop(L)>=2 then
      mode:=lua_tointeger(L, 2)
    else
    begin
      //in case the user forgets or doesn't care
      if FileExists(filename) then
        mode:=fmOpenReadWrite or fmShareDenyNone
      else
        mode:=fmCreate;
    end;

    try
      luaclass_newClass(L, TFileStream.create(filename, mode));
      result:=1;
    except
      on e:exception do
      begin
        lua_pushnil(L);
        lua_pushstring(L,e.Message);
        result:=2;
      end;
    end;

  end;
end;

{
function createStringStream(L: Plua_State): integer; cdecl;
var s: pchar;
  //sl: size_t;
  ss: TStringStream;
  b: TBytes;

  stringlength: psize_t;
begin
  getmem(stringlength,16);



  if lua_gettop(L)>0 then
    s:=lua_tolstring(L, 1, stringlength)
  else
    s:=nil;

  setlength(b,0);
  ss:=TStringStream.create('',TEncoding.Default,false); //(s);
  if (s<>nil) and (stringlength^>0) then
  begin
    ss.WriteBuffer(s^, stringlength^);
    ss.position:=0;
  end;

  freemem(stringlength);

  luaclass_newClass(L, ss);
  result:=1;
end;
}

function createStringStream(L: Plua_State): integer; cdecl;
var s: pchar;
  sl: size_t=0;
  ss: TStringStream;
begin
  if lua_gettop(L)>0 then
    s:=lua_tolstring(L, 1, @sl)
  else
    s:=nil;

  ss:=TStringStream.create(''{$if FPC_FULLVERSION>=030200},TEncoding.Default,false{$endif});
  if (s<>nil) and (sl>0) then
  begin
    ss.WriteBuffer(s^, sl);
    ss.position:=0;
  end;

  luaclass_newClass(L, ss);       //lua_pushinteger(L,ptruint(ss));

  result:=1;
end;



function readRegionFromFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  filename: string;
  address: ptruint;
  size: integer;
  x: PtrUInt;
  buf: pointer;
  f: tmemorystream;
begin
  result:=0;
  x:=0;
  f:=nil;

  parameters:=lua_gettop(L);
  if (parameters=2) then
  begin
    filename:=Lua_ToString(L, 1);
    address:=lua_toaddress(L,2);

    lua_pop(L, lua_gettop(L));

    f:=tmemorystream.create;
    try
      f.LoadFromFile(filename);
      writeprocessmemory(processhandle, pointer(address), f.memory, f.size, x);

    finally
      freeandnil(f);
    end;


    result:=1;
    lua_pushinteger(L,x);
  end else lua_pop(L, lua_gettop(L));
end;


function writeRegionToFile(L: Plua_State): integer; cdecl;
var parameters: integer;
  filename: string;
  address: ptruint;
  size: integer;
  x: PtrUInt;
  buf: pointer;
  f: tfilestream;
begin
  result:=0;
  x:=0;
  f:=nil;

  parameters:=lua_gettop(L);
  if (parameters=3) then
  begin
    filename:=Lua_ToString(L, -3);
    address:=lua_toaddress(L,-2);

    size:=lua_tointeger(L,-1);
    lua_pop(L, lua_gettop(L));

    getmem(buf,size);
    try

      readprocessmemory(processhandle, pointer(address), buf, size, x);

      f:=tfilestream.create(filename, fmCreate);
      f.WriteBuffer(buf^, x);

    finally
      if f<>nil then
        FreeAndNil(f);

      freememandnil(buf);
    end;


    result:=1;
    lua_pushinteger(L,x);
  end else lua_pop(L, lua_gettop(L));
end;


function registerSymbol(L: Plua_State): integer; cdecl;
var parameters: integer;
  symbolname: string;
  address: string;
  donotsave: boolean;
begin
  result:=1;

  parameters:=lua_gettop(L);
  if (parameters>=2) then
  begin
    symbolname:=Lua_ToString(L, 1);
    if lua_isstring(L, 2) then
      address:=lua_tostring(L,2)
    else
      address:=IntToHex(lua_tointeger(L,2),1);


    donotsave:=(parameters>=3) and (lua_toboolean(L, 3));

    try
      symhandler.DeleteUserdefinedSymbol(symbolname);
      symhandler.AddUserdefinedSymbol(address, symbolname, donotsave);
      lua_pushboolean(L,true);
    except
      on e: exception do
      begin
        lua_pushboolean(L,false);
        lua_pushstring(L,e.message);
        result:=2;
      end;
    end;
  end;
end;

function unregisterSymbol(L: Plua_State): integer; cdecl;
var parameters: integer;
  symbolname: string;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if (parameters=1) then
  begin
    symbolname:=Lua_ToString(L, 1);
    symhandler.DeleteUserdefinedSymbol(symbolname);
  end;

  lua_pop(L, lua_gettop(L));
end;

function resetLuaState(L: Plua_State): integer; cdecl;
begin
  result:=0;
  lua_pop(L, lua_gettop(L));
  InitializeLua; //this creates a NEW lua state (cut doesn't destroy the current one)
end;

function reloadSettingsFromRegistry(L: Plua_State): integer; cdecl;
begin
  result:=0;
  LoadSettingsFromRegistry;
end;

function createMemScan(L: Plua_State): integer; cdecl;
var
  progressbar: TCustomProgressbar;
  memscan: TMemScan;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
    progressbar:=lua_toceuserdata(L, -1)
  else
    progressbar:=nil;

  lua_pop(L, lua_gettop(L));

  if (progressbar<>nil) and (progressbar is TCustomProgressBar=false) then
    raise exception.create(rsCreateMemScanNeedsAProgressbarOrNil+progressbar.ClassName+rsIsNotAProgressbar);

  memscan:=TMemscan.create(progressbar);

  luaclass_newClass(L, memscan);
  result:=1;
end;

function getCurrentMemscan(L: Plua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));

  luaclass_newClass(L, mainform.memscan);
  result:=1;
end;





function supportCheatEngine(L: Plua_State): integer; cdecl;
var
  parameters: integer;
  //attachwindow, hasclosebutton, width, height, position ,yoururl OPTIONAL, extraparameters OPTIONAL, percentageshown OPTIONAL
  attachwindow: TCustomForm;
  hasCloseButton: boolean;
  width: integer;
  height: integer;
  position: integer;
  yoururl: string;
  extraparameters: string;
  percentageshown: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=5 then
  begin
    attachwindow:=lua_toceuserdata(L, 1);
    hasCloseButton:=lua_toboolean(L, 2);
    width:=lua_tointeger(L, 3);
    height:=lua_tointeger(L, 4);
    position:=lua_tointeger(L, 5);

    if parameters>=6 then
      yoururl:=Lua_ToString(L, 6)
    else
      yoururl:='';

    if parameters>=7 then
      extraparameters:=Lua_ToString(L, 7)
    else
      extraparameters:='';

    if parameters>=8 then
      percentageshown:=lua_tointeger(L, 8)
    else
      percentageshown:=0;

    lua_pop(L, lua_gettop(L));

    if adwindow=nil then
      adwindow:=TADWindow.Create2(Application, hasclosebutton);

    adwindow.clientWidth:=width;
    adwindow.clientheight:=height;
    adwindow.show;
    adwindow.AttachToForm(attachwindow);
    case position of
      0: adwindow.setPosition(akTop);
      1: adwindow.setPosition(akRight);
      2: adwindow.setPosition(akBottom);
      3: adwindow.setPosition(akLeft);
    end;

    adwindow.setUserUrl(yoururl);
    adwindow.setUserPercentage(percentageshown);
    adwindow.optional:=extraparameters;

    adwindow.LoadAdNow;


  end else lua_pop(L, lua_gettop(L));
end;

function fuckCheatEngine(L: Plua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  if adwindow<>nil then
    adWindow.visible:=false;

  result:=0;
end;


function dbk_initialize(L: Plua_State): integer; cdecl;
var
  state,x: BOOL;
  reason: string;
begin
  result:=0;
  {$IFDEF windows}
  if isDriverLoaded(nil)=false then
  begin
    reason:='A lua script wants to load the driver. Reason:';
    if lua_gettop(L)>=1 then
      reason:=Lua_ToString(L,1)
    else
      reason:='No reason';

    LoadDBK32;
    state:=isDriverLoaded(@x);
    lua_pushboolean(L, state);
    if state then
      lua_pushinteger(L, hdevice)
    else
      lua_pushnil(L);

    result:=2;
  end
  else
  begin
    lua_pushboolean(L,true);
    result:=1;
  end;
  {$ENDIF}

end;

function dbk_useKernelmodeOpenProcess(L: Plua_State): integer; cdecl;
begin
  {$IFDEF windows}
  UseDBKOpenProcess;
  {$ENDIF}
  result:=0;
end;

function dbk_useKernelmodeProcessMemoryAccess(L: Plua_State): integer; cdecl;
begin
  {$IFDEF windows}
  UseDBKReadWriteMemory;
  {$ENDIF}
  result:=0;
end;

function dbk_useKernelmodeQueryMemoryRegions(L: Plua_State): integer; cdecl;
begin
  {$IFDEF windows}
  UseDBKQueryMemoryRegion;
  {$ENDIF}
  result:=0;
end;

function dbk_usePhysicalMemoryAccess(L: Plua_State): integer; cdecl;
begin
  {$IFDEF windows}
  if dbvm_version<>0 then
    DBKPhysicalMemoryDBVM
  else
    DBKPhysicalMemory;

  MainForm.ProcessLabel.Caption:=strPhysicalMemory;
  {$ENDIF}
  result:=0;
end;

function dbk_setSaferPhysicalMemoryScanning(L: Plua_State): integer; cdecl;
begin
  {$IFDEF windows}
  saferQueryPhysicalMemory:=lua_toboolean(L,1);
  {$ENDIF}
  result:=0;
end;


function lua_dbk_readphysicalmemory(L: PLua_state): integer; cdecl;
var
  PhysicalAddress: qword;
  buffer:pointer;
  size:integer;
  i: integer;
  br: ptruint;
begin
  result:=0;

  {$IFDEF windows}
  if lua_gettop(L)<2 then raise exception.create('not all parameters given');
  PhysicalAddress:=lua_tointeger(L,1);
  size:=lua_tointeger(L,2);
  getmem(buffer,size);

  br:=0;
  ReadPhysicalMemory(qword(-1),pointer(PhysicalAddress), buffer, size, br);
  if size=br then
  begin
    CreateByteTableFromPointer(L, buffer,size);
    freemem(buffer);
    exit(1);
  end
  else
  begin
    freemem(buffer);
    exit(0);
  end;
  {$ENDIF}
end;

function lua_dbk_writephysicalmemory(L: PLua_state): integer; cdecl;
var
  PhysicalAddress: qword;
  buffer:pointer;
  size:integer;
  i: integer;
  bw: ptruint;
begin
  result:=0;

  {$IFDEF windows}
  if lua_gettop(L)<2 then raise exception.create('not all parameters given');
  PhysicalAddress:=lua_tointeger(L,1);
  if lua_istable(L,2)=false then raise exception.create('2nd parameter needs to be a bytetable');
  size:=lua_objlen(L, 2);
  getmem(buffer,size);
  readBytesFromTable(L,2,buffer,size);

  bw:=0;
  WritePhysicalMemory(qword(-1),pointer(PhysicalAddress), buffer, size, bw);
  if size=bw then
    lua_pushboolean(L,true)
  else
    lua_pushboolean(L,false);

  freemem(buffer);

  exit(1);
  {$ENDIF}

end;


function dbk_getPEProcess(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  pid: dword;
begin
  result:=0;
  {$IFDEF windows}
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    pid:=lua_tointeger(L,-1);

    lua_pushinteger(L, GetPEProcess(pid));
    result:=1;
  end else lua_pop(L, parameters);
  {$ENDIF}
end;

function dbk_getPEThread(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  pid: dword;
begin
  result:=0;
  {$IFDEF windows}
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    pid:=lua_tointeger(L,-1);

    lua_pushinteger(L, GetPEThread(pid));
    result:=1;
  end else lua_pop(L, parameters);
  {$ENDIF}
end;

function dbk_executeKernelMemory(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  address: ptruint;
  parameter: ptruint;
begin
  result:=0;
  {$IFDEF windows}
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    address:=lua_toaddress(L,-parameters);

    if parameters>=2 then
    begin
      if lua_isstring(L, -parameters+1) then
        parameter:=symhandler.getAddressFromNameL(Lua_ToString(L,-parameters+1), waitforsymbols)
      else
        parameter:=lua_tointeger(L, -parameters+1);
    end
    else
      parameter:=0;

    lua_pop(L, parameters);


    executeKernelCode(address,parameter);

    result:=0;
  end else lua_pop(L, parameters);
  {$ENDIF}
end;

function dbk_writesIgnoreWriteProtection(L: PLua_State): integer; cdecl;
var state: boolean;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)=1 then
  begin
    state:=lua_toboolean(L, 1);
    lua_pushboolean(L, KernelWritesIgnoreWriteProtection(state));
    result:=1;
  end;
  {$ENDIF}
end;

function lua_getPhysicalAddressCR3(L: PLua_State): integer; cdecl;
var
  CR3: QWORD;
  VirtualAddress: QWORD;
  PhysicalAddress: QWORD;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=2 then
  begin
    CR3:=lua_tointeger(L,1);
    VirtualAddress:=lua_tointeger(L,2);
    if VirtualToPhysicalCR3(CR3, VirtualAddress, PhysicalAddress) then
    begin
      lua_pushinteger(L, PhysicalAddress);
      result:=1;
    end;
  end;
  {$ENDIF}
end;

function lua_readProcessMemoryCR3(L: PLua_State): integer; cdecl;
var
  CR3: QWORD;
  Address: QWORD;
  Size: integer;
  x: ptruint;
  buf: pointer;

begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=3 then
  begin
    CR3:=lua_tointeger(L,1);
    Address:=lua_tointeger(L,2);
    Size:=lua_tointeger(L,3);

    getmem(buf, size);
    x:=0;
    ReadProcessMemoryCR3(cr3, pointer(address), buf, size, x);
    if x>0 then
    begin
      CreateByteTableFromPointer(L, buf, x);
      result:=1;
      freememandnil(buf);
    end;
  end;
  {$ENDIF}
end;

function lua_writeProcessMemoryCR3(L: PLua_State): integer; cdecl;
var
  CR3: QWORD;
  Address: QWORD;
  size: integer;
  buf: pointer;
  x: ptruint;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=3 then
  begin
    CR3:=lua_tointeger(L,1);
    Address:=lua_tointeger(L,2);
    if lua_istable(L,3) then
    begin
      size:=lua_objlen(L, 3);
      getmem(buf,size);
      readBytesFromTable(L, 3, buf, size);

      x:=0;
      WriteProcessMemoryCR3(cr3, pointer(Address), buf, size, x);
      freememandnil(buf);

      if (x>0) then
      begin
        lua_pushboolean(L,true);
        exit(1);
      end;
    end;
  end;

  lua_pushboolean(L, false);
  result:=1;
  {$ENDIF}
end;

function dbk_getPhysicalAddress(L: PLua_State): integer; cdecl;
var
  address: ptruint;
  pa: qword;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)=1 then
  begin
    address:=lua_toaddress(L,1);

    if GetPhysicalAddress(processhandle, pointer(address), pa) then
    begin
      lua_pushinteger(L, pa);
      result:=1;
    end;
  end;
  {$ENDIF}
end;

function dbk_getCR0(L: PLua_state): integer; cdecl;
begin
  result:=0;
  {$IFDEF windows}
  lua_pushinteger(L, getcr0);
  result:=1;
  {$ENDIF}
end;

function dbk_getCR3(L: PLua_state): integer; cdecl;
var cr3: qword;
begin
  result:=0;
  {$IFDEF windows}
  if GetCR3(processhandle, cr3) then
    lua_pushinteger(L, cr3)
  else
    lua_pushnil(L);

  result:=1;
  {$ENDIF}
end;

function dbk_getCR4(L: PLua_state): integer; cdecl;
begin
  result:=0;
  {$IFDEF windows}
  lua_pushinteger(L, getcr4);
  result:=1;
  {$ENDIF}
end;

function dbvm_getCR0(L: PLua_state): integer; cdecl;
begin
  result:=0;
  {$IFDEF windows}
  lua_pushinteger(L, dbvm_getRealCR0);
  result:=1;
  {$ENDIF}
end;

function dbvm_getCR3(L: PLua_state): integer; cdecl;
begin
  result:=0;
  {$IFDEF windows}
  lua_pushinteger(L, dbvm_getRealCR3);
  result:=1;
  {$ENDIF}
end;

function dbvm_getCR4(L: PLua_state): integer; cdecl;
begin
  lua_pushinteger(L, dbvm_getRealCR4);
  result:=1;
end;

function lua_dbk_test(L: PLua_state): integer; cdecl;
begin
  result:=0;
  {$IFDEF windows}
  dbk_test;
  {$ENDIF}
end;


function lua_dbvm_jtagbp(L: PLua_state): integer; cdecl;
begin
  lua_pushboolean(L, dbvm_jtagbp);
  result:=1;
end;

function lua_dbvm_readMSR(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  msr: dword;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    msr:=lua_tointeger(L,1);
    lua_pushinteger(L, dbvm_readMSR(msr));
    result:=1;
  end;
end;

function lua_dbvm_writeMSR(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  msr: dword;
  msrvalue: qword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    msr:=lua_tointeger(L,1);
    msrvalue:=lua_tointeger(L,2);
    dbvm_writeMSR(msr, msrvalue);
  end;

  lua_pop(L, parameters);
end;

function lua_dbvm_writephysicalmemory(L: PLua_state): integer; cdecl;
var
  PhysicalAddress: qword;
  buffer:pointer;
  size:integer;
  i: integer;
begin
  if lua_gettop(L)<2 then raise exception.create('not all parameters given');
  PhysicalAddress:=lua_tointeger(L,1);
  if lua_istable(L,2)=false then raise exception.create('2nd parameter needs to be a bytetable');
  size:=lua_objlen(L, 2);
  getmem(buffer,size);
  readBytesFromTable(L,2,buffer,size);

  i:=dbvm_write_physical_memory(PhysicalAddress, buffer, size);
  if size=i then
    lua_pushboolean(L,true)
  else
    lua_pushboolean(L,false);

  freemem(buffer);

  exit(1);

end;

function lua_dbvm_readphysicalmemory(L: PLua_state): integer; cdecl;
var
  PhysicalAddress: qword;
  buffer:pointer;
  size:integer;
  i: integer;
begin
  if lua_gettop(L)<2 then raise exception.create('not all parameters given');
  PhysicalAddress:=lua_tointeger(L,1);
  size:=lua_tointeger(L,2);
  getmem(buffer,size);

  i:=dbvm_read_physical_memory(PhysicalAddress, buffer, size);
  if size=i then
  begin
    CreateByteTableFromPointer(L, buffer,size);
    freemem(buffer);
    exit(1);
  end
  else
  begin
    freemem(buffer);
    raise exception.create('not all memory read');
  end;
end;

function lua_dbvm_psod(L: PLua_state): integer; cdecl;
begin
  dbvm_psod;
  result:=1;
  lua_pushstring(L,'WTF');
end;

function lua_dbvm_getNMIcount(L: PLua_state): integer; cdecl;
begin
  lua_pushinteger(L, dbvm_getNMIcount);
  result:=1;
end;

function lua_dbvm_debug_setSpinlockTimeout(L: PLua_state): integer; cdecl;
var timeout: qword;
begin
  timeout:=lua_tointeger(L,1);
  dbvm_debug_setSpinlockTimeout(timeout);
  result:=0;
end;

function lua_dbvm_get_statistics(L: PLua_state): integer; cdecl;
var
  stats: TDBVMStatistics;
  i: integer;
  count: qword;
begin
  result:=0;
  count:=dbvm_get_statistics(stats);

  lua_newtable(L);
  //
  lua_pushstring(L,'Local');
  lua_newtable(L);
  for i:=0 to 55 do
  begin
    lua_pushinteger(L,i);
    lua_pushinteger(L,stats.eventCountersCurrentCPU[i]);
    lua_settable(L,-3);
  end;
  lua_settable(L,-3);

  lua_pushstring(L,'Global');
  lua_newtable(L);
  for i:=0 to 55 do
  begin
    lua_pushinteger(L,i);
    lua_pushinteger(L,stats.eventCountersAllCPUS[i]);
    lua_settable(L,-3);
  end;
  lua_settable(L,-3);


  lua_pushinteger(L,count);
  result:=2;

end;

function lua_dbvm_watch_writes(L: PLua_state): integer; cdecl;
var
  physicalAddress: qword=0;
  size: integer;
  options: DWORD;
  MaxEntryCount: integer;

  top: integer;

  usermodeLoop: qword=0;
  kernelmodeLoop: qword=0;
begin
  top:=lua_gettop(L);
  if top>=1 then
    physicalAddress:=lua_tointeger(L,1)
  else
  begin
    lua_pushstring(L, 'dbvm_watch_writes needs a physical address');
    lua_error(L);
  end;

  if top>=2 then
    size:=lua_tointeger(L,2)
  else
    size:=4;

  if top>=3 then
    options:=lua_tointeger(L,3)
  else
    options:=0;

  if top>=4 then
    MaxEntryCount:=lua_tointeger(L,4)
  else
    MaxEntryCount:=16;


  if (options and EPTO_DBVMBP) = EPTO_DBVMBP then
  begin
    //needs usermode field
    if top>=5 then
      usermodeloop:=lua_tointeger(L,5)
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'option DBVMBP needs a usermode loop address');
      exit(2);
    end;

    if top>=6 then
      kernelmodeLoop:=lua_tointeger(L,6); //NOT recommended.  If CE writes it, CE will freeze.
  end;


  lua_pushinteger(L, dbvm_watch_writes(physicalAddress, Size, Options, MaxEntryCount,UserModeLoop, kernelmodeloop));   //there is no kernelmode loop, so those will be skipped
  result:=1;
end;

function lua_dbvm_watch_reads(L: PLua_state): integer; cdecl;
var
  physicalAddress: qword=0;
  size: integer;
  options: DWORD;
  MaxEntryCount: integer;
  usermode :qword;
  top: integer;


  usermodeLoop: qword=0;
  kernelmodeLoop: qword=0;
begin
  top:=lua_gettop(L);
  if top>=1 then
    physicalAddress:=lua_tointeger(L,1)
  else
  begin
    lua_pushstring(L, 'dbvm_watch_reads needs a physical address');
    lua_error(L);
  end;

  if top>=2 then
    size:=lua_tointeger(L,2)
  else
    size:=4;

  if top>=3 then
    options:=lua_tointeger(L,3)
  else
    options:=0;

  if top>=4 then
    MaxEntryCount:=lua_tointeger(L,4)
  else
    MaxEntryCount:=16;

  if (options and EPTO_DBVMBP) = EPTO_DBVMBP then
  begin
    //needs usermode field
    if top>=5 then
      usermodeloop:=lua_tointeger(L,5)
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'option DBVMBP needs a usermode loop address');
      exit(2);
    end;

    if top>=6 then
      kernelmodeLoop:=lua_tointeger(L,6); //NOT recommended.  If CE writes it, CE will freeze.
  end;

  lua_pushinteger(L, dbvm_watch_reads(physicalAddress, Size, Options, MaxEntryCount, usermodeLoop, kernelmodeloop));
  result:=1;
end;

function lua_dbvm_watch_executes(L: PLua_state): integer; cdecl;
var
  physicalAddress: qword=0;
  size: integer;
  options: DWORD;
  MaxEntryCount: integer;

  top: integer;

  usermodeLoop: qword=0;
  kernelmodeLoop: qword=0;
begin
  top:=lua_gettop(L);
  if top>=1 then
    physicalAddress:=lua_tointeger(L,1)
  else
  begin
    lua_pushstring(L, 'dbvm_watch_reads needs a physical address');
    lua_error(L);
  end;

  if top>=2 then
    size:=lua_tointeger(L,2)
  else
    size:=4;

  if top>=3 then
    options:=lua_tointeger(L,3)
  else
    options:=0;

  if top>=4 then
    MaxEntryCount:=lua_tointeger(L,4)
  else
    MaxEntryCount:=16;


  if (options and EPTO_DBVMBP) = EPTO_DBVMBP then
  begin
    //needs usermode field
    if top>=5 then
      usermodeloop:=lua_tointeger(L,5)
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'option DBVMBP needs a usermode loop address');
      exit(2);
    end;

    if top>=6 then
      kernelmodeLoop:=lua_tointeger(L,6);
  end;



  lua_pushinteger(L, dbvm_watch_executes(physicalAddress, Size, Options, MaxEntryCount, usermodeloop, kernelmodeloop));
  result:=1;
end;

procedure lua_push_watch_basic_fields(L: PLua_state; pbasic: PPageEventBasic; index: integer);
begin
  lua_pushstring(L,'VirtualAddress');
  lua_pushinteger(L,pbasic^.VirtualAddress);
  lua_settable(L,index);

  lua_pushstring(L,'PhysicalAddress');
  lua_pushinteger(L,pbasic^.PhysicalAddress);
  lua_settable(L,index);

  lua_pushstring(L,'CR3');
  lua_pushinteger(L,pbasic^.CR3);
  lua_settable(L,index);

  lua_pushstring(L,'FSBASE');
  lua_pushinteger(L,pbasic^.FSBASE);
  lua_settable(L,index);

  lua_pushstring(L,'GSBASE');
  lua_pushinteger(L,pbasic^.GSBASE);
  lua_settable(L,index);

  lua_pushstring(L,'GSBASE_KERNEL');
  lua_pushinteger(L,pbasic^.GSBASE_KERNEL);
  lua_settable(L,index);

  lua_pushstring(L,'FLAGS');
  lua_pushinteger(L,pbasic^.FLAGS);
  lua_settable(L,index);

  lua_pushstring(L,'RAX');
  lua_pushinteger(L,pbasic^.RAX);
  lua_settable(L,index);

  lua_pushstring(L,'RBX');
  lua_pushinteger(L,pbasic^.RBX);
  lua_settable(L,index);

  lua_pushstring(L,'RCX');
  lua_pushinteger(L,pbasic^.RCX);
  lua_settable(L,index);

  lua_pushstring(L,'RDX');
  lua_pushinteger(L,pbasic^.RDX);
  lua_settable(L,index);

  lua_pushstring(L,'RSI');
  lua_pushinteger(L,pbasic^.RSI);
  lua_settable(L,index);

  lua_pushstring(L,'RDI');
  lua_pushinteger(L,pbasic^.RDI);
  lua_settable(L,index);

  lua_pushstring(L,'R8');
  lua_pushinteger(L,pbasic^.R8);
  lua_settable(L,index);

  lua_pushstring(L,'R9');
  lua_pushinteger(L,pbasic^.R9);
  lua_settable(L,index);

  lua_pushstring(L,'R10');
  lua_pushinteger(L,pbasic^.R10);
  lua_settable(L,index);

  lua_pushstring(L,'R11');
  lua_pushinteger(L,pbasic^.R11);
  lua_settable(L,index);

  lua_pushstring(L,'R12');
  lua_pushinteger(L,pbasic^.R12);
  lua_settable(L,index);

  lua_pushstring(L,'R13');
  lua_pushinteger(L,pbasic^.R13);
  lua_settable(L,index);

  lua_pushstring(L,'R14');
  lua_pushinteger(L,pbasic^.R14);
  lua_settable(L,index);

  lua_pushstring(L,'R15');
  lua_pushinteger(L,pbasic^.R15);
  lua_settable(L,index);

  lua_pushstring(L,'RBP');
  lua_pushinteger(L,pbasic^.RBP);
  lua_settable(L,index);

  lua_pushstring(L,'RSP');
  lua_pushinteger(L,pbasic^.RSP);
  lua_settable(L,index);

  lua_pushstring(L,'RIP');
  lua_pushinteger(L,pbasic^.RIP);
  lua_settable(L,index);

  lua_pushstring(L,'DR0');
  lua_pushinteger(L,pbasic^.DR0);
  lua_settable(L,index);

  lua_pushstring(L,'DR1');
  lua_pushinteger(L,pbasic^.DR1);
  lua_settable(L,index);

  lua_pushstring(L,'DR2');
  lua_pushinteger(L,pbasic^.DR2);
  lua_settable(L,index);

  lua_pushstring(L,'DR3');
  lua_pushinteger(L,pbasic^.DR3);
  lua_settable(L,index);

  lua_pushstring(L,'DR6');
  lua_pushinteger(L,pbasic^.DR6);
  lua_settable(L,index);

  lua_pushstring(L,'DR7');
  lua_pushinteger(L,pbasic^.DR7);
  lua_settable(L,index);

  lua_pushstring(L,'CS');
  lua_pushinteger(L,pbasic^.CS);
  lua_settable(L,index);

  lua_pushstring(L,'DS');
  lua_pushinteger(L,pbasic^.DS);
  lua_settable(L,index);

  lua_pushstring(L,'ES');
  lua_pushinteger(L,pbasic^.ES);
  lua_settable(L,index);

  lua_pushstring(L,'SS');
  lua_pushinteger(L,pbasic^.SS);
  lua_settable(L,index);

  lua_pushstring(L,'FS');
  lua_pushinteger(L,pbasic^.FS);
  lua_settable(L,index);

  lua_pushstring(L,'GS');
  lua_pushinteger(L,pbasic^.GS);
  lua_settable(L,index);

  lua_pushstring(L,'Count');
  lua_pushinteger(L,pbasic^.Count);
  lua_settable(L,index);
end;

procedure lua_push_watch_fxsave_fields(L: PLua_state; fpudata: PFXSAVE64; index: integer);
var index2: integer;
begin
  lua_pushstring(L,'FXSAVE64');
  lua_createtable(L,0,32);
  index2:=lua_gettop(L); //should be index+2

  lua_pushstring(L,'FCW');
  lua_pushinteger(L,fpudata^.FCW);
  lua_settable(L,index2);

  lua_pushstring(L,'FSW');
  lua_pushinteger(L,fpudata^.FSW);
  lua_settable(L,index2);

  lua_pushstring(L,'FTW');
  lua_pushinteger(L,fpudata^.FTW);
  lua_settable(L,index2);

  lua_pushstring(L,'FOP');
  lua_pushinteger(L,fpudata^.FOP);
  lua_settable(L,index2);

  lua_pushstring(L,'IP');
  lua_pushinteger(L,fpudata^.FPU_IP);
  lua_settable(L,index2);

  lua_pushstring(L,'DP');
  lua_pushinteger(L,fpudata^.FPU_DP);
  lua_settable(L,index2);

  lua_pushstring(L,'MXCSR');
  lua_pushinteger(L,fpudata^.MXCSR);
  lua_settable(L,index2);

  lua_pushstring(L,'MXCSR_MASK');
  lua_pushinteger(L,fpudata^.MXCSR_MASK);
  lua_settable(L,index2);

  lua_pushstring(L,'FP_MM0');
  CreateByteTableFromPointer(L, @fpudata.FP_MM0,16);
  lua_settable(L,index2);

  lua_pushstring(L,'FP_MM1');
  CreateByteTableFromPointer(L, @fpudata.FP_MM1,16);
  lua_settable(L,index2);

  lua_pushstring(L,'FP_MM2');
  CreateByteTableFromPointer(L, @fpudata.FP_MM2,16);
  lua_settable(L,index2);

  lua_pushstring(L,'FP_MM3');
  CreateByteTableFromPointer(L, @fpudata.FP_MM3,16);
  lua_settable(L,index2);

  lua_pushstring(L,'FP_MM4');
  CreateByteTableFromPointer(L, @fpudata.FP_MM4,16);
  lua_settable(L,index2);

  lua_pushstring(L,'FP_MM5');
  CreateByteTableFromPointer(L, @fpudata.FP_MM5,16);
  lua_settable(L,index2);

  lua_pushstring(L,'FP_MM6');
  CreateByteTableFromPointer(L, @fpudata.FP_MM6,16);
  lua_settable(L,index2);

  lua_pushstring(L,'FP_MM7');
  CreateByteTableFromPointer(L, @fpudata.FP_MM7,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM0');
  CreateByteTableFromPointer(L, @fpudata.XMM0,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM1');
  CreateByteTableFromPointer(L, @fpudata.XMM1,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM2');
  CreateByteTableFromPointer(L, @fpudata.XMM2,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM3');
  CreateByteTableFromPointer(L, @fpudata.XMM3,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM4');
  CreateByteTableFromPointer(L, @fpudata.XMM4,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM5');
  CreateByteTableFromPointer(L, @fpudata.XMM5,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM6');
  CreateByteTableFromPointer(L, @fpudata.XMM6,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM7');
  CreateByteTableFromPointer(L, @fpudata.XMM7,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM8');
  CreateByteTableFromPointer(L, @fpudata.XMM8,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM9');
  CreateByteTableFromPointer(L, @fpudata.XMM9,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM10');
  CreateByteTableFromPointer(L, @fpudata.XMM10,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM11');
  CreateByteTableFromPointer(L, @fpudata.XMM11,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM12');
  CreateByteTableFromPointer(L, @fpudata.XMM12,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM13');
  CreateByteTableFromPointer(L, @fpudata.XMM13,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM14');
  CreateByteTableFromPointer(L, @fpudata.XMM14,16);
  lua_settable(L,index2);

  lua_pushstring(L,'XMM15');
  CreateByteTableFromPointer(L, @fpudata.XMM15,16);
  lua_settable(L,index2);


  lua_settable(L,index);
end;

procedure lua_push_watch_stack(L: PLua_state; stack: pointer; index: integer);
begin
  lua_pushstring(L,'Stack');
  CreateByteTableFromPointer(L, stack, 4096);
  lua_settable(L,index);
end;


function lua_dbvm_watch_retrievelog(L: PLua_state): integer; cdecl;
var
  id: integer;
  size: integer;
  buf: PPageEventListDescriptor;
  i: integer;

  basic: PPageEventBasicArray;
  extended: PPageEventExtendedArray absolute basic;
  basics: PPageEventBasicWithStackArray absolute basic;
  extendeds: PPageEventExtendedWithStackArray absolute basic;
begin

  OutputDebugString('lua_dbvm_watch_retrievelog');
  result:=0;
  if lua_gettop(L)=0 then exit;
  ID:=lua_tointeger(L,1);

  OutputDebugString('id='+inttostr(id));

  lua_pop(L,lua_gettop(L));

  buf:=nil;
  size:=0;
  i:=dbvm_watch_retrievelog(ID, buf,size);
  while i=2 do
  begin
    //must be 2
    OutputDebugString('reallocating buffer for watchlog');

    if (buf<>nil) then
    begin
      freememandnil(buf);

    end;

    size:=size*2;
    getmem(buf, size);
    if (buf=nil) then exit;

    FillMemory(buf, size,$ce);
    i:=dbvm_watch_retrievelog(ID, buf,size);
  end;

  if i<>0 then
  begin
    lua_pushnil(L);
    case i of
      1: lua_pushstring(L,'invalid id');
      3: lua_pushstring(L,'inactive id');
      4: lua_pushstring(L,'invalid address for buffer');
      else lua_pushstring(L,'unknown error '+inttostr(i));
    end;

    exit(2);
  end;

  outputdebugstring('Buf allocated at '+inttohex(QWORD(buf),8));

  basic:=PPageEventBasicArray(qword(buf)+sizeof(TPageEventListDescriptor));

  outputdebugstring('sizeof(TPageEventBasic)='+inttostr(sizeof(TPageEventBasic)));
  outputdebugstring('sizeof(TPageEventExtended)='+inttostr(sizeof(TPageEventExtended)));
  outputdebugstring('sizeof(TPageEventBasicWithStack)='+inttostr(sizeof(TPageEventBasicWithStack)));
  outputdebugstring('sizeof(TPageEventExtendedWithStack)='+inttostr(sizeof(TPageEventExtendedWithStack)));


  outputdebugstring('sizeof(TPageEventListDescriptor)='+inttostr(sizeof(TPageEventListDescriptor)));
  outputdebugstring('  buf^.ID='+inttostr(buf^.ID));
  outputdebugstring('  buf^.maxSize='+inttostr(buf^.maxSize));
  outputdebugstring('  buf^.numberOfEntries='+inttostr(buf^.numberOfEntries));
  outputdebugstring('  buf^.entryType='+inttostr(buf^.entryType));

  lua_createtable(L, buf^.numberOfEntries, 0); //index 1

  for i:=0 to buf^.numberOfEntries-1 do
  begin
    lua_pushinteger(L,i+1); //2
    lua_createtable(L, 0, 32); //3

    case buf^.entryType of
      0:
      begin
        //basic
        lua_push_watch_basic_fields(L, @basic[i], 3);
      end;

      1:
      begin
        //extended
        lua_push_watch_basic_fields(L, @extended^[i].basic, 3);
        lua_push_watch_fxsave_fields(L, @extended^[i].fpudata, 3);
      end;

      2:
      begin
        //basics
        lua_push_watch_basic_fields(L, @basics^[i].basic, 3);
        lua_push_watch_stack(L, @basics^[i].stack[0], 3);
      end;

      3:
      begin
        //extendeds
        lua_push_watch_basic_fields(L, @extendeds^[i].basic, 3);
        lua_push_watch_fxsave_fields(L, @extendeds^[i].fpudata, 3);
        lua_push_watch_stack(L, @extendeds^[i].stack[0], 3);
      end;
    end;

    lua_settable(L,1);

  end;

  if buf<>nil then
    freemem(buf);

  result:=1;
end;

function lua_dbvm_watch_disable(L: PLua_State): integer; cdecl;
var
  id: integer;
begin
  if lua_gettop(L)=0 then exit(0);

  id:=lua_tointeger(L,1);
  dbvm_watch_delete(id);

  result:=0;
end;

function lua_dmvm_watch_getstatus(L: PLua_State): integer; cdecl;
var last,best: TEPTWatchLogData;
begin
  if dbvm_watch_getstatus(last,best) then
  begin
    lua_createtable(L,0,2);

    lua_pushstring(L,'last');
    lua_createtable(L,0,7);

    lua_pushstring(L,'physicalAddress');
    lua_pushinteger(L,last.physicalAddress);
    lua_settable(L,-3);

    lua_pushstring(L,'initialID');
    lua_pushinteger(L,last.initialID);
    lua_settable(L,-3);

    lua_pushstring(L,'actualID');
    lua_pushinteger(L,last.actualID);
    lua_settable(L,-3);

    lua_pushstring(L,'rip');
    lua_pushinteger(L,last.rip);
    lua_settable(L,-3);

    lua_pushstring(L,'data');
    lua_pushinteger(L,last.data);
    lua_settable(L,-3);

    lua_pushstring(L,'cacheIssue');
    lua_pushinteger(L,last.cacheIssue);
    lua_settable(L,-3);

    lua_pushstring(L,'skipped');
    lua_pushinteger(L,last.skipped);
    lua_settable(L,-3);

    lua_settable(L,-3);

    lua_pushstring(L,'best');
    lua_createtable(L,0,7);

    lua_pushstring(L,'physicalAddress');
    lua_pushinteger(L,best.physicalAddress);
    lua_settable(L,-3);

    lua_pushstring(L,'initialID');
    lua_pushinteger(L,best.initialID);
    lua_settable(L,-3);

    lua_pushstring(L,'actualID');
    lua_pushinteger(L,best.actualID);
    lua_settable(L,-3);

    lua_pushstring(L,'rip');
    lua_pushinteger(L,best.rip);
    lua_settable(L,-3);

    lua_pushstring(L,'data');
    lua_pushinteger(L,best.data);
    lua_settable(L,-3);

    lua_pushstring(L,'cacheIssue');
    lua_pushinteger(L,best.cacheIssue);
    lua_settable(L,-3);

    lua_pushstring(L,'skipped');
    lua_pushinteger(L,best.skipped);
    lua_settable(L,-3);

    lua_settable(L,-3);

    result:=1;
  end
  else
    result:=0;
end;

function lua_dbvm_cloak_activate(L: PLua_State): integer; cdecl;
var PA, VA: QWORD;
  mode :integer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    pa:=lua_tointeger(L,1);
    if lua_gettop(L)>=2 then
      VA:=lua_tointeger(L,2)
    else
      VA:=0;

    if lua_gettop(L)>=3 then
      mode:=lua_tointeger(L,3)
    else
      mode:=1;

    lua_pushinteger(L, dbvm_cloak_activate(PA, VA, mode));
    result:=1;
  end;
end;

function lua_dbvm_cloak_deactivate(L: PLua_State): integer; cdecl;
var PA: QWORD;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    pa:=lua_tointeger(L,1);
    lua_pushboolean(L, dbvm_cloak_deactivate(PA));
    result:=1;
  end;
end;

function lua_dbvm_cloak_readOriginal(L: PLua_State): integer; cdecl;
var
  PA: QWORD;
  buf: pointer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    pa:=lua_tointeger(L,1);
    getmem(buf,4096);
    dbvm_cloak_readoriginal(PA, buf);

    CreateByteTableFromPointer(L, buf, 4096);
    FreeMemAndNil(buf);
    result:=1;
  end;
end;

function lua_dbvm_cloak_writeOriginal(L: PLua_State): integer; cdecl;
var
  PA: QWORD;
  buf: pointer;
  r: integer;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    pa:=lua_tointeger(L,1);
    if lua_istable(L,2) then
    begin
      getmem(buf,4096);
      readBytesFromTable(L, 2,buf,4096);
      r:=dbvm_cloak_writeoriginal(PA, buf);
      lua_pushinteger(L, r);

      FreeMemAndNil(buf);
      result:=1;
    end;
  end;
end;

function lua_dbvm_traceonbp(L: PLua_State): integer; cdecl;
(*
dbvm_traceonbp(pa, count, va, {secondaryoptions})
secondaryoptions is a table:
  logFPU: boolean
  logStack: boolean
  ...
*)
var
  pa,va: qword;
  count: integer;
  options: dword;

  logFPU: boolean;
  logStack: boolean;
  r: integer;
begin
  if lua_gettop(L)>=2 then
  begin
    pa:=lua_tointeger(L,1);
    count:=lua_tointeger(L,2);

    if count<=0 then
    begin
      lua_pushnil(L);
      lua_pushstring(L,'count has to be > 0 ');
      exit(2);
    end;

    if lua_gettop(L)>=3 then
      va:=lua_toaddress(L,3)
    else
      va:=0;

    logFPU:=false;
    logStack:=false;
    if lua_gettop(L)>=4 then
    begin
      if lua_istable(L,4) then
      begin
        lua_pushstring(L,'logFPU');
        lua_gettable(L,4);
        if lua_isboolean(L,-1) then
          logFPU:=lua_toboolean(L,-1);

        lua_pop(L,1);

        lua_pushstring(L,'logStack');
        lua_gettable(L,4);
        if lua_isboolean(L,-1) then
          logStack:=lua_toboolean(L,-1);

        lua_pop(L,1);
      end;
    end;

    options:=0;
    if logfpu then options:=options or 1;
    if logstack then options:=options or 2;
    r:=dbvm_cloak_traceonbp(pa,count,options,va);

    lua_pushinteger(L,r);
    result:=1;
  end
  else
    result:=0;
end;

function lua_dbvm_traceonbp_retrievelog(L: PLua_state): integer; cdecl;
var
  id: integer;
  size: integer;
  buf: PTracerListDescriptor;
  i: integer;

  basic: PPageEventBasicArray;
  extended: PPageEventExtendedArray absolute basic;
  basics: PPageEventBasicWithStackArray absolute basic;
  extendeds: PPageEventExtendedWithStackArray absolute basic;
begin

  OutputDebugString('lua_dbvm_traceonbp_retrievelog');
  result:=0;
  lua_pop(L,lua_gettop(L));

  buf:=nil;
  size:=0;
  i:=dbvm_cloak_traceonbp_readlog(buf,size);
  while i=2 do
  begin
    //must be 2
    OutputDebugString(format('reallocating buffer for tracelog (should be at least %d bytes)',[size]));

    if (buf<>nil) then
      freememandnil(buf);

    size:=size*2;
    getmem(buf, size);
    if (buf=nil) then exit;

    FillMemory(buf, size,$ce);
    i:=dbvm_cloak_traceonbp_readlog(buf,size);
  end;

  if i<>0 then
  begin
    lua_pushnil(L);
    case i of
      4: lua_pushstring(L,'invalid address for buffer');
      6: lua_pushstring(L,'offset too high');
      else lua_pushstring(L,'unknown error '+inttostr(i));
    end;

    exit(2);
  end;

  outputdebugstring('Buf allocated at '+inttohex(QWORD(buf),8));

  basic:=PPageEventBasicArray(qword(buf)+sizeof(TTracerListDescriptor));


  outputdebugstring('sizeof(TTracerListDescriptor)='+inttostr(sizeof(TTracerListDescriptor)));
  outputdebugstring('  buf^.datatype='+inttostr(buf^.datatype));
  outputdebugstring('  buf^.count='+inttostr(buf^.count));


  lua_createtable(L, buf^.count, 0); //index 1

  for i:=0 to buf^.count-1 do
  begin
    lua_pushinteger(L,i+1); //2
    lua_createtable(L, 0, 32); //3

    case buf^.datatype of
      0:
      begin
        //basic
        lua_push_watch_basic_fields(L, @basic[i], 3);
      end;

      1:
      begin
        //extended
        lua_push_watch_basic_fields(L, @extended^[i].basic, 3);
        lua_push_watch_fxsave_fields(L, @extended^[i].fpudata, 3);
      end;

      2:
      begin
        //basics
        lua_push_watch_basic_fields(L, @basics^[i].basic, 3);
        lua_push_watch_stack(L, @basics^[i].stack[0], 3);
      end;

      3:
      begin
        //extendeds
        lua_push_watch_basic_fields(L, @extendeds^[i].basic, 3);
        lua_push_watch_fxsave_fields(L, @extendeds^[i].fpudata, 3);
        lua_push_watch_stack(L, @extendeds^[i].stack[0], 3);
      end;
    end;

    lua_settable(L,1);

  end;

//  if (buf<>nil) then
//    freemem(buf);

  result:=1;
end;

function lua_dbvm_traceonbp_getstatus(L: PLua_State): integer; cdecl;
var
  count, maxcount: dword;
  r: integer;
begin
  r:=dbvm_cloak_traceonbp_getstatus(count, maxcount);

  lua_pushinteger(L,r);
  lua_pushinteger(L,count);
  lua_pushinteger(L,maxcount);
  result:=3;
end;

function lua_dbvm_traceonbp_stoptrace(L: PLua_State): integer; cdecl;
var r: integer;
begin
  r:=dbvm_cloak_traceonbp_stoptrace();
  lua_pushinteger(L,r);
  result:=r;
end;

function lua_dbvm_traceonbp_remove(L: PLua_State): integer; cdecl;
var
  pa: qword=0;
  force: boolean=false;
  r: integer;
begin
  if lua_gettop(L)>=1 then
    pa:=lua_tointeger(L,1);

  if lua_gettop(L)>=2 then
    force:=lua_toboolean(L,2);

  r:=dbvm_cloak_traceonbp_remove(pa, force);
  lua_pushinteger(L,r);
  result:=1;
end;

function lua_dbvm_changeregonbp(L: PLua_State): integer; cdecl;
var
  pa,va: qword;
  changeregonbpinfo: TChangeRegOnBPInfo;
  r: boolean;

begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    pa:=lua_tointeger(L,1);
    if lua_istable(L,2) then
    begin
      lua_pushstring(L,'newCF');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.Flags.newCF:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeCF:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);


      lua_pushstring(L,'newPF');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.Flags.newPF:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changePF:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newAF');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.Flags.newAF:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeAF:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newZF');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.Flags.newZF:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeZF:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newSF');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.Flags.newSF:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeSF:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newOF');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.Flags.newOF:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeOF:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRAX');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRAX:=lua_tointeger(L,-1);
      changeregonbpinfo.flags.changeRAX:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRBX');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRBX:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeRBX:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRCX');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRCX:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeRCX:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRDX');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRDX:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeRDX:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRSI');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRSI:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeRSI:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRDI');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRDI:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeRDI:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRBP');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRBP:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeRBP:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRSP');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRSP:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeRSP:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newRIP');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newRIP:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeRIP:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newR8');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newR8:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeR8:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newR9');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newR9:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeR9:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newR10');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newR10:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeR10:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newR11');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newR11:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeR11:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newR12');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newR12:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeR12:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newR13');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newR13:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeR13:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newR14');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newR14:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeR14:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      lua_pushstring(L,'newR15');
      lua_gettable(L,2);

      if not lua_isnil(L,-1) then
        changeregonbpinfo.newR15:=lua_tointeger(L,-1);
      changeregonbpinfo.Flags.changeR15:=ifthen(lua_isnil(L,-1), 0, 1);
      lua_pop(L,1);

      if lua_gettop(L)>=3 then
        VA:=lua_tointeger(L,3)
      else
        VA:=0;


      r:=dbvm_cloak_changeregonbp(PA, changeregonbpinfo, VA)=0;
      lua_pushboolean(l,r);
      result:=1;
    end;

  end;
end;

function lua_dbvm_removechangeregonbp(L: PLua_State): integer; cdecl;
var PA: qword;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    PA:=lua_tointeger(L,1);
    lua_pushboolean(L, dbvm_cloak_removechangeregonbp(PA)=0);
    result:=1;
  end;
end;

function lua_dbvm_ept_reset(L: PLua_State): integer; cdecl;
begin
  dbvm_ept_reset;
  result:=0;
end;



function lua_dbvm_log_cr3_start(L: PLua_State): integer; cdecl;
begin
  lua_pushboolean(L, dbvm_log_cr3values_start);
  result:=1;
end;

function lua_dbvm_log_cr3_stop(L: PLua_State): integer; cdecl;
var
  r: boolean;
  log: array [0..511] of QWORD;
  i,j: integer;
begin
  result:=0;
  ZeroMemory(@log[0],4096);

  r:=dbvm_log_cr3values_stop(@log[0]);
  if r then
  begin
    lua_newtable(L);
    j:=1;
    for i:=0 to 511 do
    begin
      if log[i]<>0 then
      begin
        lua_pushinteger(L, j);
        lua_pushinteger(L,log[i]);
        lua_settable(L, -3);
        inc(j);
      end;
    end;
    result:=1;
  end;
end;


function lua_dbvm_registerPlugin(L: PLua_State): integer; cdecl;
var
  pluginAddress: ptruint;
  pluginSize: integer;
  plugintype: integer;
  r: integer;
begin
  if lua_gettop(L)>=3 then
  begin
    pluginAddress:=lua_tointeger(L,1);
    pluginSize:=lua_tointeger(L,2);
    plugintype:=lua_tointeger(L,2);
    r:=dbvm_registerPlugin(pointer(pluginAddress), pluginSize, pluginType);
    lua_pushinteger(L,r);
    result:=1;
  end
  else
    result:=0;
end;

function lua_dbvm_raisePMI(L: PLua_State): integer; cdecl;
begin
  result:=0;
  dbvm_raisePMI;
end;

function lua_dbvm_ultimap2_hideRangeUsage(L: PLua_State): integer; cdecl;
begin
  result:=0;
  dbvm_ultimap2_hideRangeUsage;
end;

function lua_dbvm_ultimap_getDebugInfo(L: PLua_State): integer; cdecl;
var
  di: TULTIMAPDEBUGINFO;
  r: integer;
begin
  r:=dbvm_ultimap_debuginfo(@di);
  result:=2;
  lua_pushinteger(L,r);
  lua_newtable(L);


  lua_pushstring(L,'Active');
  lua_pushinteger(L,di.Active);
  lua_settable(L,2);

  lua_pushstring(L,'CR3');
  lua_pushinteger(L,di.CR3);
  lua_settable(L,2);

  lua_pushstring(L,'DEBUGCTL');
  lua_pushinteger(L,di.DEBUGCTL);
  lua_settable(L,2);

  lua_pushstring(L,'DS_AREA');
  lua_pushinteger(L,di.DS_AREA);
  lua_settable(L,2);

  lua_pushstring(L,'OriginalDebugCTL');
  lua_pushinteger(L,di.OriginalDebugCTL);
  lua_settable(L,2);

  lua_pushstring(L,'OriginalDS_AREA');
  lua_pushinteger(L,di.OriginalDS_AREA);
  lua_settable(L,2);

  lua_pushstring(L,'CR3_switchcount');
  lua_pushinteger(L,di.CR3_switchcount);
  lua_settable(L,2);

  lua_pushstring(L,'CR3_switchcount2');
  lua_pushinteger(L,di.CR3_switchcount2);
  lua_settable(L,2);

  lua_pushstring(L,'LastOldCR3');
  lua_pushinteger(L,di.LastOldCR3);
  lua_settable(L,2);

  lua_pushstring(L,'LastNewCR3');
  lua_pushinteger(L,di.LastNewCR3);
  lua_settable(L,2);

  lua_pushstring(L,'cpunr');
  lua_pushinteger(L,di.cpunr);
  lua_settable(L,2);

  result:=2;
end;

function lua_dbvm_setTSCAdjust(L: PLua_State): integer; cdecl;
var
  enabled:boolean;
  timeout: integer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    enabled:=lua_toboolean(L,1);
    if enabled and (lua_gettop(L)>=2) then
      timeout:=lua_tointeger(L,2)
    else
      timeout:=2000;

    dbvm_setTSCAdjust(enabled, timeout);
  end;
end;

function lua_dbvm_speedhack_setSpeed(L: PLua_State): integer; cdecl;
var speed: double;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    speed:=lua_tonumber(L,1);
    dbvm_speedhack_setSpeed(speed);
  end;

end;

function lua_dbvm_enableTSCHook(L: PLua_State): integer; cdecl;
begin
  dbvm_enableTSCHook;
  result:=0;
end;

function lua_dbvm_disableTSCHook(L: PLua_State): integer; cdecl;
begin
  lua_pushboolean(L, dbvm_disableTSCHook);
  result:=1;
end;

function lua_dbvm_findCR3(L: PLua_State): integer; cdecl;
begin
  lua_pushinteger(L, dbvm_findCR3(processhandle));
  result:=1;
end;

function lua_dbvm_hidephysicalmemory(L: PLua_State): integer; cdecl;
begin
  result:=0;
  dbvm_hidephysicalmemory;
end;

function lua_dbvm_hidephysicalmemoryall(L: PLua_State): integer; cdecl;
begin
  result:=0;
  dbvm_hidephysicalmemoryall;
end;


function lua_dbvm_bp_getBrokenThreadListSize(L: PLua_State): integer; cdecl;
begin
  lua_pushinteger(L,dbvm_bp_getBrokenThreadListSize());
  result:=1;
end;

function lua_dbvm_bp_getProcessAndThreadIDFromEvent(L: PLua_State): integer; cdecl;
var
  shortstate: TDBVMBPShortState;
  cid: TClientID;
begin
  result:=0;
  if lua_gettop(L)>=1 then //given is a short or full state
  begin
    if lua_istable(L,1) then
    begin
      lua_pushstring(L,'GSBASE');
      lua_gettable(L,1);
      if lua_isnil(L,-1) then
      begin
        lua_pushnil(L);
        lua_pushstring(L,'Missing GSBASE');
        exit(2);
      end;
      shortstate.gsbase:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L,'GSBASE_KERNEL');
      lua_gettable(L,1);
      if lua_isnil(L,-1) then
      begin
        lua_pushnil(L);
        lua_pushstring(L,'Missing GSBASE_KERNEL');
        exit(2);
      end;
      shortstate.GSBASE_KERNEL:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L,'FSBASE');
      lua_gettable(L,1);
      if lua_isnil(L,-1) then
      begin
        lua_pushnil(L);
        lua_pushstring(L,'Missing FSBASE');
        exit(2);
      end;
      shortstate.FSBASE:=lua_tointeger(L,-1);
      lua_pop(L,1);

      lua_pushstring(L,'CR3');
      lua_gettable(L,1);
      if lua_isnil(L,-1) then
      begin
        lua_pushnil(L);
        lua_pushstring(L,'Missing CR3');
        exit(2);
      end;
      shortstate.cr3:=lua_tointeger(L,-1);
      lua_pop(L,1);

      cid.UniqueProcess:=0;
      cid.UniqueThread:=0;
      if getClientIDFromDBVMBPShortState(shortstate, cid) then
      begin
        lua_pushinteger(L,cid.UniqueProcess);
        lua_pushinteger(L,cid.UniqueThread);
        exit(2);
      end
      else
      begin
        lua_pushnil(L);
        lua_pushstring(L,'Failure to get the process and threadid');
        exit(2);
      end;



    end
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'First parameter needs to be a threadevent table');
      exit(2);
    end;
  end;
end;

function lua_dbvm_bp_getBrokenThreadEventShort(L: PLua_State): integer; cdecl;
var
  id: integer;
  r: integer;
  shortstate: TDBVMBPShortState;
begin
  result:=0;
  if lua_Gettop(L)>=1 then
  begin
    id:=lua_tointeger(L,1);
    r:=dbvm_bp_getBrokenThreadEventShort(id,shortstate);
    if r=0 then
    begin
      lua_createtable(L,0,7);

      lua_pushstring(L,'WatchID');
      lua_pushinteger(L,shortstate.watchid);
      lua_settable(L,-3);

      lua_pushstring(L,'Status');
      lua_pushinteger(L,shortstate.status);
      lua_settable(L,-3);

      lua_pushstring(L,'CS');
      lua_pushinteger(L,shortstate.cs);
      lua_settable(L,-3);

      lua_pushstring(L,'RIP');
      lua_pushinteger(L,shortstate.rip);
      lua_settable(L,-3);

      lua_pushstring(L,'CR3');
      lua_pushinteger(L,shortstate.cr3);
      lua_settable(L,-3);

      lua_pushstring(L,'FSBASE');
      lua_pushinteger(L,shortstate.fsbase);
      lua_settable(L,-3);

      lua_pushstring(L,'GSBASE');
      lua_pushinteger(L,shortstate.gsbase);
      lua_settable(L,-3);

      lua_pushstring(L,'GSBASE_KERNEL');
      lua_pushinteger(L,shortstate.gsbase_kernel);
      lua_settable(L,-3);

      lua_pushstring(L,'Heartbeat');
      lua_pushinteger(L,shortstate.heartbeat);
      lua_settable(L,-3);

      result:=1;
    end
    else
    begin
      lua_pushnil(L);
      case r of
        1: lua_pushstring(L,'invalid id');
        2: lua_pushstring(L,'not active');
        else
          lua_pushstring(L,'unknown');
      end;

      exit(2);

    end;
  end;
end;

function lua_dbvm_bp_getBrokenThreadEventFull(L: PLua_State): integer; cdecl;
var
  id: integer;
  r: integer;
  state: TPageEventExtended ;
  watchid: integer;
  status: integer;
  ti: integer;
begin
  result:=0;
  if lua_Gettop(L)>=1 then
  begin
    id:=lua_tointeger(L,1);
    r:=dbvm_bp_getBrokenThreadEventFull(id,watchid, status, state);
    if r=0 then
    begin
      lua_newtable(L);
      ti:=lua_gettop(L);
      lua_push_watch_basic_fields(L, @state.basic, ti);
      lua_push_watch_fxsave_fields(L, @state.fpudata, ti);

      lua_pushstring(L,'Count');
      lua_pushnil(L);
      lua_settable(L,ti);

      lua_pushstring(L,'Heartbeat');
      lua_pushinteger(L,state.basic.Count);
      lua_settable(L,ti);

      lua_pushstring(L,'Status');
      lua_pushinteger(L,status);
      lua_settable(L,ti);

      lua_pushstring(L,'WatchID');
      lua_pushinteger(L,watchid);
      lua_settable(L,-3);


      result:=1;
    end
    else
    begin
      lua_pushnil(L);
      case r of
        1: lua_pushstring(L,'invalid id');
        2: lua_pushstring(L,'not active');
        else
          lua_pushstring(L,'unknown');
      end;

      exit(2);

    end;
  end;
end;

function lua_dbvm_bp_setBrokenThreadEventFull(L: PLua_state): integer; cdecl;
var
  id: integer;
  state: TPageEventExtended;
  watchid, status: integer;
  fi: integer;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    id:=lua_tointeger(L,1);
    if lua_istable(L,2) then
    begin
      //get the old state
      if dbvm_bp_getBrokenThreadEventFull(id,watchid, status, state)=0 then
      begin
        lua_pushstring(L,'FLAGS');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.FLAGS:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RAX');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RAX:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RBX');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RBX:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RCX');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RCX:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RDX');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RDX:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RSI');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RSI:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RDI');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RDI:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'R8');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.R8:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'R9');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.R9:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'R10');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.R10:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'R11');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.R11:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'R12');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.R12:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'R13');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.R13:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'R14');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.R14:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'R15');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.R15:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RBP');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RBP:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RSP');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RSP:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'RIP');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.RIP:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'DR0');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.DR0:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'DR1');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.DR1:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'DR2');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.DR2:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'DR3');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.DR3:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'DR6');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.DR6:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'DR7');
        lua_gettable(L,2);
        if not lua_isnil(L,-1) then state.basic.DR7:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'FXSAVBE64');
        lua_gettable(L,2);
        if lua_istable(L,-1) then
        begin
          //fxsave fields
          fi:=lua_gettop(L);
          lua_pushstring(L,'FCW');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then state.fpudata.FCW:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'FSW');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then state.fpudata.FSW:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'FTW');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then state.fpudata.FTW:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'FOP');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then state.fpudata.FOP:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'IP');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then state.fpudata.FPU_IP:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'DP');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then state.fpudata.FPU_DP:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'MXCSR');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then state.fpudata.MXCSR:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'MXCSR_MASK');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then state.fpudata.MXCSR_MASK:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'FP_MM0');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.FP_MM0,16);
          lua_pop(L,1);

          lua_pushstring(L,'FP_MM1');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.FP_MM1,16);
          lua_pop(L,1);

          lua_pushstring(L,'FP_MM2');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.FP_MM2,16);
          lua_pop(L,1);

          lua_pushstring(L,'FP_MM3');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.FP_MM3,16);
          lua_pop(L,1);

          lua_pushstring(L,'FP_MM4');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.FP_MM4,16);
          lua_pop(L,1);

          lua_pushstring(L,'FP_MM5');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.FP_MM5,16);
          lua_pop(L,1);

          lua_pushstring(L,'FP_MM6');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.FP_MM6,16);
          lua_pop(L,1);

          lua_pushstring(L,'FP_MM7');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.FP_MM7,16);
          lua_pop(L,1);


          lua_pushstring(L,'XMM0');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM0,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM1');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM1,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM2');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM2,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM3');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM3,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM4');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM4,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM5');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM5,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM6');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM6,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM7');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM7,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM8');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM8,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM9');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM9,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM10');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM10,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM11');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM11,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM12');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM12,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM13');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM13,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM14');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM14,16);
          lua_pop(L,1);

          lua_pushstring(L,'XMM15');
          lua_gettable(L,fi);
          if not lua_isnil(L,-1) then readBytesFromTable(L,lua_gettop(L), @state.fpudata.XMM15,16);
          lua_pop(L,1);
        end;
        lua_pop(L,1);

        lua_pushinteger(L, dbvm_bp_setBrokenThreadEventFull(id,state));
        result:=1;


      end
      else
      begin
        lua_pushnil(L);
        lua_pushstring(L,'the given ID is not valid (anymore)');
        exit(2);
      end;

    end
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'param2 has to be a table');
      exit(2);
    end;

  end;


end;

function lua_dbvm_bp_resumeBrokenThread(L: PLua_state): integer; cdecl;
var
  id, continueMethod: integer;
  r: integer;
begin
  result:=0;
  if lua_Gettop(L)>=2 then
  begin
    id:=lua_tointeger(L,1);
    continuemethod:=lua_tointeger(L,2);

    r:=dbvm_bp_resumeBrokenThread(id,continueMethod);
    if r=0 then
    begin
      lua_pushboolean(L,true);
      exit(1);
    end
    else
    begin
      lua_pushnil(L);
      case r of
        1: lua_pushstring(L,'invalid id');
        2: lua_pushstring(L,'not active');
        3: lua_pushstring(L,'was already set to continue');
        4: lua_pushstring(L,'abandoned.  Has been marked as free now');
        else
          lua_pushstring(L,'unknown');
      end;

      exit(2);
    end;
  end;
end;


function dbk_readMSR(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  msr: dword;
begin
  result:=0;
  {$IFDEF windows}
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    msr:=lua_tointeger(L,-1);
    lua_pushinteger(L, readMSR(msr));
    result:=1;
  end else lua_pop(L, parameters);
  {$ENDIF}
end;

function dbk_writeMSR(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  msr: dword;
  msrvalue: qword;
begin
  result:=0;
  {$IFDEF windows}
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    msr:=lua_tointeger(L,1);
    msrvalue:=lua_tointeger(L,2);
    writemsr(msr, msrvalue);
  end;

  lua_pop(L, parameters);
  {$ENDIF}
end;

function createSplitter(L: Plua_State): integer; cdecl;
var
  Splitter: TCESplitter;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_toceuserdata(L, -parameters)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  Splitter:=TCESplitter.Create(owner);
  if owner<>nil then
    Splitter.Parent:=owner;

  luaclass_newClass(L, splitter);
  result:=1;
end;

function createPaintBox(L: Plua_State): integer; cdecl;
var
  paintbox: TPaintbox;
  parameters: integer;
  owner: TWincontrol;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  lua_pop(L, lua_gettop(L));


  paintbox:=TPaintBox.Create(owner);
  if owner<>nil then
    paintbox.Parent:=owner;

  luaclass_newClass(L, paintbox);
  result:=1;
end;

function allocateSharedMemoryLocal(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  sharedmemoryname: string;
  size: ptruint;
  address: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    sharedmemoryname:=Lua_ToString(L,1);

    if parameters>=2 then
      size:=lua_tointeger(L, 2)
    else
      size:=4096;

    lua_pop(L, parameters);

    address:=allocateSharedMemory(sharedmemoryname, size);
    if address<>nil then
    begin
      lua_pushinteger(L, ptruint(address));
      result:=1;
    end;
  end else lua_pop(L, parameters);
end;

function deallocateSharedMemoryLocal(L: PLua_State): integer; cdecl;
var address: ptruint;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    address:=lua_tointeger(L,1);

    UnmapViewOfFile(pointer(address));
  end;
end;

function allocateSharedMemory(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  sharedmemoryname: string;
  size: ptruint;
  address: pointer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    sharedmemoryname:=Lua_ToString(L,-parameters);

    if parameters>=2 then
      size:=lua_tointeger(L, -parameters+1)
    else
      size:=4096;

    lua_pop(L, parameters);

    address:=allocateSharedMemoryIntoTargetProcess(sharedmemoryname, size);
    if address<>nil then
    begin
      lua_pushinteger(L, ptruint(address));
      result:=1;
    end;
  end else lua_pop(L, parameters);
end;

function deallocateSharedMemory(L: PLua_State): integer; cdecl;
var parameters: integer;
  address: ptruint;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
    lua_pop(L, parameters);

  lua_pushstring(L, rsDeallocateSharedMemoryIsNotImplemented);
  lua_error(L);
end;

function getCheatEngineDir(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(l));
  lua_pushstring(L, CheatEngineDir);
  result:=1;
end;

function lua_getCheatEngineProcessID(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(l));
  lua_pushinteger(L, GetCurrentProcessId);
  result:=1;
end;



function getInstructionSize(L: PLua_State): integer; cdecl;
var parameters: integer;
  address, address2: ptruint;
  d: TDisassembler;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    address:=lua_toaddress(L,1);

    lua_pop(L, parameters);

    address2:=address;


    d:=TDisassembler.create;
    d.disassemble(address);
    d.free;
    lua_pushinteger(L, address-address2);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;

function getPreviousOpcode(L: PLua_State): integer; cdecl;
var parameters: integer;
  address, address2: ptruint;
  d: TDisassembler;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    address:=lua_toaddress(L,1);

    lua_pop(L, parameters);

    d:=TDisassembler.create;
    lua_pushinteger(L, previousopcode(address,d));
    d.free;
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;

function disassemble_lua(L: PLua_State): integer; cdecl;
var parameters: integer;
  address: ptruint;
  d: TDisassembler;
  x: string;
  s: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    address:=lua_toaddress(L,1);

    lua_pop(L, parameters);

    d:=TDisassembler.Create;
    try
      d.showmodules:=false;
      d.showsymbols:=false;
      d.showsections:=false;
      s:=d.disassemble(address, x);
    finally
      d.free;
    end;


    result:=1;
    lua_pushstring(L, s);
  end else lua_pop(L, parameters);
end;

function splitDisassembledString(L: PLua_State): integer; cdecl;
var parameters: integer;
  disassembledstring: string;

  address, bytes, opcode, special: string;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    disassembledstring:=lua_tostring(L,-parameters);
    lua_pop(L, parameters);

    disassembler.splitDisassembledString(disassembledstring, true, address, bytes, opcode, special);

    result:=4;
    lua_pushstring(L, special);
    lua_pushstring(L, opcode);
    lua_pushstring(L, bytes);
    lua_pushstring(L, address);
  end
  else
    lua_pop(L, parameters);
end;

function hexadecimalview_getTopAddress(L: PLua_State): integer; cdecl;
var
  hv: THexView;
begin
  hv:=luaclass_getClassObject(L);
  lua_pushinteger(L, hv.address);
  result:=1;
end;

function hexadecimalview_setTopAddress(L: PLua_State): integer; cdecl;
var
  hv: THexView;
  address: ptruint;
begin
  result:=0;
  hv:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    address:=lua_toaddress(L,1);

    hv.address:=address;
  end
end;

function hexadecimalview_onAddressChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  hexadecimalview: THexView;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    hexadecimalview:=lua_toceuserdata(L,-2);

    CleanupLuaCall(tmethod(hexadecimalview.onAddressChange));
    hexadecimalview.onAddressChange:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      hexadecimalview.onAddressChange:=lc.AddressChangeEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      hexadecimalview.onAddressChange:=lc.AddressChangeEvent;
    end;

  end;

  lua_pop(L, parameters);
end;

function hexadecimalview_onByteSelect(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  hexadecimalview: THexView;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    hexadecimalview:=lua_toceuserdata(L,-2);

    CleanupLuaCall(tmethod(hexadecimalview.onByteSelect));
    hexadecimalview.onByteSelect:=nil;

    if lua_isfunction(L,-1) then
    begin
      routine:=Lua_ToString(L,-1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      hexadecimalview.onByteSelect:=lc.ByteSelectEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      hexadecimalview.onByteSelect:=lc.ByteSelectEvent;
    end;

  end;

  lua_pop(L, parameters);
end;


function disassemblerview_getSelectedAddress(L: PLua_State): integer; cdecl;    //6.2-
var
  dv: TDisassemblerview;
begin
  dv:=luaclass_getClassObject(L);
  result:=2;
  lua_pushinteger(L, dv.SelectedAddress);
  lua_pushinteger(L, dv.SelectedAddress2); //6.2: Returns both addresses
end;

function disassemblerview_setSelectedAddress(L: PLua_State): integer; cdecl;  //6.2-
var
  dv: TDisassemblerview;
  address: ptruint;
begin
  result:=0;
  dv:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    address:=lua_toaddress(L,1);

    dv.SelectedAddress:=address;
  end;
end;

function disassemblerview_onSelectionChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  disassemblerview: TDisassemblerview;
  f: integer;
  routine: string;

  lc: TLuaCaller;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    disassemblerview:=lua_toceuserdata(L,-2);

    CleanupLuaCall(tmethod(disassemblerview.onselectionchange));
    disassemblerview.onselectionchange:=nil;

    if lua_isfunction(L,-1) then
    begin

      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      disassemblerview.onselectionchange:=lc.DisassemblerSelectionChangeEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      disassemblerview.onselectionchange:=lc.DisassemblerSelectionChangeEvent;
    end;
    lua_pop(L, lua_gettop(L));
  end;

  lua_pop(L, parameters);
end;

function getForegroundProcess(L: PLua_State): integer; cdecl;
var h: thandle;
  pid: dword;
begin
  result:=0;
  {$IFDEF windows}
  lua_pop(L, lua_gettop(L));

  h:=GetForegroundWindow;

  GetWindowThreadProcessId(h, pid);
  lua_pushinteger(L, pid);
  result:=1;
  {$ENDIF}
end;

function cheatEngineIs64Bit(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  lua_pushboolean(L, {$ifdef cpu64}true{$else}false{$endif});
  result:=1;
end;

function targetIs64Bit(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  lua_pushboolean(L, processhandler.is64Bit);
  result:=1;
end;

function unregisterFormAddNotification(L: PLua_State): integer; cdecl;
var lc: TLuacaller;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    lc:=lua_ToCEUserData(L, -1);
    if lc<>nil then
      screen.RemoveHandlerFormAdded(lc.ScreenFormEvent);

    lc.Free;
  end;
end;

function registerFormAddNotification(L: PLua_State): integer; cdecl;
var lc: TLuaCaller;
  f: integer;
  routine: string;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    lc:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      screen.AddHandlerFormAdded(lc.ScreenFormEvent);
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      screen.AddHandlerFormAdded(lc.ScreenFormEvent);
    end;


    luaclass_newClass(L, lc);
    result:=1;
  end;
end;

function getFormCount(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  lua_pushinteger(L, screen.FormCount);
  result:=1;
end;

function getForm(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  f: TCustomForm;
  index: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    index:=lua_tointeger(L, -1);
    lua_pop(L, lua_gettop(L));

    if index<screen.formcount then
    begin
      luaclass_newClass(L, screen.Forms[index]);
      result:=1;
    end;
  end
  else
    lua_pop(L, lua_gettop(L));
end;

function onAutoGuess(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  lc: TLuaCaller;
  routine: string;
  f: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    CleanupLuaCall(tmethod(onAutoGuessRoutine));
    onAutoGuessRoutine:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX); //pop the last item of the stack, which is what I need

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      onAutoGuessRoutine:=lc.AutoGuessEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=Lua_ToString(L, -1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      onAutoGuessRoutine:=lc.AutoGuessEvent;
    end;


  end;

  lua_pop(L, lua_gettop(L));
end;

function onAPIPointerChange(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  lc: TLuaCaller;
  routine: string;
  f: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    CleanupLuaCall(tmethod(plugin.onAPIPointerChange));
    plugin.onAPIPointerChange:=nil;

    if lua_isfunction(L,-1) then
    begin
      f:=luaL_ref(L,LUA_REGISTRYINDEX); //pop the last item of the stack, which is what I need

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
      plugin.onAPIPointerChange:=lc.NotifyEvent;
    end
    else
    if lua_isstring(L,-1) then
    begin
      routine:=Lua_ToString(L, -1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
      plugin.onAPIPointerChange:=lc.NotifyEvent;
    end;


  end;

  lua_pop(L, lua_gettop(L));
end;

function setAPIPointer(L: PLua_State): integer; cdecl;
var parameters: integer;
  apiID: integer;
  address: ptruint;
begin
  result:=0;
  {$IFDEF windows}
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    apiID:=lua_tointeger(L, -2);
    address:=lua_toaddress(L,-1);


    lua_pop(L, parameters);

    case apiid of
      0: newkernelhandler.OpenProcess:=pointer(address);
      1: newkernelhandler.ReadProcessMemoryActual:=pointer(address);
      2: newkernelhandler.WriteProcessMemoryActual:=pointer(address);
      3: newkernelhandler.VirtualQueryExActual:=pointer(address);
    end;

  end
  else
    lua_pop(L, parameters);
  {$ENDIF}
end;


function dbvm_initialize(L: PLua_State): integer; cdecl;
var
  parameters: integer;
  offload: boolean;
  r: boolean;
  reason: string;
begin
  result:=0;

  {$IFDEF windows}
  //for now use the default
  if (dbvm_version>0) then
  begin
    //already loaded and initialized
    lua_pop(L, lua_gettop(L));
    lua_pushboolean(L, true);
    result:=1;
    exit;
  end;

  //not yet loaded/initialized
  if (vmx_password1=0) and (vmx_password2=0) and (vmx_password3=0) then
  begin
    vmx_password1:=$76543210;
    vmx_password2:=$fedcba98;
    vmx_password3:=$90909090;
  end;


  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    offload:=lua_toboolean(L, 1);

    if offload then
    begin
      if isRunningDBVM=false then
      begin
        reason:='A lua script wants to launch the DBVM hypervisor. Reason:';

        if parameters>=2 then
          reason:=Lua_ToString(L,2)
        else
          reason:='No reason given';

        //not yet loaded.
        if isDBVMCapable then
          r:=loaddbvmifneeded(reason);
      end;
    end;
  end
  else
    lua_pop(L, lua_gettop(L));

  result:=1;
  lua_pushboolean(L, dbvm_version>0);
  {$ENDIF}
end;

function dbvm_setKeys(L: PLua_State): integer; cdecl;
var key1, key3: qword;
  key2: dword;
begin
  if lua_gettop(L)>=3 then
  begin
    key1:=lua_tointeger(L,1);
    key2:=lua_tointeger(L,2);
    key3:=lua_tointeger(L,3);

    configure_vmx(key1, key2, key3);

    lua_pushboolean(L, dbvm_version>=$ce000000);
    result:=1;
  end
  else
  begin
    lua_pushnil(L);
    lua_pushstring(L,rsIncorrectNumberOfParameters);
    result:=2;
  end;
end;

function lua_dbvm_getMemory(L: PLua_State): integer; cdecl;
var
  size: qword;
  pages: qword;
begin
  size:=dbvm_getMemory(pages);

  lua_pushinteger(L,size);
  lua_pushinteger(L,pages);
  result:=2;
end;

function dbvm_addMemory(L: PLua_State): integer; cdecl;
var pagecount: qword;
begin
  {$IFDEF windows}
  LoadDBK32;
  if lua_gettop(L)>=1 then
  begin
    pagecount:=lua_tointeger(L,1);
    allocateMemoryForDBVM(pagecount);

    dbvm_getMemory(pagecount);  //get the new count
    lua_pushinteger(L, pagecount);
    result:=1;
  end
  else
  {$ENDIF}
    result:=0;
end;

type
  TNewProcess=class(TProcess)

   // function ges: boolean;

    function RunCommandLoop(out outputstring: string; out stderrstring: string;
      out anexitstatus: integer): integer; override;
  end;
   {
function tnewprocess.ges: boolean;
begin
  Result:=GetExitCodeProcess(ProcessHandle,FExitCode) and (FExitCode<>Still_Active);
  if not result then
    WaitForSingleObject(FProcessHandle,10);
end;  }

function TNewProcess.RunCommandLoop(out outputstring:string;
                              out stderrstring:string; out anexitstatus:integer):integer;
  var
      bytesread : integer;
      outputlength, stderrlength : integer;
      stderrbytesread : integer;
      gotoutput,gotoutputstderr : boolean;

      tries: integer;
  begin
    OutputString:='';
    result:=-1;
      try
      Options := Options + [poUsePipes];
      bytesread:=0;
      outputlength:=0;
      stderrbytesread:=0;
      stderrlength:=0;
      Execute;

      while WaitForSingleObject(FProcessHandle,0)=WAIT_TIMEOUT do
        begin
          // Only call ReadFromStream if Data from corresponding stream
          // is already available, otherwise, on  linux, the read call
          // is blocking, and thus it is not possible to be sure to handle
          // big data amounts bboth on output and stderr pipes. PM.
          gotoutput:=ReadInputStream(output,BytesRead,OutputLength,OutputString,1);
          // The check for assigned(P.stderr) is mainly here so that
          // if we use poStderrToOutput in p.Options, we do not access invalid memory.
          gotoutputstderr:=false;
          if assigned(stderr) then
              gotoutputstderr:=ReadInputStream(StdErr,StdErrBytesRead,StdErrLength,StdErrString,1);

         { if (porunidle in options) and not gotoutput and not gotoutputstderr and Assigned(FOnRunCommandEvent) Then
            FOnRunCommandEvent(self,Nil,RunCommandIdle,'');  }
        end;
      // Get left output after end of execution


    //  WaitForSingleObject(FProcessHandle,INFINITE);
     //  WaitForThreadTerminate(FThreadHandle, 0);

      //ThreadSwitch;
      ReadInputStream(output,BytesRead,OutputLength,OutputString,2500);
      setlength(outputstring,BytesRead);
      if assigned(stderr) then
        ReadInputStream(StdErr,StdErrBytesRead,StdErrLength,StdErrString,250);
      setlength(stderrstring,StderrBytesRead);
      anexitstatus:=exitstatus;
      result:=0; // we came to here, document that.
  {    if Assigned(FOnRunCommandEvent) then          // allow external apps to react to that and finish GUI
        FOnRunCommandEvent(self,Nil,RunCommandFinished,'');}

      except
        on e : Exception do
           begin
             result:=1;
             setlength(outputstring,BytesRead);
             setlength(stderrstring,StderrBytesRead);
             {if Assigned(FOnRunCommandEvent) then
               FOnRunCommandEvent(self,Nil,RunCommandException,e.Message);  }
           end;
       end;
  end;

function RunCommandIndir2(const curdir:TProcessString;const exename:TProcessString;const commands:array of TProcessString;out outputstring:string;out exitstatus:integer; Options : TProcessOptions = [];SWOptions:TShowWindowOptions=swoNone):integer;
Var
    p : TNewProcess;
    i : integer;
    ErrorString : String;
begin
  p:=TNewProcess.create(nil);
  if Options<>[] then
    P.Options:=Options-[poWaitOnExit];
  P.ShowWindow:=SwOptions;
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  try
    result:=p.RunCommandLoop(outputstring,errorstring,exitstatus);
  finally
    p.free;
  end;
end;

function lua_runCommand(L: PLua_State): integer; cdecl;
var
  p: array of TProcessString;
  curdir: string;
  exe: string;
  parameters: string;

  s: string;
  i,pl: integer;
  exitstatus: integer;
begin
  p:=[];
  if lua_gettop(L)>=1 then
  begin
    s:='';

    exe:=Lua_ToString(L,1);

    if lua_gettop(L)>=2 then
    begin
      if lua_istable(L,2) then
      begin
        pl:=lua_objlen(L, 2);
        setlength(p,pl);

        for i:=1 to pl do
        begin
          lua_pushinteger(L,i);
          lua_gettable(L,2);

          if lua_isstring(L,-1) then
          begin
            p[i-1]:=Lua_ToString(L,-1);
            lua_pop(L,1);
          end
          else
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Invalid parameter at index '+inttostr(i));
            exit(2);
          end;

        end;
      end
      else
      if lua_isstring(L,2) then
      begin
        SetLength(p,1);
        p[0]:=Lua_ToString(L,2);
      end;
    end
    else
      setlength(p,0);

    if lua_gettop(L)>=3 then
      curdir:=Lua_ToString(L,3)
    else
      curdir:='';

    RunCommandInDir2(curdir, exe, p, s, exitstatus, [poNoConsole]);
    lua_pushstring(L,s);
    lua_pushinteger(L,exitstatus);
    exit(2);
  end
  else
  begin
    lua_pushnil(L);
    lua_pushstring(L,rsIncorrectNumberOfParameters);
    exit(2);
  end;

end;

function lua_shellExecute(L: PLua_State): integer; cdecl;
var
  pcount: integer;
  command: string;
  parameters: string;
  folder: string;
  showcommand: integer;
begin
  pcount:=lua_gettop(L);
  if pcount>=1 then
  begin
    command:=utf8toansi(lua_tostring(L, -pcount));

    if pcount>=2 then
      parameters:=utf8toansi(lua_tostring(L, -pcount+1))
    else
      parameters:='';


    if pcount>=3 then
      folder:=utf8toansi(lua_tostring(L, -pcount+2))
    else
      folder:='';

    if pcount>=4 then
      showcommand:=lua_tointeger(L, -pcount+3)
    else
      showcommand:=SW_NORMAL;

    shellexecute(0,'open',pchar(command),pchar(parameters),pchar(folder),showcommand);
  end;

  lua_pop(L, lua_gettop(L));

  result:=0;

end;

function getTickCount_lua(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  result:=1;
  lua_pushinteger(L, GetTickCount64);
end;

function lua_rdtsc(L: PLua_State): integer; cdecl;
var v: qword;
begin
  {$ifndef darwinarm64}
  {$ifdef cpu32}
  asm
    push edx
    push edi
    rdtsc
    lea edi,v
    mov [edi],eax
    mov [edi+4],edx
    pop edi
    pop edx
  end;
  {$else}
  asm
    push rdx
    rdtsc
    shl rax,32
    shr rax,32
    shl rdx,32
    or rax,rdx
    pop rdx

    mov v,rax
  end;
  {$endif}

  lua_pushinteger(L,v);
  result:=1;
{$else}
  exit(0);
{$endif}

end;

function processMessages(L: PLua_State): integer; cdecl;
begin
  lua_pop(L, lua_gettop(L));
  result:=0;
  application.ProcessMessages;
end;

function integerToUserData(L: PLua_State): integer; cdecl;
var
  i: integer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    i:=lua_tointeger(L,-1);
    lua_pop(L, parameters);

    luaclass_newClass(L, pointer(ptruint(i)));
    result:=1;

  end else lua_pop(L, parameters);
end;

function userDataToInteger(L: PLua_State): integer; cdecl;
var
  u: pointer;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    u:=lua_toceuserdata(L,-1);
    lua_pop(L, parameters);

    lua_pushinteger(L, ptruint(u));
    result:=1;

  end else lua_pop(L, parameters);
end;

function writeToClipboard(L: PLua_State): integer; cdecl;
var
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
    Clipboard.AsText:=Lua_ToString(L, -1);


  lua_pop(L, parameters);
end;

function readFromClipboard(L: PLua_State): integer; cdecl;
var s: string;
begin
  lua_pop(L, lua_gettop(L));

  lua_pushstring(L, Clipboard.AsText);
  result:=1;
end;




function createBitmap(L: Plua_State): integer; cdecl;
var
  Bitmap: TBitmap;
  parameters: integer;
  width, height: integer;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    width:=lua_tointeger(L, 1)
  else
    width:=screen.width;

  if parameters>=2 then
    height:=lua_tointeger(L, 2)
  else
    height:=screen.height;


  lua_pop(L, parameters);

  Bitmap:=TBitmap.Create;
  bitmap.Width:=width;
  bitmap.Height:=height;


  luaclass_newClass(L, Bitmap);
  result:=1;
end;

function createPNG(L: Plua_State): integer; cdecl;
var
  png: TPortableNetworkGraphic;
  parameters: integer;
  width, height: integer;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    width:=lua_tointeger(L, 1)
  else
    width:=screen.width;

  if parameters>=2 then
    height:=lua_tointeger(L, 2)
  else
    height:=screen.height;


  lua_pop(L, parameters);

  png:=TPortableNetworkGraphic.Create;
  png.Width:=width;
  png.Height:=height;


  luaclass_newClass(L, Png);
  result:=1;
end;

function createJpeg(L: Plua_State): integer; cdecl;
var
  jpeg: TJPEGImage;
  parameters: integer;
  width, height: integer;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    width:=lua_tointeger(L, 1)
  else
    width:=screen.width;

  if parameters>=2 then
    height:=lua_tointeger(L, 2)
  else
    height:=screen.height;


  lua_pop(L, parameters);

  jpeg:=TJPEGImage.Create;
  jpeg.Width:=width;
  jpeg.Height:=height;


  luaclass_newClass(L, jpeg);
  result:=1;
end;

function createIcon(L: Plua_State): integer; cdecl;
var
  Icon: TIcon;
  parameters: integer;
  width, height: integer;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
    width:=lua_tointeger(L, 1)
  else
    width:=64;

  if parameters>=2 then
    height:=lua_tointeger(L, 2)
  else
    height:=64;


  lua_pop(L, parameters);

  icon:=TIcon.Create;
  icon.Width:=width;
  icon.Height:=height;


  luaclass_newClass(L, icon);
  result:=1;
end;

function lua_waitforsymbols(L: Plua_State): integer; cdecl;
begin
  result:=1;
  lua_pushboolean(L,waitforsymbols);

  if lua_gettop(L)>0 then
    waitforsymbols:=lua_toboolean(L,1);


end;

function errorOnLookupFailure(L: Plua_State): integer; cdecl;
var
  parameters: integer;

  oldvalue: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  oldvalue:=symhandler.ExceptionOnLuaLookup;

  if parameters=1 then
  begin
    symhandler.ExceptionOnLuaLookup:=lua_toboolean(L, -1);
    lua_pop(L, parameters);
    lua_pushboolean(L, oldvalue);
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;

function loadPlugin(L: PLua_State): integer; cdecl;
var
  p: string;
  parameters: integer;
  pluginid: integer;
begin
  result:=1;
  pluginid:=0;

  parameters:=lua_gettop(L);

  if parameters=1 then
  begin
    p:=Lua_ToString(L, -1);
    try

      pluginid:=pluginhandler.LoadPlugin(p);
      if pluginid<>-1 then
        pluginhandler.EnablePlugin(pluginid);
    except
      pluginid:=-1;
    end;

  end;

  lua_pop(L, parameters);

  if pluginid=-1 then
    lua_pushnil(L)
  else
    lua_pushinteger(L, pluginid);
end;

function getCEVersion(L: PLua_State): integer; cdecl;
var
  p: string;
  parameters: integer;
  pluginid: integer;
begin
  lua_pop(L, lua_gettop(L));

  result:=1;
  lua_pushnumber(L, ceversion);
end;

function lua_Utf8ToAnsi(L: Plua_State): integer; cdecl;
var
  s: string;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters=1 then
  begin
    s:=Lua_ToString(L, -1);
    lua_pop(L, parameters);

    lua_pushstring(L, UTF8ToWinCP(s));
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;

function lua_AnsitoUTF8(L: Plua_State): integer; cdecl;
var
  s: string;
  parameters: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);

  if parameters=1 then
  begin
    s:=Lua_ToString(L, -1);
    lua_pop(L, parameters);

    lua_pushstring(L, WinCPToUTF8(s));
    result:=1;
  end
  else
    lua_pop(L, parameters);
end;


function fullAccess(L: PLua_state): integer; cdecl;
var parameters: integer;
  address: ptruint;
  size: integer;
  op: dword;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=2 then
  begin
    address:=lua_toaddress(L,1);

    size:=lua_tointeger(L,2);

    lua_pop(L, lua_gettop(l));

    if virtualprotectex(processhandle,pointer(address),size,PAGE_EXECUTE_READWRITE,op) then
      lua_pushboolean(L,true)
    else
      lua_pushboolean(L,false);

    result:=1;
  end;
end;

function lua_setMemoryProtection(L: PLua_state): integer; cdecl;
var parameters: integer;
  address: ptruint;
  size: integer;
  prot: dword;
  op: dword;
  R,W,X: boolean;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=3 then
  begin
    address:=lua_toaddress(L,1);
    size:=lua_tointeger(L,2);
    if lua_istable(L,3) then
    begin
      lua_pushstring(L,'R');
      lua_gettable(L,3);
      if lua_isnil(L,-1) then
        r:=false
      else
        r:=lua_toboolean(L,-1);
      lua_pop(L,1);

      lua_pushstring(L,'W');
      lua_gettable(L,3);
      if lua_isnil(L,-1) then
        w:=false
      else
        w:=lua_toboolean(L,-1);
      lua_pop(L,1);

      lua_pushstring(L,'X');
      lua_gettable(L,3);
      if lua_isnil(L,-1) then
        x:=false
      else
        x:=lua_toboolean(L,-1);
      lua_pop(L,1);

      if (not SystemSupportsWritableExecutableMemory) and (w and x) then
      begin
        lua_pushboolean(L,false);
        lua_pushstring(L,'This system does not support writable executable memory.  Tip: Pause the process, make writable, write, make executable, continue');
        exit(2);
      end;

      if not (r or w or x) then
        prot:=PAGE_NOACCESS
      else
      begin
        if not w and not x then
          prot:=PAGE_READONLY
        else
        if w and not x then
          prot:=PAGE_READWRITE
        else
        if not w and x then
          prot:=PAGE_EXECUTE_READ
        else
        if w and x then
          prot:=PAGE_EXECUTE_READWRITE;
      end;
    end
    else
    if lua_isnumber(L,3) then //undocumented
      prot:=lua_tointeger(L,3)
    else
    begin
      lua_pushboolean(L,false);
      lua_pushstring(L,'Unexpected type for memory protection');
      exit(2);
    end;

    lua_pop(L, lua_gettop(l));

    if virtualprotectex(processhandle,pointer(address),size,prot,op) then
    begin
      lua_pushboolean(L,true);
      lua_pushinteger(L, op);
    end
    else
      lua_pushboolean(L,false);

    result:=1;
  end;
end;

function getWindowList_lua(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  s: tstrings;
  i,j: integer;
  pid: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    s:=lua_toceuserdata(L,1);
    lua_pop(L, lua_gettop(l));
    if (s<>nil) and (s is TStrings) then
    begin
      GetWindowList(s);
      sanitizeProcessList(s);

    end
    else
    begin
      lua_pushstring(L,rsGetProcessListTheProvidedListObjectIsNotValid);
      lua_error(L);
    end;
  end
  else
  begin
    //table version
    s:=tstringlist.create;
    GetWindowList(s);
    sanitizeProcessList(s);



    lua_newtable(L);

    for i:=0 to s.Count-1 do
    begin
      if TryStrToInt('0x'+copy(s[i],1,8), pid) then
      begin
        lua_pushinteger(L, pid);
        lua_gettable(L,1);

        if lua_isnil(L,-1) then
        begin
          //not yet in the list
          j:=lua_gettop(L);

          lua_pop(L,1);
          lua_pushinteger(L, pid);
          lua_newtable(L);
          lua_settable(L,1);

          lua_pushinteger(L, pid);
          lua_gettable(L,1);
        end;

        j:=lua_objlen(L,-1);
        lua_pushinteger(L, j+1);
        lua_pushstring(L, copy(s[i], 10, length(s[i])));
        lua_settable(L, -3);

        lua_pop(L,1); //pop the current processid table
      end;
    end;

    s.free;

    result:=1; //table
  end;
end;

function getProcesslist_lua(L: PLua_state): integer; cdecl;
var
  parameters: integer;
  s: tstrings;
  i: integer;
  pid: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    s:=lua_toceuserdata(L,1);
    lua_pop(L, lua_gettop(l));
    if (s<>nil) and (s is TStrings) then
      GetProcessList(s, false, true)
    else
    begin
      lua_pushstring(L,rsGetProcessListTheProvidedListObjectIsNotValid);
      lua_error(L);
    end;
  end
  else
  begin
    //table version
    s:=tstringlist.create;
    GetProcessList(s, false, true);

    lua_newtable(L);

    for i:=0 to s.Count-1 do
    begin
      if TryStrToInt('0x'+copy(s[i],1,8), pid) then
      begin
        lua_pushinteger(L, pid);
        lua_pushstring(L, copy(s[i], 10, length(s[i])));
        lua_settable(L, 1);
      end;
    end;
    s.free;

    result:=1; //table
  end;
end;

function getThreadlist_lua(L: PLua_state): integer; cdecl;
var parameters: integer;
  s: tstrings;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    s:=lua_toceuserdata(L,1);
    lua_pop(L, lua_gettop(l));
    if (s<>nil) and (s is TStrings) then
      GetThreadList(s)
    else
    begin
      lua_pushstring(L,rsGetThreadlistTheProvidedListObjectIsNotValid);
      lua_error(L);
    end;
  end
  else
    lua_pop(L, lua_gettop(l));
end;

function lua_loadTable(L: Plua_State): integer; cdecl;
var
  filename: string='';
  parameters: integer;
  merge: boolean;
  doc: TXMLDocument;
  s: Tstream;
  ignoreluascriptdialog: boolean;
begin
  result:=0;
  s:=nil;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    if lua_isstring(L, 1) then
    begin
      filename:=Lua_ToString(L, 1);
      if (not fileexists(filename)) and (fileexists(wincptoutf8(filename))) then
        filename:=wincptoutf8(filename);
    end
    else
    begin
      s:=lua_toceuserdata(L, 1);
      if s=nil then
        exit;
    end;

    if parameters>=2 then
      merge:=lua_toboolean(L,2)
    else
      merge:=false;

    try

      if s<>nil then //read a stream
      begin
        ignoreluascriptdialog:=false;
        if parameters>=3 then
          ignoreluascriptdialog:=lua_toboolean(L,3);

        ReadXMLFile(doc, s);
        loadxml(doc, merge, ignoreluascriptdialog);
      end
      else
        loadtable(filename,merge);

      lua_pushboolean(L,true);
      result:=1;
    except
      on e:exception do
      begin
        lua_pushboolean(L,false);
        lua_pushstring(L,e.message);
        result:=2;
      end
    end;
  end;
end;

function lua_saveTable(L: Plua_State): integer; cdecl;
var
  filename: string;
  s: tstream=nil;
  parameters: integer;
  protect: boolean;
  dontDeactivateDesignerForms: boolean;
  doc: TXMLDocument;
begin
  result:=0;

  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    if lua_isstring(L, 1) then
    begin
      filename:=Lua_ToString(L, 1);
    end
    else
    begin
      s:=lua_toceuserdata(L, 1);
      if s=nil then
        exit;

      if not (s is TStream) then
        exit;
    end;

    if parameters>=2 then
      protect:=lua_toboolean(L,2)
    else
      protect:=false;

    if parameters>=3 then
      dontDeactivateDesignerForms:=lua_toboolean(L,3)
    else
      dontDeactivateDesignerForms:=true;

    if s<>nil then
    begin
      doc:=TXMLDocument.Create;
      try
        SaveXML(doc, dontDeactivateDesignerForms, true);
        WriteXMLFile(doc, s);
        lua_pushboolean(L,true);
        result:=1;
      except
        on e:exception do
        begin
          lua_pushboolean(L,false);
          lua_pushstring(L,e.message);
          result:=2;
        end
      end;

      doc.free;
    end
    else
    begin
      try
        savetable(filename, protect, dontDeactivateDesignerForms);
        lua_pushboolean(L,true);
        result:=1;
      except
        on e:exception do
        begin
          lua_pushboolean(L,false);
          lua_pushstring(L,e.message);
          result:=2;
        end
      end;
    end;

  end;
end;

function lua_signTable(L: Plua_State): integer; cdecl;
var
  filename: string;
begin
  {$ifdef windows}
  if lua_gettop(L)>0 then
  begin
    filename:=Lua_ToString(L,1);

    if FileExists(filename) then
    begin
      try
        signTableFile(filename);
        lua_pushboolean(L,true);
        exit(1);
      except
        on e: exception do
        begin
          lua_pushboolean(L,false);
          lua_pushstring(L,e.message);
          exit(2);
        end;
      end;
    end
    else
    begin
      lua_pushboolean(L,false);
      lua_pushstring(L,filename+' not found');
      exit(2);
    end;
  end
  else
  begin
    lua_pushboolean(L,false);
    lua_pushstring(L, rsIncorrectNumberOfParameters);
    exit(2);
  end;
  {$else}
  lua_pushboolean(L,false);
  lua_pushstring(L,'This version does not support signing yet');
  exit(2);
  {$endif}
end;

function lua_detachIfPossible(L: Plua_State): integer; cdecl;
begin
  result:=0;
  DetachIfPossible;
  lua_pop(L, lua_gettop(L));
end;

function getComment(L: PLua_state): integer; cdecl;
var address: ptruint;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    address:=lua_toaddress(L,1);

    lua_pushstring(L, dassemblercomments.comments[address]);
    result:=1;
  end;
end;

function setComment(L: PLua_state): integer; cdecl;
var address: ptruint;
  comment: string;
begin
  result:=0;
  if lua_gettop(L)=2 then
  begin
    address:=lua_toaddress(L,1);

    comment:=Lua_ToString(L, 2);

    dassemblercomments.comments[address]:=comment;
  end;
end;

function getHeader(L: PLua_state): integer; cdecl;
var address: ptruint;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    address:=lua_toaddress(L,1);

    lua_pushstring(L, dassemblercomments.commentHeader[address]);
    result:=1;
  end;
end;

function setHeader(L: PLua_state): integer; cdecl;
var address: ptruint;
  Header: string;
begin
  result:=0;
  if lua_gettop(L)=2 then
  begin
    address:=lua_toaddress(L,1);

    Header:=Lua_ToString(L, 2);

    dassemblercomments.commentHeader[address]:=Header;
  end;
end;

function lua_createClass(L: PLua_State): integer; cdecl;
var
  classname: string;
  c: TPersistentClass;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    classname:=Lua_ToString(L,1);

    c:=GetClass(classname);

    if c=nil then
    begin
      lua_pushnil(L);
      lua_pushstring(L,Classname+' is not available');
      exit(2);
    end;

    luaclass_newClass(L, c.Create);
    result:=1;
  end;
end;

function lua_createComponentClass(L: PLua_State): integer; cdecl;
var
  classname: string;
  owner: TComponent;

  c: TPersistentClass;
begin
  result:=0;
  if lua_gettop(L)=2 then
  begin
    classname:=Lua_ToString(L,1);
    c:=GetClass(classname);

    if c=nil then
    begin
      lua_pushnil(L);
      lua_pushstring(L,Classname+' is not available');
      exit(2);
    end;

    owner:=lua_ToCEUserData(L,2);
    luaclass_newClass(L, TComponentClass(c).Create(owner));
    result:=1;
  end;
end;

function openLuaServer(L: PLua_State): integer; cdecl;
var name: string;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)=1 then
    name:=Lua_ToString(L, 1)
  else
    name:='cheatenginebla';

  if luaserverExists(name)=false then
    tluaserver.create(name);
  {$ENDIF}
end;

function lua_registerAutoAssemblerCommand(L: PLua_State): integer; cdecl;
var command: string;
  f: integer;
  routine: string;
  lc: tluacaller;
begin
  result:=0;

  if lua_gettop(L)=2 then
  begin
    if lua_isstring(L, 1) then
    begin
      command:=Lua_ToString(L, 1);

      if lua_isfunction(L,2) then
      begin
        f:=luaL_ref(L,LUA_REGISTRYINDEX);

        lc:=TLuaCaller.create;
        lc.luaroutineIndex:=f;
      end
      else
      if lua_isstring(L,2) then
      begin
        routine:=lua_tostring(L,2);
        lc:=TLuaCaller.create;
        lc.luaroutine:=routine;
      end
      else exit;

      RegisterAutoAssemblerCommand(command, lc.AutoAssemblerCallback);
    end;

  end;

end;

function lua_unregisterAutoAssemblerCommand(L: PLua_State): integer; cdecl;
var command: string;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    command:=Lua_ToString(L, 1);
    UnregisterAutoAssemblerCommand(command);
  end;
end;

function lua_registerSymbolLookupCallback(L: PLua_State): integer; cdecl;
var
  f: integer;
  sltype: TSymbolLookupCallbackPoint;
  routine: string;
  lc: tluacaller;
  i: integer;
begin
  result:=0;

  if lua_gettop(L)=2 then
  begin
    if lua_isnumber(L, 2) then sltype:=TSymbolLookupCallbackPoint(lua_tointeger(L, 2)) else exit;
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    i:=registerSymbolLookupCallback(lc.SymbolLookupCallback, sltype);

    lua_pushinteger(L, lua_integer(i));
    result:=1;
  end;

end;

function lua_unregisterSymbolLookupCallback(L: PLua_State): integer; cdecl;
var id: integer;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterSymbolLookupCallback(lua_tointeger(L, 1));
end;

function lua_registerAddressLookupCallback(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;
  lc: tluacaller;
begin
  result:=0;

  if lua_gettop(L)=1 then
  begin
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    lua_pushinteger(L, registerAddressLookupCallback(lc.AddressLookupCallback));
    result:=1;
  end;

end;

function lua_unregisterAddressLookupCallback(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterAddressLookupCallback(lua_tointeger(L, 1));
end;

//----
function lua_registerStructureAndElementListCallback(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;
  lc,lc2: tluacaller;
begin
  result:=0;

  if lua_gettop(L)>=2 then
  begin
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    if lua_isfunction(L, 2) then
    begin
      lua_pushvalue(L, 2);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc2:=TLuaCaller.create;
      lc2.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,2) then
    begin
      routine:=lua_tostring(L,2);
      lc2:=TLuaCaller.create;
      lc2.luaroutine:=routine;
    end
    else exit;

    lua_pushinteger(L, registerStructureAndElementListCallback(lc.StructureListCallback, lc2.ElementListCallback));
    result:=1;
  end;

end;

function lua_unregisterStructureAndElementListCallback(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterStructureAndElementListCallback(lua_tointeger(L, 1));
end;
//----

function lua_registerGlobalDisassembleOverride(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;
  lc: tluacaller;
begin
  result:=0;

  if lua_gettop(L)=1 then
  begin
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    lua_pushinteger(L, registerGlobalDisassembleOverride(lc.DisassembleEvent));
    result:=1;
  end;

end;

function lua_unregisterGlobalDisassembleOverride(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterGlobalDisassembleOverride(lua_tointeger(L, 1));
end;

function lua_registerStructureDissectOverride(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;
  lc: tluacaller;
  m: Tmethod;
begin
  result:=0;

  if lua_gettop(L)=1 then
  begin
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    lua_pushinteger(L, registerStructureDissectOverride(lc.StructureDissectEvent));
    result:=1;
  end;
end;

function lua_unregisterStructureDissectOverride(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterStructureDissectOverride(lua_tointeger(L, 1));
end;


function lua_registerStructureNameLookup(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;
  lc: tluacaller;
begin
  result:=0;

  if lua_gettop(L)=1 then
  begin
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    lua_pushinteger(L, registerStructureNameLookup(lc.StructureNameLookup));
    result:=1;
  end;
end;

function lua_unregisterStructureNameLookup(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterStructureNameLookup(lua_tointeger(L, 1));
end;

//
function lua_registerGlobalStructureListUpdateNotification(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;
  lc: tluacaller;
begin
  result:=0;

  if lua_gettop(L)=1 then
  begin
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    lua_pushinteger(L, registerGlobalStructureListUpdateNotification(lc.NotifyEvent));
    result:=1;
  end;
end;

function lua_unregisterGlobalStructureListUpdateNotification(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterGlobalStructureListUpdateNotification(lua_tointeger(L, 1));
end;

//

function lua_registerAssembler(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;
  lc: tluacaller;
begin
  result:=0;

  if lua_gettop(L)=1 then
  begin
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    lua_pushinteger(L, registerAssembler(lc.AssemblerEvent));
    result:=1;
  end;
end;

function lua_unregisterAssembler(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterAssembler(lua_tointeger(L, 1));
end;


function lua_registerAutoAssemblerPrologue(L: PLua_State): integer; cdecl;
var
  f: integer;
  routine: string;
  lc: tluacaller;
  postaob: boolean;
begin
  result:=0;

  if lua_gettop(L)>=1 then
  begin
    if lua_isfunction(L, 1) then
    begin
      lua_pushvalue(L, 1);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,1) then
    begin
      routine:=lua_tostring(L,-1);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    if lua_gettop(L)=2 then
      postaob:=lua_toboolean(L,2)
    else
      postaob:=false;

    lua_pushinteger(L, registerAutoAssemblerPrologue(lc.AutoAssemblerPrologueEvent, postaob));
    result:=1;
  end;
end;

function lua_unregisterAutoAssemblerPrologue(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>0 then
    unregisterAutoAssemblerPrologue(lua_tointeger(L, 1));
end;

function lua_shortCutToText(L: PLua_State): integer; cdecl;
begin
  if lua_gettop(L)>0 then
  begin
    lua_pushstring(L, ShortCutToText(TShortCut(lua_tointeger(L, 1))));
    result:=1;
  end
  else
    result:=0;
end;

function lua_textToShortCut(L: PLua_State): integer; cdecl;
begin
  if lua_gettop(L)>0 then
  begin
    lua_pushinteger(L, TextToShortCut(Lua_ToString(L, 1)));
    result:=1;
  end
  else
    result:=0;
end;

function lua_outputDebugString(L: PLua_State): integer; cdecl;
begin
  if lua_gettop(L)>0 then
    OutputDebugString(pchar(Lua_ToString(L, 1)));

  result:=0;
end;

function getUserRegistryEnvironmentVariable(L: PLua_State): integer; cdecl;
var
  name: string;
  r: Tregistry;
begin
  result:=0;
  if lua_gettop(L)>0 then
  begin
    name:=Lua_ToString(L, 1);

    r:=tregistry.Create;
    try
      r.RootKey:=HKEY_CURRENT_USER;
      if r.OpenKey('\Environment',false) then
      begin
        if r.ValueExists(name) then
        begin
          lua_pushstring(L, r.ReadString(name));
          result:=1;
        end;
      end;
    finally
      r.free;
    end;

  end;
end;

function setUserRegistryEnvironmentVariable(L: PLua_State): integer; cdecl;
var
  name, value: string;
  r: tregistry;
begin
  result:=0;
  if lua_gettop(L)>1 then
  begin
    name:=Lua_ToString(L, 1);
    value:=Lua_ToString(L, 2);

    r:=tregistry.Create;
    try
      r.RootKey:=HKEY_CURRENT_USER;
      if r.OpenKey('\Environment',false) then
        r.WriteString(name, value);
    finally
      r.free;
    end;
  end;
end;

function broadcastEnvironmentUpdate(L: PLua_State): integer; cdecl;
{$if FPC_FULLVERSION<=30002}
var rv: DWORD; //bug in laz 1.6.4 (not the end of the world, as rv is on a 8 byte boundary in the stack and not used)
{$else}
var rv: DWORD_PTR;
{$endif}
begin
  result:=0;

  {$IFDEF windows}
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(pchar('Environment')), SMTO_ABORTIFHUNG, 5000, rv);
  {$ENDIF}
end;

{$IFDEF windows}
var winmm: THandle=0;
var PlaySoundA: function (pszSound: LPCSTR; hmod: HModule; fdwSound: DWORD): BOOL; stdcall;
{$ENDIF}

function lua_playSound(L: PLua_State): integer; cdecl;
const
    SND_NODEFAULT = 2;
    SND_MEMORY = 4;
    SND_LOOP = 8;
    SND_NOSTOP = 16;
    SND_SYNC = 0;
    SND_ASYNC = 1;
    SND_PURGE = 64;
    SND_APPLICATION = 128;
var
  o: tobject;
  ms: TMemorystream;
  playparam: dword;
begin
  result:=0;

  {$IFDEF windows}
  if lua_gettop(L)>=1 then
  begin
    if winmm=0 then
      winmm:=LoadLibrary('winmm.dll');

    if (winmm<>0) then
    begin
      if not assigned(PlaySoundA) then
        PlaySoundA:=GetProcAddress(winmm,'PlaySoundA');

      if assigned(PlaySoundA) then
      begin
        if lua_isuserdata(L,1) then
        begin
          o:=lua_ToCEUserData(L, 1);

          if o is TLuafile then
            ms:=TLuaFile(o).stream
          else
          if o is TMemoryStream then
            ms:=TMemoryStream(o)
          else
            raise exception.create(rsPlaySoundTheParameterMustBeATableFileOrAMemoryStream);

          playparam:=SND_MEMORY;
          if (lua_gettop(L)>=2) and lua_toboolean(L,2) then
            playparam:=playparam or SND_SYNC
          else
            playparam:=playparam or SND_ASYNC;

          PlaySoundA(ms.Memory, 0, playparam);
        end;


      end;
    end;
  end;
  {$ENDIF}

end;

function createRef(L: PLua_State): integer; cdecl;
begin
  lua_pushinteger(L, luaL_ref(L, LUA_REGISTRYINDEX));
  result:=1;
end;

function getRef(L: PLua_State): integer; cdecl;
begin
  lua_rawgeti(Luavm, LUA_REGISTRYINDEX, lua_tointeger(L,1));
  result:=1;
end;

function destroyRef(L: PLua_State): integer; cdecl;
begin
  luaL_unref(L, LUA_REGISTRYINDEX, lua_tointeger(L,1));
  result:=0;
end;

function activateProtection(L: PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)=0 then
    protectme
  else
    protectme(lua_tointeger(L,1));
end;

function getLuaEngine(L:PLua_State): integer; cdecl;
begin
  if frmLuaEngine=nil then
    frmLuaEngine:=TfrmLuaEngine.Create(application);

  luaclass_newClass(L, frmLuaEngine);
  result:=1;
end;


function createLuaEngine(L:PLua_State): integer; cdecl;
var f: TfrmLuaEngine;
begin
  f:=TfrmLuaEngine.Create(application);

  if frmLuaEngine<>nil then
    f.miView.Visible:=false
  else
    frmLuaEngine:=f;

  luaclass_newClass(L, f);
  result:=1;
end;

function getApplication(L:PLua_State): integer; cdecl;
begin
  luaclass_newClass(L, application);
  result:=1;
end;

function lua_stringToMD5String(L:PLua_State): integer; cdecl;
var msg: pchar;
    size: size_t;
begin
  if lua_gettop(L)=1 then
  begin
    msg:=lua_tolstring(L, 1, @size);
    lua_pushstring(L, MD5Print(MD5Buffer(msg^,size)));
    result:=1;
  end
  else
    result:=0;
end;

function lua_ConvertKeyComboToString(L:PLua_State): integer; cdecl;
var
  keycombo: TKeyCombo;
  keycount: integer;
  i: integer;
begin
  zeromemory(@keycombo, sizeof(keycombo));
  keycount:=min(lua_gettop(L), 5);

  if (keycount=1) and lua_istable(L,1) then
  begin
    for i:=0 to 4 do
    begin
      lua_pushinteger(L, i+1);
      lua_gettable(L, 1);
      if lua_isnil(L, -1) then  //end of the list
      begin
        lua_pop(L,1);
        break;
      end
      else
      begin
        keycombo[i]:=lua_tointeger(L,-1);
        lua_pop(L,1);
      end;
    end;

  end
  else
  begin
    for i:=0 to keycount-1 do
      keycombo[i]:=lua_tointeger(L, i+1);
  end;

  lua_pushstring(L, ConvertKeyComboToString(keycombo));
  result:=1;
end;

function restoreSeDebugPrivilege(L:PLua_State): integer; cdecl;
{$IFDEF windows}
var
  tp: TTokenPrivileges;
  prev: TTokenPrivileges;
  returnlength: dword;
  tokenhandle: thandle;
{$ENDIF}
begin
  result:=0;
  {$IFDEF windows}
  if ownprocesshandle <> 0 then
  begin
    if OpenProcessToken(ownprocesshandle, TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES, tokenhandle) then
    begin
      ZeroMemory(@tp, sizeof(tp));

      if lookupPrivilegeValue(nil, 'SeDebugPrivilege', tp.Privileges[0].Luid) then
      begin
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        tp.PrivilegeCount := 1; // One privilege to set
        if AdjustTokenPrivileges(tokenhandle, False, tp, sizeof(tp),  prev, returnlength) then
        begin
          lua_pushboolean(L, true);
          result:=1;
        end


      end;

      closehandle(tokenhandle);
    end;

  end;
  {$ENDIF}
end;

function lua_frexp(L:PLua_State): integer; cdecl;
var
  d: float;
  m: float;
  e: integer;
begin
  result:=0;
  if lua_gettop(l)>=1 then
  begin
    d:=lua_tonumber(L, 1);
    Frexp(d, m,e);

    lua_pushnumber(l,m);
    lua_pushnumber(l, e);
    result:=2;
  end
  else
  begin
    lua_pushstring(L, rsNumberRequired);
    lua_error(L);
  end;
end;

function lua_cosh(L:PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(l)>=1 then
  begin
    lua_pushnumber(L, cosh(lua_tonumber(L,1)));
    result:=1;
  end
  else
  begin
    lua_pushstring(L, rsNumberRequired);
    lua_error(L);
  end;
end;


function lua_sinh(L:PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(l)>=1 then
  begin
    lua_pushnumber(L, sinh(lua_tonumber(L,1)));
    result:=1;
  end
  else
  begin
    lua_pushstring(L, rsNumberRequired);
    lua_error(L);
  end;
end;

function lua_tanh(L:PLua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(l)>=1 then
  begin
    lua_pushnumber(L, tanh(lua_tonumber(L,1)));
    result:=1;
  end
  else
  begin
    lua_pushstring(L, rsNumberRequired);
    lua_error(L);
  end;
end;

function lua_getTranslationFolder(L: PLua_State): integer; cdecl;
begin
  lua_pushstring(L, translationfilepath);
  result:=1;
end;

function lua_loadPOFile(L: PLua_State): integer; cdecl;
var
  POFile: TPOFile;
  filename: string;
  postrings: TStringlist;
begin
  if lua_gettop(L)>=1 then
  begin
    postrings:=Tstringlist.create;
    try
      filename:=Lua_ToString(L, 1);
      postrings.LoadFromFile(filename{$if FPC_FULLVERSION>=030200}, true{$endif});
      if assigned(LRSTranslator) then
      begin
        if (LRSTranslator is TPOTranslator) then
        begin
          pofile:=TPOTranslator(LRSTranslator).POFile;
          pofile.ReadPOText(postrings.text);
        end;
      end;
      lua_pushboolean(L, true);
    except
      lua_pushboolean(L, false);
    end;
    postrings.free;
    result:=1;
  end
  else
    result:=0;
end;

function lua_translateid(L:PLua_state): integer; cdecl;
var
  POFile: TPOFile;
  id, orig: string;
  r: string;
begin
  if lua_gettop(L)>=1 then
  begin
    id:=Lua_ToString(L, 1);

    if lua_gettop(L)>=2 then
      orig:=Lua_ToString(L,2)
    else
      orig:='';

    r:=orig;

    if assigned(LRSTranslator) then
    begin
      if (LRSTranslator is TPOTranslator) then
      begin
        pofile:=TPOTranslator(LRSTranslator).POFile;

        if assigned(pofile) then
          r:=pofile.Translate(id, orig);
      end;
    end;

    {$ifdef altname}
    r:=altnamer(r);
    {$endif}

    lua_pushstring(L, r);
    result:=1;
  end
  else
    result:=0;
end;

function lua_translate(L:PLua_state): integer; cdecl;
var
  s: string;
  POFile: TPOFile;
  r: string;

  z: TStringList;
begin
  if lua_gettop(L)>=1 then
  begin
    r:=Lua_ToString(L, 1);

    if assigned(LRSTranslator) then
    begin
      if (LRSTranslator is TPOTranslator) then
      begin
        pofile:=TPOTranslator(LRSTranslator).POFile;

        if assigned(pofile) then
          r:=pofile.Translate('',r);
      end;
    end;

    {$ifdef altname}
    r:=altnamer(r);
    {$endif}

    lua_pushstring(L, r);
    result:=1;
  end
  else
    result:=1;
end;

function lua_registerBinUtil (L:PLua_state): integer; cdecl;
var
  name, description: string;
  path: string;
  prefix: string;
  ASParam: string;
  LDParam: string;
  OBJDUMPParam: string;
  DisassemblerCommentChar: string;


  bu: TBinUtils;

  miBu: TMenuItem;
  OnDisassemble: integer;
  arch: string;

  i: integer;
begin
  result:=0;
  if (lua_gettop(L)>0) and (lua_istable(L, 1)) then
  begin
    //get the data from the provided table
    lua_pushstring(L, 'Name');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      name:='No name'
    else
      name:=Lua_ToString(L,-1);

    lua_pop(L,1);

    lua_pushstring(L, 'Description');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      description:=''
    else
      description:=Lua_ToString(L,-1);

    lua_pop(L,1);

    lua_pushstring(L, 'Path');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      path:=''
    else
      path:=Lua_ToString(L,-1);

    lua_pop(L,1);

    lua_pushstring(L, 'Prefix');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      prefix:=''
    else
      prefix:=Lua_ToString(L,-1);

    lua_pop(L,1);

    lua_pushstring(L, 'OnDisassemble');
    lua_gettable(L, 1);

    i:=lua_gettop(L);
    if lua_isnil(L,-1) then
    begin
      onDisassemble:=0;
      lua_pop(L,1);
    end
    else
      OnDisassemble:=luaL_ref(L, LUA_REGISTRYINDEX);

    lua_pushstring(L, 'Architecture');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      arch:=''
    else
      arch:=Lua_ToString(L,-1);

    lua_pop(L,1);

    lua_pushstring(L, 'ASParam');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      ASParam:=''
    else
      ASParam:=Lua_ToString(L,-1);

    lua_pop(L,1);

    lua_pushstring(L, 'LDParam');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      LDParam:=''
    else
      LDParam:=Lua_ToString(L,-1);

    lua_pop(L,1);

    lua_pushstring(L, 'OBJDUMPParam');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      OBJDUMPParam:=''
    else
      OBJDUMPParam:=Lua_ToString(L,-1);

    lua_pop(L,1);

    lua_pushstring(L, 'DisassemblerCommentChar');
    lua_gettable(L, 1);

    if lua_isnil(L,-1) then
      DisassemblerCommentChar:=''
    else
      DisassemblerCommentChar:=Lua_ToString(L,-1);

    lua_pop(L,1);




    bu:=TBinUtils.create;

    bu.Name:=name;
    bu.description:=description;
    bu.prefix:=prefix;
    bu.path:=path;
    bu.OnDisassemble:=ondisassemble;
    bu.arch:=Arch;
    bu.ASParam:=ASParam;
    bu.LDParam:=LDParam;
    bu.OBJDUMPParam:=OBJDUMPParam;
    bu.DisassemblerCommentChar:=DisassemblerCommentChar;


    binutilslist.add(bu);

    miBu:=TMenuItem.Create(MemoryBrowser.miBinUtils);
    if bu.description<>'' then
      miBu.caption:=bu.name+' - '+bu.description
    else
      miBu.caption:=bu.name;

    miBu.Tag:=binutilslist.count-1;
    miBu.AutoCheck:=true;
    miBu.RadioItem:=true;
    miBu.OnClick:=MemoryBrowser.miBinutilsSelect.OnClick;

    MemoryBrowser.miBinUtils.Add(miBu);

    if binutilslist.count>0 then //make a menu visible so the user can choose at runtime
    begin
      MemoryBrowser.miBinUtils.visible:=true;
      MemoryBrowser.miGNUAssembler.visible:=true;
    end;
  end;
end;

function setPointerSize(L:PLua_state): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
    processhandler.overridePointerSize(lua_tointeger(L, 1));

  result:=0;
end;


function getDebugContext(L:PLua_state): integer; cdecl;
var extraregs: boolean;
begin
  result:=1;
  if lua_gettop(L)>=1 then
    extraregs:=lua_toboolean(L,1)
  else
    extraregs:=false;

  if (debuggerthread<>nil) and (debuggerthread.isWaitingToContinue) and (debuggerthread.CurrentThread<>nil) then
  begin
    LUA_SetCurrentContextState(debuggerthread.CurrentThread.ThreadId, debuggerthread.CurrentThread.context, extraregs);
    lua_pushboolean(L, true);
  end
  else
    lua_pushboolean(L, false);

end;

function setDebugContext(L:PLua_state): integer; cdecl;
var extraregs: boolean;
begin
  result:=1;
  if lua_gettop(L)>=1 then
    extraregs:=lua_toboolean(L,1)
  else
    extraregs:=false;

  if (debuggerthread<>nil) and (debuggerthread.isWaitingToContinue) and (debuggerthread.CurrentThread<>nil) then
  begin
    LUA_GetNewContextState(debuggerthread.CurrentThread.context, extraregs);
    lua_pushboolean(L, true);
  end
  else
    lua_pushboolean(L, false);
end;

function debug_updateGUI(L:PLua_state): integer; cdecl;
begin
  result:=0;
  if (debuggerthread<>nil) and (debuggerthread.isWaitingToContinue) and (debuggerthread.CurrentThread<>nil) then
  begin
    debuggerthread.CurrentThread.UpdateMemoryBrowserContext;
    MemoryBrowser.UpdateDebugContext(debuggerthread.CurrentThread.handle,debuggerthread.CurrentThread.ThreadID, false);
  end;
end;



function createExecuteMethodStub(L:PLua_state): integer; cdecl;
(*
assembles a function that takes a set parameter format and returns that address
createExecuteCodeExStub(callmethod, address, {type=x} or param1,{type=x,value=param2} or param2,...)
*)
//callmethod:
//0: stdcall
//1: cdecl
//other, not implemented yet
//
//timeout:
//0: don't wait (no return value)
//nil or -1: infinite
//else time in milliseconds
//
//
//paramtypes:
//0: integer/pointer
//1: float
//2: double
//3: asciistring (turns into 0:pointer after writing the string)
//4: widestring
//5: bytetable
var
  callmethod: integer;
  address: ptruint;
  paramcount: integer;

  i,j: integer;

  s: tstringlist;
  valuetype: integer;

  stackalloc: integer;

  instancereg: integer=1;
  regstr: string;

  stackpointer: integer;

  value: qword;


  f: single;
  floatdword: dword absolute f;
  d: double;
  doubleqword: qword absolute d;
  z: PDwordArray;


  sai: integer;
  x: ptruint;
  y,wr: dword;

  stubaddress, resultaddress: ptruint;
 // allocs: TCEAllocArray;
 // exceptionlist: TCEExceptionListArray;
  disableinfo: TDisableinfo;

  r: ptruint;
  dontfree: boolean;
   thread:thandle;



  instanceSelector: TStringlist=nil;

  valuesize: integer;
  IsInputOnly, IsOutputOnly: boolean;

  parameterlist: array of integer;
begin
  if lua_gettop(L)<3 then
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Not enough parameters. Minimum: callmethod, address, instance');
    exit(2);
  end;

  paramcount:=lua_gettop(L)-3;


  callmethod:=lua_tointeger(L,1);
  if callmethod>=2 then
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Invalid callmethod:'+inttostr(callmethod));
    exit(2);
  end;

  address:=lua_toaddress(L,2);
  setlength(parameterlist,0);

  disableinfo:=TDisableInfo.create;

  s:=tstringlist.create;

  s.Add('allocXO(stub,4096)'); //+inttohex(address,8)+')');

  s.add('stub:');

  stackpointer:=0;

  if processhandler.is64Bit then
  begin
    s.add('push rbp');
    s.add('mov rbp,rsp');
    //rbp=old rbp
    //rbp+8=return address
    //rbp+10=scratch space for param1
    s.add('mov [rbp+10],rcx'); //store rcx(the parameters) in the scratch space for this function
    s.add('mov rax,rcx');

    stackalloc:=s.add('sub rsp,'+inttohex(align(max(4,paramcount)*8,$10),1))
  end
  else
  begin
    s.add('push ebp');
    s.add('mov ebp,esp');
    s.add('mov eax,[ebp+8]'); //ebp=old ebp, ebp+4=return address, ebp+8=parameter1

    stackalloc:=s.add('sub esp,'+inttohex(paramcount*4,1));  //save this linenr in case doubles are used
  end;

  if lua_isnil(L,3)=false then  //check if instance is nil
  begin
    //instance is provided
    if lua_istable(L,3) then
    begin
      //table
      lua_pushstring(L,'regnr');
      lua_gettable(L,3);
      if lua_isnil(L,-1) then
      begin
        lua_pushinteger(L,1);
        lua_gettable(L,3);
        if not lua_isnil(L,-1) then
          instancereg:=lua_tointeger(L,-1)
        else
          instancereg:=1; //assume the user used a table and left out the instancereg cause he wants ecx/rcx

        lua_pop(L,1);
      end
      else
        instanceReg:=lua_tointeger(L,-1);

      lua_pop(L,1);
    end
    else
      instanceReg:=1; //ECX/RCX

    case instancereg of
      0: regstr:='rax';
      1:
      begin
        regstr:='rcx';
        if processhandler.is64Bit then
        begin
          stackpointer:=1; //unlike 32-bit, in 64-bit method calls it's the first param
        end;
      end;
      2: regstr:='rdx';
      3: regstr:='rbx';
      4: regstr:='rsp';
      5: regstr:='rbp';
      6: regstr:='rsi';
      7: regstr:='rdi';
      8: regstr:='r8';
      9: regstr:='r9';
      10: regstr:='r10';
      11: regstr:='r11';
      12: regstr:='r12';
      13: regstr:='r13';
      14: regstr:='r14';
      15: regstr:='r15';
    end;

    if processhandler.is64Bit=false then
    begin
      if instancereg>=8 then raise exception.create('Invalid instance register');
      regstr[1]:='e';
    end;

    instanceSelector:=tstringlist.Create;
    if processhandler.is64bit then
    begin
      //this gets added right in front of the call instruction
      instanceSelector.add('mov eax,[rbp+10]'); //get the address of the parameters
      instanceSelector.add('mov '+regstr+',[rax]'); //the first parameter is the instance
      s.add('add rax,8')  //rax now points to the first real parameter
    end
    else
    begin
      instanceSelector.add('mov eax,[ebp+8]');
      instanceSelector.add('mov '+regstr+',[eax]');
      s.add('add eax,4')  //eax now points to the first real parameter
    end;

    setlength(parameterlist, length(parameterlist)+1);
    parameterlist[length(parameterlist)-1]:=0; //pointer
  end;

  try
    //setup the types
    for i:=4 to lua_gettop(L) do
    begin
      valuetype:=0;
      valuesize:=0;
      IsInputOnly:=false;
      IsOutputOnly:=false;
      if lua_istable(l,i) then
      begin
        lua_pushstring(L,'type');
        lua_gettable(L,i);

        if lua_isnil(L,-1) then
          valuetype:=5
        else
          valuetype:=lua_tointeger(L,-1);

        lua_pop(L,1);

        if valuetype=5 then
        begin
          //check for a 'size' field, and a 'IsOutputOnly' or 'IsInputOnly'

          lua_pushstring(L,'size');
          lua_gettable(L,i);
          if not lua_isnil(L,-1) then
            valuesize:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L, 'isOutputOnly');
          lua_gettable(L,i);
          if not lua_isnil(L,-1) then
            IsOutputOnly:=lua_toboolean(L,-1);
          lua_pop(L,1);

          lua_pushstring(L, 'isInputOnly');
          lua_gettable(L,i);
          if not lua_isnil(L,-1) then
            IsOutputOnly:=lua_toboolean(L,-1);
          lua_pop(L,1);
        end;
      end
      else
      if lua_isinteger(L,i) then  //just typenumbers
        valuetype:=lua_tointeger(L,i)
      else
      begin
        lua_pushnil(L);
        lua_pushstring(L,'Unknown type for param '+inttostr(i));
        exit(2);
      end;


      case valuetype of
        0,3,4,5:  //vt5 is a bytetable
        begin
          if processhandler.is64Bit then
          begin
            case stackpointer of
              0: s.add('mov rcx,[rax+'+inttohex(stackpointer*8,2)+']');
              1: s.add('mov rdx,[rax+'+inttohex(stackpointer*8,2)+']');
              2: s.add('mov r8,[rax+'+inttohex(stackpointer*8,2)+']');
              3: s.add('mov r9,[rax+'+inttohex(stackpointer*8,2)+']');
              else
              begin
                s.add('mov rbx,[rax+'+inttohex(stackpointer*8,2)+']');
                s.add('mov qword ptr [rsp+'+inttohex(stackpointer*8,8)+'],rbx');
              end;
            end;
          end
          else
          begin
            s.add('push [eax+'+inttohex(stackpointer*4,2)+']'); //could be faster with a rep movsd , or even use edi as a pointer, but meh
          end;
          inc(stackpointer);
        end;

        1: //float(single)
        begin
          if processhandler.is64Bit then
          begin
            if stackpointer<4 then
            begin
              s.add('movss xmm'+inttostr(stackpointer)+',[rax+'+inttohex(stackpointer*8,2)+']');
            end
            else
            begin
              s.add('mov rbx,[rax+'+inttohex(stackpointer*8,2)+']');
              s.add('mov qword ptr [rsp+'+inttohex(stackpointer*8,8)+'],rbx');
            end;
          end
          else
          begin
            s.add('mov ebx,[eax+'+inttohex(stackpointer*4,2)+']');
            s.add('mov dword ptr [esp+'+inttohex(stackpointer*4,1)+'],ebx');
          end;

          inc(stackpointer);
        end;

        2: //double
        begin
          if processhandler.is64Bit then
          begin
            if stackpointer<4 then
              s.add('movsd xmm'+inttostr(stackpointer)+',[rax+'+inttohex(stackpointer*8,2)+']')
            else
            begin
              s.add('mov rbx,[rax+'+inttohex(stackpointer*8,2)+']');
              s.add('mov qword ptr [rsp+'+inttohex(stackpointer*8,8)+'],rbx');
            end;
          end
          else
          begin
            s.add('mov ebx,[eax+'+inttohex(stackpointer*4,2)+']');
            s.add('mov dword ptr [esp+'+inttohex(stackpointer*4,1)+'],ebx');

            inc(stackpointer);
            s.add('mov ebx,[eax+'+inttohex(stackpointer*4,2)+']');
            s.add('mov dword ptr [esp+'+inttohex(stackpointer*4,1)+'],ebx');
          end;
          inc(stackpointer);
        end;



        else
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Invalid parametertype '+inttostr(i+3)+'('+inttostr(valuetype)+')');
          exit(2);
        end;
      end;




      setlength(parameterlist, length(parameterlist)+1);
      if valuetype=5 then //convert it to a native type
      begin
        valuetype:=(1 shl 31) or valuesize;
        if IsOutputOnly then valuetype:=valuetype or (1 shl 30);
        if IsInputOnly then valuetype:=valuetype or (1 shl 29);
      end;
      parameterlist[length(parameterlist)-1]:=valuetype;

    end;

    if processhandler.is64Bit=false then //fix the stack for the caller
      s[stackalloc]:='sub esp,'+inttohex(stackpointer*4,1);

    if instanceSelector<>nil then
    begin
      s.AddStrings(instanceSelector);
      freeandnil(instanceSelector);
    end;
    s.add('call '+inttohex(address,8)); //ce will make it a 16 byte call if needed
    if processhandler.is64Bit then
    begin
      s.add('add rsp,'+inttohex(align(max(4,paramcount)*8,$10),1));
      s.add('pop rbp');
      s.add('ret');
    end
    else
    begin
      if callmethod=1 then
        s.add('add esp,'+inttohex(paramcount*4,1));

      s.add('pop ebp');
      s.add('ret 4');
    end;


    if autoassemble(s,false,true,false,false,disableinfo) then
    begin
      //return a table describing this stub so it can be executed

      //addressToCall
      //paramlist (types)

      lua_createtable(L,0,1);


      for i:=0 to length(disableinfo.allocs)-1 do
      begin
        if  disableinfo.allocs[i].varname='stub' then
        begin
          lua_pushstring(L,'StubAddress');
          lua_pushinteger(L,disableinfo.allocs[i].address);
          lua_settable(L,-3);

          lua_pushstring(L,'Parameters');
          lua_createtable(L,0,length(parameterlist));
          for j:=0 to length(parameterlist)-1 do
          begin
            lua_pushinteger(L,j+1);
            lua_pushinteger(L, parameterlist[j]);
            lua_settable(L,-3);
          end;

          lua_settable(L,-3);
          break;
        end;
      end;


      exit(1);




    end
    else
      exit(0);
  finally
    s.free;

    if disableinfo<>nil then
      freeandnil(disableinfo);
  end;
end;

function createExecuteCodeExStub(L:PLua_state): integer; cdecl;
var paramcount: integer;
begin
  paramcount:=lua_gettop(L);
  if paramcount<2 then
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Not enough parameters. Minimum: callmethod, address');
    exit(2);
  end;

  lua_pushnil(L);
  lua_insert(L, 3); //instance=nil
  exit(createExecuteMethodStub(L));
end;


function freeExecuteCodeExStub(L:PLua_state): integer; cdecl;
begin
  result:=0;
end;

function executeMethod(L:PLua_state): integer; cdecl; //executecodeex(callmethod, timeout, address, {instance},{param1},{param2},{param3},{...})
//executeCodeEx(callmethod, timeout, address, {type=x,value=param1} or param1,{type=x,value=param2} or param2,...)

//callmethod:
//0: stdcall
//1: cdecl
//other, not implemented yet
//
//timeout:
//0: don't wait (no return value)
//nil or -1: infinite
//else time in milliseconds
//
//
//paramtypes:
//0: integer/pointer
//1: float
//2: double
//3: asciistring (turns into 0:pointer after writing the string)
//4: widestring
var
  callmethod: integer;
  address: ptruint;
  paramcount: integer;

  i: integer;

  s: tstringlist;
  floatvalues: tstringlist;
  valuetype: integer;

  stackalloc: integer;
  floatvalueallocs: integer;

  instanceValue: ptruint;
  instancereg: integer=1;
  regstr: string;

  stackpointer: integer;

  value: qword;


  f: single;
  floatdword: dword absolute f;
  d: double;
  doubleqword: qword absolute d;
  z: PDwordArray;

  stringsize: integer;
  str: string;
  wstr: widestring;
  stringallocs: array of pointer;
  sai: integer;
  x: ptruint;
  y,wr: dword;

  stubaddress, resultaddress: ptruint;
  //allocs: TCEAllocArray;
  //exceptionlist: TCEExceptionListArray;
  disableinfo: TDisableInfo;

  r: ptruint;
  dontfree: boolean;
  timeout: dword;
  thread:thandle;
begin
  if lua_gettop(L)<4 then
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Not enough parameters. Minimum: callmethod, timeout, address, instance');
    exit(2);
  end;

  paramcount:=lua_gettop(L)-4;


  setlength(stringallocs,0);

  callmethod:=lua_tointeger(L,1);
  if callmethod>=2 then
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Invalid callmethod:'+inttostr(callmethod));
    exit(2);
  end;

  if lua_isnil(L,2) then
    timeout:=INFINITE
  else
    timeout:=lua_tointeger(L,2);

  address:=lua_toaddress(L,3);


  disableinfo:=tdisableinfo.create;
  s:=tstringlist.create;
  floatvalues:=tstringlist.create;

  s.Add('allocXO(stub, 4096)');
  if processhandler.is64Bit then
  begin
    floatvalueallocs:=s.add('allocXO(addressToCall, 8)');
    s.add('allocNX(result,8)');
  end
  else
  begin
    floatvalueallocs:=s.add('allocXO(addressToCall, 4)');
    s.add('allocNX(result,4)');
  end;

  s.add('addressToCall:');
  if processhandler.is64Bit then
    s.add('dq '+inttohex(address,8))
  else
    s.add('dd '+inttohex(address,8));


  s.add('stub:');
  if processhandler.is64Bit then
    stackalloc:=s.add('sub rsp,'+inttohex(align(max(4,paramcount)*8,$10)+8,1))
  else
    stackalloc:=s.add('sub esp,'+inttohex(paramcount*4,1));  //save this linenr in case doubles are used


  if lua_isnil(L,4)=false then  //check if instance is nil
  begin
    //instance is provided
    if lua_istable(L,4) then
    begin
      //table
      lua_pushstring(L,'regnr');
      lua_gettable(L,4);
      if lua_isnil(L,-1) then
      begin
        lua_pushinteger(L,1);
        lua_gettable(L,4);
        if not lua_isnil(L,-1) then
          instancereg:=lua_tointeger(L,-1)
        else
          instancereg:=1; //assume the user used a table and left out the instancereg cause he wants ecx/rcx

        lua_pop(L,1);
      end
      else
        instanceReg:=lua_tointeger(L,-1);

      lua_pop(L,1);

      lua_pushstring(L,'classinstance');
      lua_gettable(L,4);
      if lua_isnil(L,-1) then
      begin
        //fu
        lua_pushinteger(L,2);
        lua_gettable(L,4);
        if lua_isnil(L,-1) then
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Invalid instance');
          exit(2);
        end;

        instanceValue:=lua_tointeger(L,-1);
        lua_pop(L,1);
      end
      else
        instanceValue:=lua_tointeger(L,-1);

      lua_pop(L,1);

    end
    else
    begin
      instanceValue:=lua_tointeger(L,4);
      instanceReg:=1; //ECX/RCX
    end;

    case instancereg of
      0: regstr:='rax';
      1: regstr:='rcx';
      2: regstr:='rdx';
      3: regstr:='rbx';
      4: regstr:='rsp';
      5: regstr:='rbp';
      6: regstr:='rsi';
      7: regstr:='rdi';
      8: regstr:='r8';
      9: regstr:='r9';
      10: regstr:='r10';
      11: regstr:='r11';
      12: regstr:='r12';
      13: regstr:='r13';
      14: regstr:='r14';
      15: regstr:='r15';
    end;

    if processhandler.is64Bit=false then
    begin
      if instancereg>=8 then raise exception.create('Invalid instance register');
      regstr[1]:='e';
    end;
    s.add('mov '+regstr+','+inttohex(instanceValue,8));

  end;


  try

    //setup the parameters:
    stackpointer:=0;
    for i:=5 to lua_gettop(L) do
    begin
      valuetype:=0;
      if lua_istable(l,i) then
      begin
        lua_pushstring(L,'type');
        lua_gettable(L,i);
        if lua_isnil(L,-1) then
        begin
          lua_pop(L,1);
          lua_pushinteger(L,1);
          lua_gettable(L,i);
          if lua_isnil(L,-1) then
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Invalid parametertype '+inttostr(i+3));
            exit(2);
          end;
        end;
        valuetype:=lua_tointeger(L,-1);
        lua_pop(L,1);

        lua_pushstring(L,'value');
        lua_gettable(L,i);
        if lua_isnil(L,-1) then
        begin
          lua_pop(L,1);
          lua_pushinteger(L,2);
          lua_gettable(L,i);
          if lua_isnil(L,-1) then
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Invalid parametervalue '+inttostr(i+3));
            exit(2);
          end;
        end;
      end
      else
      begin
        //no specific type is given, guess it based on the parameters (damn those lazy ass users)
        if lua_type(L,i)=LUA_TSTRING then
          valuetype:=3
        else
        begin
          if lua_isnumber(L,i) then
          begin
            if lua_isinteger(L,i) then
              valuetype:=0 //integer/pointer
            else
              valuetype:=1; //float (if you want double then give a proper typespecficiation and FU)
          end
          else
          begin
            lua_pushnil(L);
            lua_pushstring(L,'No idea how to handle the type you provided for parameter '+inttostr(i));
            exit(2);
          end;
        end;
        lua_pushvalue(L,i);  //-1 now contains the value
      end;

      case valuetype of
        0,3,4:
        begin
          if valuetype in [3..4] then
          begin
            sai:=length(stringallocs);
            setlength(stringallocs, sai+1);

            if valuetype=3 then
            begin
              //ascii
              str:=Lua_ToString(L,-1);
              stringallocs[sai]:=virtualallocex(processhandle,nil,length(str)+1,MEM_COMMIT or MEM_RESERVE,PAGE_READWRITE);
              WriteProcessMemory(processhandle, stringallocs[sai],@str[1],length(str)+1,x);
            end
            else
            begin
              //widestring
              wstr:=Lua_ToString(L,-1);
              stringallocs[sai]:=virtualallocex(processhandle,nil,length(wstr)*2+2,MEM_COMMIT or MEM_RESERVE,PAGE_READWRITE);
              WriteProcessMemory(processhandle, stringallocs[sai],@wstr[1],length(wstr)*2+2,x);
            end;
            value:=ptruint(stringallocs[sai]);
          end
          else
            value:=lua_tointeger(L,-1);

          if processhandler.is64Bit then
          begin
            case stackpointer of
              0: s.add('mov rcx,'+inttohex(value,8));
              1: s.add('mov rdx,'+inttohex(value,8));
              2: s.add('mov r8,'+inttohex(value,8));
              3: s.add('mov r9,'+inttohex(value,8));
              else
              begin
                s.add('mov rax,'+inttohex(lua_tointeger(L,-1),8));
                s.add('mov qword ptr [rsp+'+inttohex(stackpointer*8,8)+'],rax');
              end;
            end;
          end
          else
          begin
            s.add('mov dword ptr [esp+'+inttohex(stackpointer*4,1)+'],'+inttohex(value,8));
          end;
          inc(stackpointer);
        end;

        1: //float(single)
        begin
          f:=lua_tonumber(L,-1);
          if processhandler.is64Bit then
          begin
            floatvalues.Add('floatvalue'+inttostr(stackpointer)+':');
            floatvalues.add('dd '+inttohex(floatdword,8));
            if stackpointer<4 then
            begin
              s.add('movss xmm'+inttostr(stackpointer)+',[floatvalue'+inttostr(stackpointer)+']')
            end
            else
            begin
              s.add('xor rax,rax');
              s.add('mov eax,[floatvalue'+inttostr(stackpointer)+']');
              s.add('mov qword ptr [rsp+'+inttohex(stackpointer*8,8)+'],rax');
            end;
          end
          else
            s.add('mov dword ptr [esp+'+inttohex(stackpointer*4,1)+'],'+inttohex(floatdword,8));

          inc(stackpointer);
        end;

        2: //double
        begin
          d:=lua_tonumber(L,-1);
          if processhandler.is64Bit then
          begin
            floatvalues.Add('floatvalue'+inttostr(stackpointer)+':');
            floatvalues.add('dq '+inttohex(doubleqword,16));

            if stackpointer<4 then
              s.add('movsd xmm'+inttostr(stackpointer)+',[floatvalue'+inttostr(stackpointer)+']')
            else
            begin
              s.add('mov rax,[floatvalue'+inttostr(stackpointer)+']');
              s.add('mov qword ptr [rsp+'+inttohex(stackpointer*8,8)+'],rax');
            end;
          end
          else
          begin
            z:=@doubleqword;
            s.add('mov dword ptr [esp+'+inttohex(stackpointer*4,1)+'],'+inttohex(z[0],8));
            inc(stackpointer);
            s.add('mov dword ptr [esp+'+inttohex(stackpointer*4,1)+'],'+inttohex(z[0],8));
          end;
          inc(stackpointer);
        end;

        else
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Invalid parametertype '+inttostr(i+3)+'('+inttostr(valuetype)+')');
          exit(2);
        end;
      end;

      lua_pop(L,1);
    end;

    if processhandler.is64Bit=false then //fix the stackfor the caller
      s[stackalloc]:='sub esp,'+inttohex(stackpointer*4,1);

    s.add('call [addressToCall]');
    if processhandler.is64Bit then
    begin
      s.add('mov [result],rax');
      s.add('add rsp,'+inttohex(align(max(4,paramcount)*8,$10)+8,1));
      s.add('ret');
    end
    else
    begin
      s.add('mov [result],eax');

      if callmethod=1 then
        s.add('add esp,'+inttohex(stackpointer*4,1));

      s.add('ret 4');
    end;

    if floatvalues.count>0 then
      s.add('align 10,0');

    for i:=0 to floatvalues.count-1 do
      s.add(floatvalues[i]);


    dontfree:=false;

    if autoassemble(s,false,true,false,false,disableinfo) then
    begin
      for i:=0 to length(disableinfo.allocs)-1 do
      begin
        if disableinfo.allocs[i].varname='stub' then
          stubaddress:=disableinfo.allocs[i].address;

        if disableinfo.allocs[i].varname='result' then
          resultaddress:=disableinfo.allocs[i].address;
      end;

      {
      lua_pop(L,lua_gettop(L));
      lua_pushstring(L,pchar('stub at '+inttohex(stubaddress,8)));
      print(L);
      dontfree:=true;
      exit(0);
                  }


      thread:=CreateRemoteThread(processhandle, nil, 0, pointer(stubaddress), nil, 0, y);

      if (thread<>0) then
      begin
        dontfree:=timeout=0;


        {$ifdef darwin}
        if macWaitForRemoteThread(thread,timeout) then
          wr:=WAIT_OBJECT_0
        else
          wr:=WAIT_TIMEOUT;
        {$endif}
        {$ifdef windows}
        wr:=WaitForSingleObject(thread, timeout);
        {$endif}
        if wr=WAIT_OBJECT_0 then
        begin
          if ReadProcessMemory(processhandle, pointer(resultaddress), @r, sizeof(r), x) then
          begin
            lua_pushinteger(L, r);
            exit(1);
          end
          else
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Failure reading the result address');
            exit(2);
          end;
        end
        else
        if wr=WAIT_TIMEOUT then
        begin
          dontfree:=true;
          lua_pushnil(L);
          lua_pushstring(L,'Execution timeout');
          exit(2);
        end
        else
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Wait failure');
          exit(2);
        end;


        closehandle(thread);
      end
      else
      begin
        lua_pushnil(L);
        lua_pushstring(L,'Failure launching thread');
        exit(2);
      end;
    end;
  finally
    s.free;
    floatvalues.free;
    disableinfo.free;

    if (dontfree=false) then
    begin
      if stubaddress<>0 then VirtualFreeEx(processhandle, pointer(stubaddress), 0, MEM_RELEASE);
      if resultaddress<>0 then VirtualFreeEx(processhandle, pointer(resultaddress), 0, MEM_RELEASE);
      for i:=0 to length(stringallocs)-1 do
        VirtualFreeEx(processhandle, pointer(stringallocs[i]), 0, MEM_RELEASE);
    end;


    //free allocated strings

  end;

end;

function executeCodeEx(L:PLua_state): integer; cdecl;  //executecodeex(callmethod, timeout, address, {param1},{param2},{param3},{...})
var
  paramcount: integer;
  i: integer;
begin
  //convert to
  //executeMethod(callmethod, timeout, address, nil, param1, param2, param3, ...

  paramcount:=lua_gettop(L);
  if paramcount<3 then
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Not enough parameters. Minimum: callmethod, timeout, address');
    exit(2);
  end;

  lua_pushnil(L);
  lua_insert(L, 4); //instance=nil
  exit(executeMethod(L));
end;

function executeCode(L:PLua_state): integer; cdecl; //executecode(address, parameter)
var
  s: tstringlist;
  //allocs: TCEAllocArray;
  //exceptionlist: TCEExceptionListArray;
  Disableinfo: TDisableinfo;
  address: ptruint;
  i: integer;
  stubaddress: ptruint;
  resultaddress: ptruint;

  parameter: ptruint;
  timeout: dword;

  thread: thandle;

  r: ptruint;
  x: dword;
  y: ptruint;

  wr: DWORD;
  dontfree: boolean;
begin
  //creates a thread in the target process.  calls stdcall function(parameter):pointer and wait for it's return
  stubaddress:=0;
  result:=0;
  dontfree:=false;

  if lua_gettop(L)>=1 then
  begin
    if lua_isnil(L,1) then exit(0);
    address:=lua_toaddress(L,1);

    if lua_gettop(L)>=2 then
    begin
      if lua_isnumber(L,2) then
        parameter:=lua_tointeger(L, 2)
      else
        parameter:=symhandler.getAddressFromName(Lua_ToString(L,2), waitforsymbols);
    end
    else
      parameter:=0;

    if lua_gettop(L)>=3 then
      timeout:=lua_tointeger(L,3)
    else
      timeout:=INFINITE;
  end
  else
    exit;

  s:=tstringlist.create;
  disableinfo:=TDisableinfo.create;
  try
    s.Add('allocXO(stub, 2048)');

    if processhandler.is64Bit then
    begin
      s.add('allocXO(addressToCall, 8)');
      s.add('allocNX(result,8)');
    end
    else
      s.add('allocNX(result,4)');


    s.add('stub:');
    if processhandler.is64Bit then
    begin
      s.add('sub rsp,28');
      s.add('call [addressToCall]');
      s.add('mov [result],rax');
      s.add('add rsp,28');
      s.add('ret');
    end
    else
    begin
      s.add('push [esp+4]');  //push the parameter again
      s.add('call '+inttohex(address,8));
      s.add('mov [result],eax');
      s.add('ret 4');
    end;

    if processhandler.is64Bit then
    begin
      s.add('addressToCall:');
      s.add('dq '+inttohex(address,8));
    end;

    if autoassemble(s, false, true, false, false, disableinfo) then
    begin

      for i:=0 to length(disableinfo.allocs)-1 do
      begin
        if disableinfo.allocs[i].varname='stub' then
          stubaddress:=disableinfo.allocs[i].address;

        if disableinfo.allocs[i].varname='result' then
          resultaddress:=disableinfo.allocs[i].address;
      end;

      if stubaddress<>0 then
      begin
        thread:=CreateRemoteThread(processhandle, nil, 0, pointer(stubaddress), pointer(parameter), 0, x);

        if (thread<>0) then
        begin
          {$ifdef darwin}
          if macWaitForRemoteThread(thread,timeout) then
            wr:=WAIT_OBJECT_0
          else
            wr:=WAIT_TIMEOUT;
          {$endif}
          {$ifdef windows}
          wr:=WaitForSingleObject(thread, timeout);
          {$endif}
          if wr=WAIT_OBJECT_0 then
          begin
            if ReadProcessMemory(processhandle, pointer(resultaddress), @r, sizeof(r), y) then
            begin
              lua_pushinteger(L, r);
              result:=1;
            end
            else
            begin
              lua_pushnil(L);
              lua_pushstring(L,'Failure reading the result address');
              exit(2);
            end;
          end
          else
          if wr=WAIT_TIMEOUT then
          begin
            dontfree:=true;
            lua_pushnil(L);
            lua_pushstring(L,'Execution timeout');
            exit(2);
          end
          else
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Wait failure');
            exit(2);
          end;


          closehandle(thread);
        end
        else
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Failure launching thread');
          exit(2);
        end;
      end;
    end;

  finally
    s.free;
    disableinfo.free;

    if (dontfree=false) and (stubaddress<>0) then
      VirtualFreeEx(processhandle, pointer(stubaddress), 0, MEM_RELEASE);

    if (dontfree=false) and (resultaddress<>0) then
      VirtualFreeEx(processhandle, pointer(resultaddress), 0, MEM_RELEASE);
  end;
end;


function test(x: integer): qword;
begin
  result:=x+12;
end;



function executeCodeLocalEx(L:PLua_state): integer; cdecl; //address,{param},{param},{param}
//paramtypes:
//0: integer/pointer
//1: float
//2: double
//3: asciistring (turns into 0:pointer after writing the string)
//4: widestring

label
 p1typeisint, p1typeisfloat, afterp1,
 p2typeisint, p2typeisfloat, afterp2,
 p3typeisint, p3typeisfloat, afterp3,
 p4typeisint, p4typeisfloat, afterp4;
var
  //callstack: pointer;
  oldstack: pointer;
  callstack: array of byte; //stoprage to hold the parameters pushed on the stack (gets copied when needed)
  p1type: byte;
  p2type: byte;
  p3type: byte;
  p4type: byte;

  valuetype: integer;

  AddressToCall: pointer;
  paramcount: integer;
  paramsize: qword;

  paramstart,paramstart2: pointer;

  r: qword;
  i: integer;

  s: string;
  ws: widestring;

  ts: array of string;
  tws: array of widestring;




begin
  {$ifndef darwinarm64}
  {$ifdef cpu64}
  //allocate a stack for this call and fill in the parameters
  //setup the parameters at callstack $fff0-parametersize
  //getmem(callstack, 65536);

  setlength(ts,0);
  setlength(tws,0);

  if lua_gettop(L)>=1 then
  begin
    AddressToCall:=pointer(lua_toaddress(L,1,true));
    paramcount:=lua_gettop(L)-1;


    paramsize:=max(32,8*paramcount);
    setlength(callstack,paramsize);
    paramstart:=@callstack[0];

    if paramcount>0 then
    begin
      setlength(ts,paramcount);
      setlength(tws,paramcount);

      for i:=2 to 2+paramcount-1 do  //
      begin
        if lua_istable(L,i) then
        begin
          lua_pushstring(L,'type');
          lua_gettable(L,i);
          if lua_isnil(L,-1) then
          begin
            lua_pop(L,1);
            lua_pushinteger(L,1);
            lua_gettable(L,i);
            if lua_isnil(L,-1) then
            begin
              lua_pushnil(L);
              lua_pushstring(L,'Invalid parametertype '+inttostr(i));
              exit(2);
            end;
          end;
          valuetype:=lua_tointeger(L,-1);
          lua_pop(L,1);

          lua_pushstring(L,'value');
          lua_gettable(L,i);
          if lua_isnil(L,-1) then
          begin
            lua_pop(L,1);
            lua_pushinteger(L,2);
            lua_gettable(L,i);
            if lua_isnil(L,-1) then
            begin
              lua_pushnil(L);
              lua_pushstring(L,'Invalid parametervalue '+inttostr(i));
              exit(2);
            end;
          end;
        end
        else
        begin
          //no typedefinition
          if lua_type(L,i)=LUA_TSTRING then
            valuetype:=3
          else
          begin
            if lua_isnumber(L,i) then
            begin
              if lua_isinteger(L,i) then
                valuetype:=0 //integer/pointer
              else
                valuetype:=1; //float (if you want double then give a proper typespecficiation and FU)
            end
            else
            begin
              lua_pushnil(L);
              lua_pushstring(L,'No idea how to handle the type you provided for parameter '+inttostr(i));
              exit(2);
            end;
          end;
          lua_pushvalue(L,i);  //-1 now contains the value

        end;

        case valuetype of
          0: pqword(ptruint(paramstart)+(i-2)*sizeof(pointer))^:=lua_tointeger(L,-1);
          1: psingle(ptruint(paramstart)+(i-2)*sizeof(pointer))^:=lua_tonumber(L,-1);
          2: pdouble(ptruint(paramstart)+(i-2)*sizeof(pointer))^:=lua_tonumber(L,-1);
          3:
          begin
            ts[i-2]:=Lua_ToString(L,-1);
            pqword(ptruint(paramstart)+(i-2)*sizeof(pointer))^:=ptruint(pchar(ts[i-2]));
            valuetype:=0;
          end;

          4:
          begin
            tws[i-2]:=Lua_ToString(L,-1);
            pqword(ptruint(paramstart)+(i-2)*sizeof(pointer))^:=ptruint(pwidechar(tws[i-2]));
            valuetype:=0;
          end;
        end;

        case i-1 of
          1: p1type:=valuetype;
          2: p2type:=valuetype;
          3: p3type:=valuetype;
          4: p4type:=valuetype;
        end;

        lua_pop(L,1);

      end;


      asm
        //parameters are accessed by use of RBP which is unaffected by this code
        mov oldstack,rsp

        sub rsp,paramsize
        and rsp,$fffffffffffffff0   //align just in case it was an unaligned paramcount

        mov paramstart2,rsp

        push rsi
        push rdi
        push rcx
        mov rsi,paramstart
        mov rdi,paramstart2
        mov rcx,paramsize
        rep movsb

        pop rcx
        pop rdi
        pop rsi

        cmp paramcount,1
        jb afterp4
//p1:
        cmp p1type,0
        je p1typeisint

        cmp p1type,1
        je p1typeisfloat

        movsd xmm0,[rsp]
        jmp afterp1

p1typeisint:
        mov rcx,[rsp]
        jmp afterp1

p1typeisfloat:
        movss xmm0,[rsp]

afterp1:
//p2:
        cmp paramcount,2
        jb afterp4

        cmp [p2type],0
        je p2typeisint

        cmp [p2type],1
        je p2typeisfloat

        movsd xmm1,[rsp+8]
        jmp afterp2

p2typeisint:
        mov rdx,[rsp+8]
        jmp afterp2

p2typeisfloat:
        movss xmm1,[rsp+8]

afterp2:

//p3
        cmp paramcount,3
        jb afterp4

        cmp [p3type],0
        je p3typeisint

        cmp [p3type],1
        je p3typeisfloat

        movsd xmm2,[rsp+$10]
        jmp afterp3

p3typeisint:
        mov r8,[rsp+$10]
        jmp afterp3

p3typeisfloat:
        movss xmm2,[rsp+$10]

afterp3:

//p4
        cmp paramcount,4
        jb afterp4

        cmp [p4type],0
        je p4typeisint

        cmp [p4type],1
        je p4typeisfloat

        movsd xmm3,[rsp+$18]
        jmp afterp4

p4typeisint:
        mov r9,[rsp+$18]
        jmp afterp4

p4typeisfloat:
        movss xmm3,[rsp+$18]

afterp4:


        call AddressToCall

        mov r,rax
        mov rsp,oldstack
      end;

      lua_pushinteger(L,r);
      exit(1);
    end;
  end
  else exit(0);




  {$else}
  lua_pushstring(L,'executeCodeLocalEx is currently not supported on the 32-bit build');
  lua_error(L);
  {$endif}
  {$else}
  lua_pushstring(L,'executeCodeLocalEx is currently not supported on the aarch64 build');
  lua_error(L);
  {$endif}

end;


function executeCodeLocal(L:PLua_state): integer; cdecl;
type
  TFunction=function(parameter: pointer):pointer; stdcall;
var
  address, parameter: PtrUInt;
  f: TFunction;
begin
  //executes the given address
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    address:=lua_toaddress(L,1,true);


    if lua_gettop(L)>=2 then
    begin
      if lua_isstring(L,2) then
        parameter:=selfsymhandler.getAddressFromName(Lua_ToString(L,2), waitforsymbols)
      else
        parameter:=lua_tointeger(L, 2)

    end
    else
      parameter:=0;

    f:=pointer(address);
    lua_pushinteger(L, ptruint(f(pointer(parameter))));
    result:=1;
  end
  else
    exit;
end;

function md5file(L:PLua_state): integer; cdecl;
var
  filename: string;
  f: TMemoryStream;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    filename:=Lua_ToString(L,1);

    f:=TMemoryStream.create;
    try
      f.LoadFromFile(filename);
      lua_pushstring(L, MD5Print(MD5Buffer(f.Memory^, f.Size)));
      result:=1;
    finally
      f.free;
    end;
  end;
end;

function md5memory(L:PLua_state): integer; cdecl;
var
  startaddress: ptruint;
  size: integer;

  buf: PByteArray;
  x: ptruint;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    startaddress:=lua_toaddress(L,1);

    size:=lua_tointeger(L, 2);

    if size>0 then
    begin
      getmem(buf, size);
      if ReadProcessMemory(processhandle, pointer(startaddress), buf, size, x) then
      begin
        if x>0 then
        begin
          lua_pushstring(L, MD5Print(MD5Buffer(buf^, x)));
          result:=1;
        end;
      end;
      FreeMemAndNil(buf);
    end;

  end;

end;


function allocateKernelMemory(L:PLua_state): integer; cdecl;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)=1 then
  begin
    lua_pushinteger(L, ptruint(KernelAlloc(lua_tointeger(L,1))));
    result:=1;
  end;
  {$ENDIF}
end;

function freeKernelMemory(L:PLua_state): integer; cdecl;
begin
  {$IFDEF windows}
  if lua_gettop(L)=1 then
    KernelFree(lua_tointeger(L,1));
  {$ENDIF}

  result:=0; //you'll know it worked by not having a BSOD
end;

function lua_mapMemory(L:PLua_state): integer; cdecl;
var
  address: uint64;
  size: dword;
  frompid, topid: dword;

  mmr: TMapMemoryResult;
begin
  result:=0;
  {$IFDEF windows}
  frompid:=0;
  topid:=0;

  if lua_gettop(L)>=2 then
  begin
    address:=lua_toaddress(L,1);
    size:=lua_tointeger(L,2);
  end
  else
    exit(0);

  if lua_gettop(L)>=3 then
    frompid:=lua_tointeger(L,3);

  if lua_gettop(L)>=4 then
    topid:=lua_tointeger(L,4);

  mmr:=MapMemory(address, size, frompid, topid);

  lua_pushinteger(L, mmr.address);
  lua_pushinteger(L, mmr.mdladdress);
  result:=2;
  {$ENDIF}
end;

function lua_unmapMemory(L:PLua_state): integer; cdecl;
var
  mmr: TMapMemoryResult;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(l)>=2 then
  begin
    mmr.address:=lua_toaddress(L, 1);
    mmr.mdladdress:=lua_toaddress(L, 2);
    UnmapMemory(mmr);
  end;
  {$ENDIF}
end;

function lua_lockMemory(L:PLua_state): integer; cdecl;
var
  address: qword;
  size: integer;
  mdl: qword;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=2 then
  begin
    address:=lua_tointeger(L,1);
    size:=lua_tointeger(L,2);

    mdl:=DBK32functions.LockMemory(processid, address, size);
    lua_pushinteger(L,mdl);
    result:=1;
  end;
  {$ENDIF}
end;

function lua_unlockMemory(L:PLua_state): integer; cdecl;
var
  mdl: qword;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=1 then
  begin
    mdl:=lua_tointeger(L,1);
    DBK32functions.unLockMemory(mdl);
  end;
  {$ENDIF}
end;

function lua_sendMessage(L:PLua_state): integer; cdecl;
var h: HWND;
    Msg:  UINT;
    wp: WPARAM;
    lp: LPARAM;
begin
  result:=0;
  if lua_gettop(L)=4 then
  begin
    h:=lua_tointeger(L,1);
    msg:=lua_tointeger(L,2);
    wp:=lua_tointeger(L,3);
    lp:=lua_tointeger(L,4);

    lua_pushinteger(L, SendMessage(h, Msg, wp, lp));
    result:=1;
  end;
end;

function lua_sendMessageTimeout(L:PLua_state): integer; cdecl;
var h: HWND;
    Msg:  UINT;
    wp: WPARAM;
    lp: LPARAM;
    flags, timeout: uint;
    r: DWORD_PTR;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)=4 then
  begin
    h:=lua_tointeger(L,1);
    msg:=lua_tointeger(L,2);
    wp:=lua_tointeger(L,3);
    lp:=lua_tointeger(L,4);
    flags:=lua_tointeger(L,5);
    timeout:=lua_tointeger(L,6);

    if SendMessageTimeoutA(h, Msg, wp, lp, flags,timeout, r)<>0 then
      lua_pushinteger(L,r)
    else
      lua_pushnil(L);

    result:=1;
  end;
  {$ENDIF}
end;

function lua_findWindow(L:PLua_state): integer; cdecl;
var
  classname, windowname: pchar;
begin
  result:=0;
  {$IFDEF windows}
  classname:=nil;
  windowname:=nil;

  if lua_gettop(L)>=1 then
    classname:=lua.lua_tostring(L,1);

  if lua_gettop(L)>=2 then
    windowname:=lua.Lua_ToString(L, 2);

  lua_pushinteger(L, FindWindow(classname, windowname));
  result:=1;
  {$ENDIF}
end;

function lua_getWindow(L:PLua_state): integer; cdecl;
var
  h: hwnd;
  cmd: uint;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=2 then
  begin
    h:=lua_tointeger(L, 1);
    cmd:=lua_tointeger(L, 2);

    lua_pushinteger(L, GetWindow(h, cmd));
    result:=1;
  end;
  {$ENDIF}
end;

function lua_getWindowProcessID(L:PLua_state): integer; cdecl;
var
  h: hwnd;
  pid: DWORD;
  tid: dword;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=1 then
  begin
    pid:=0;
    tid:=0;

    h:=lua_tointeger(L,1);

    tid:=GetWindowThreadProcessId(h, pid);

    lua_pushinteger(L, pid);
    lua_pushinteger(L, tid);
    result:=2;

  end;
  {$ENDIF}
end;

function lua_getWindowCaption(L:PLua_state): integer; cdecl;
var
  h: hwnd;
  s: pchar;
  i: integer;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)=1 then
  begin
    h:=lua_tointeger(L, 1);
    getmem(s,256);
    try
      i:=GetWindowText(h, s, 255);
      s[i]:=#0;
      lua_pushstring(L,s);
      result:=1;
    finally
      FreeMemAndNil(s);
    end;
  end;
  {$ENDIF}
end;

function lua_getWindowClassName(L:PLua_state): integer; cdecl;
var
  h: hwnd;
  s: pchar;
  i: integer;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)=1 then
  begin
    h:=lua_tointeger(L, 1);

    getmem(s,256);
    try
      i:=GetClassNameA(h, s, 255);
      s[i]:=#0;
      lua_pushstring(L,s);
      result:=1;
    finally
      FreeMemAndNil(s);
    end;
  end;
  {$ENDIF}
end;

function lua_getForegroundWindow(L:PLua_state): integer; cdecl;
begin
  result:=1;
  lua_pushinteger(L, GetForegroundWindow());
end;

function getXBox360ControllerKeyPress(L:PLua_state): integer; cdecl;
{$IFDEF windows}
var
  i: integer;
  index: integer;
  ks: XINPUT_KEYSTROKE;
  state: XINPUT_STATE;
{$ENDIF}
begin

  result:=0;

  {$IFDEF windows}
  XInputMessages(false); //you don't want to use gui support for hotkeys, but handle it yourself


  index:=-1;
  if InitXinput=false then exit;
  if not assigned(XInputGetKeystroke) then exit;

  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L, 1);
    if XInputGetKeystroke(index, 0, @ks)<>0 then exit;
  end
  else
  begin
    for i:=0 to XUSER_MAX_COUNT-1 do
    begin
      if (XInputGetState(i, state)=0) and (XInputGetKeystroke(i, 0, @ks)=0) then //found a controller  (usually 0)
      begin
        index:=i;
        break;
      end;
    end;
  end;

  if index<>-1 then
  begin
    lua_newtable(L);
    i:=lua_gettop(L);

    lua_setbasictableentry(L, i, 'VirtualKey', ks.VirtualKey);
    lua_setbasictableentry(L, i, 'Unicode', ks.Unicode);
    lua_setbasictableentry(L, i, 'Flags', ks.Flags);
    lua_setbasictableentry(L, i, 'UserIndex', ks.UserIndex);
    lua_setbasictableentry(L, i, 'HidCode', ks.HidCode);
    result:=1;
  end;
  {$ENDIF}

end;

function getXBox360ControllerState(L:PLua_state): integer; cdecl;
{$IFDEF windows}
var
  index: integer;
  i: integer;
  state: XINPUT_STATE;
{$ENDIF}
begin
  result:=0;
  {$IFDEF windows}

  index:=-1;
  if InitXinput=false then exit;

  if lua_gettop(L)>=1 then
  begin
    index:=lua_tointeger(L, 1);
    if XInputGetState(index, state)<>0 then exit;
  end
  else
  begin
    for i:=0 to XUSER_MAX_COUNT-1 do
    begin
      if XInputGetState(i, state)=0 then //found a controller  (usually 0)
      begin
        index:=i;
        break;
      end;
    end;
  end;

  if index<>-1 then
  begin
    //state is now filled in, convert it to a readable table
    lua_newtable(L);
    i:=lua_gettop(L);

    lua_setbasictableentry(L, i, 'ControllerID', index);
    lua_setbasictableentry(L, i, 'PacketNumber', state.dwPacketNumber);
    lua_setbasictableentry(L, i, 'GAMEPAD_DPAD_UP', (state.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_UP)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_DPAD_DOWN', (state.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_DOWN)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_DPAD_LEFT', (state.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_LEFT)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_DPAD_RIGHT', (state.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_RIGHT)<>0);

    lua_setbasictableentry(L, i, 'GAMEPAD_START', (state.Gamepad.wButtons and XINPUT_GAMEPAD_START)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_BACK', (state.Gamepad.wButtons and XINPUT_GAMEPAD_BACK)<>0);

    lua_setbasictableentry(L, i, 'GAMEPAD_LEFT_THUMB', (state.Gamepad.wButtons and XINPUT_GAMEPAD_LEFT_THUMB)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_RIGHT_THUMB', (state.Gamepad.wButtons and XINPUT_GAMEPAD_RIGHT_THUMB)<>0);

    lua_setbasictableentry(L, i, 'GAMEPAD_LEFT_SHOULDER', (state.Gamepad.wButtons and XINPUT_GAMEPAD_LEFT_SHOULDER)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_RIGHT_SHOULDER', (state.Gamepad.wButtons and XINPUT_GAMEPAD_RIGHT_SHOULDER)<>0);

    lua_setbasictableentry(L, i, 'GAMEPAD_A', (state.Gamepad.wButtons and XINPUT_GAMEPAD_A)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_B', (state.Gamepad.wButtons and XINPUT_GAMEPAD_B)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_X', (state.Gamepad.wButtons and XINPUT_GAMEPAD_X)<>0);
    lua_setbasictableentry(L, i, 'GAMEPAD_Y', (state.Gamepad.wButtons and XINPUT_GAMEPAD_Y)<>0);

    lua_setbasictableentry(L, i, 'wButtons', state.Gamepad.wButtons);

    lua_setbasictableentry(L, i, 'LeftTrigger', state.Gamepad.bLeftTrigger);
    lua_setbasictableentry(L, i, 'RightTrigger', state.Gamepad.bRightTrigger);

    lua_setbasictableentry(L, i, 'ThumbLeftX', state.Gamepad.sThumbLX);
    lua_setbasictableentry(L, i, 'ThumbLeftY', state.Gamepad.sThumbLY);
    lua_setbasictableentry(L, i, 'ThumbRightX', state.Gamepad.sThumbRX);
    lua_setbasictableentry(L, i, 'ThumbRightY', state.Gamepad.sThumbRY);



    result:=1;
  end;
  {$ENDIF}
end;

function setXBox360ControllerVibration(L:PLua_state): integer; cdecl;
{$IFDEF windows}
var
  index: integer;
  left, right: word;
  vib: XINPUT_VIBRATION;
{$ENDIF}
begin
  result:=0;
  {$IFDEF windows}
  if InitXinput=false then exit;

  if lua_gettop(L)>=3 then
  begin
    index:=lua_tointeger(L,1);
    left:=lua_tointeger(L,2);
    right:=lua_tointeger(L, 3);

    vib.wLeftMotorSpeed:=left;
    vib.wRightMotorSpeed:=right;

    result:=1;
    lua_pushboolean(L, XInputSetState(index, @vib)=0);
  end;
  {$ENDIF}
end;

function lua_AddSnapshotAsComment(L:PLua_state): integer; cdecl;
var
  script: TStrings;
  address: ptruint;
  radius: integer;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    script:=lua_ToCEUserData(L,1);
    address:=lua_tointeger(L,2);
    if lua_gettop(L)>=3 then
      radius:=lua_tointeger(L,3)
    else
      radius:=10;

    AddSnapshotAsComment(script, address, radius);
    result:=1;
  end;
end;

function lua_getNextAllocNumber(L:PLua_state): integer; cdecl;
var script: TStrings;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    script:=lua_ToCEUserData(L,1);
    lua_pushinteger(L, GetNextAllocNumber(script));
    result:=1;
  end;
end;

function lua_getUniqueAOB(L:PLua_state): integer; cdecl;
var
  address: ptruint;
  mi: TModuleInfo;
  offset: integer;
  r: string;
  codesize: integer;

  ca: ptruint;
  x: string;

  d: TDisassembler;

begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    address:=lua_tointeger(L,1);
    if lua_gettop(L)>=2 then
      codesize:=lua_tointeger(L,2)
    else
    begin
      //no codesize given, calculate the number of bytes needed to put a 5 byte jmp in here. (make sure to use 3th alloc param, bitch please if you don't)
      codesize:=0;
      ca:=address;

      d:=TDisassembler.create;

      while (ca-address)<5 do
        d.disassemble(ca,x);

      codesize:=ca-address;

      d.free;
    end;

    if address<>0 then
    begin
      mi.baseaddress:=0;
      symhandler.getmodulebyaddress(address,mi);

      r:=GetUniqueAOB(mi,address,codesize,offset);

      lua_pushstring(L,r);
      lua_pushinteger(L,offset);
      result:=2;
    end;
  end;

end;

function lua_registerAutoAssemblerTemplate(L:PLua_state): integer; cdecl;
var
  f: integer;
  routine: string;
  lc: tluacaller;
  name: string;
  shortcut: TShortCut;
begin
  result:=0;

  if lua_gettop(L)>=2 then
  begin
    name:=Lua_ToString(L, 1);

    if lua_isfunction(L, 2) then
    begin
      lua_pushvalue(L, 2);
      f:=luaL_ref(L,LUA_REGISTRYINDEX);

      lc:=TLuaCaller.create;
      lc.luaroutineIndex:=f;
    end
    else
    if lua_isstring(L,2) then
    begin
      routine:=lua_tostring(L,2);
      lc:=TLuaCaller.create;
      lc.luaroutine:=routine;
    end
    else exit;

    shortcut:=0;
    if lua_gettop(L)>=3 then
    begin
      if lua_isinteger(L,3) then
        shortcut:=lua_tointeger(L,3)
      else
      if lua_isstring(L,3) then
        shortcut:=textToShortCut(Lua_ToString(L,3))
    end;

    lua_pushinteger(L, registerAutoAssemblerTemplate(name, lc.AutoAssemblerTemplateCallback, shortcut));
    result:=1;
  end;
end;

function lua_unregisterAutoAssemblerTemplate(L:PLua_state): integer; cdecl;
var id: integer;
begin
  result:=0;
  if lua_gettop(L)=1 then
  begin
    id:=lua_tointeger(L, 1);
    unregisterAutoAssemblerTemplate(id);
  end;
end;


function lua_GenerateCodeInjectionScript(L: PLua_state): integer; cdecl;
var
  script: TStrings;
  address: string;

  farjmp: boolean;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    script:=lua_ToCEUserData(L,1);

    if lua_gettop(L)>=2 then
      address:=Lua_ToString(L,2)
    else
      address:=inttohex(MemoryBrowser.disassemblerview.SelectedAddress,8);

    if lua_gettop(L)>=3 then
      farjmp:=lua_toboolean(L,3)
    else
      farjmp:=false;

    try
      GenerateCodeInjectionScript(script, address,farjmp);
      lua_pushboolean(L,true);
      result:=1;
    except
    end;
  end;
end;

function lua_GenerateAOBInjectionScript(L: PLua_state): integer; cdecl;
var
  script: TStrings;
  address, symbolname: string;
  lineCountToCopy: integer;
  farjmp: boolean;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    script:=lua_ToCEUserData(L,1);
    symbolname:=Lua_ToString(L,2);

    if lua_gettop(L)>=3 then
      address:=Lua_ToString(L,3)
    else
      address:=inttohex(MemoryBrowser.disassemblerview.SelectedAddress,8);

    if lua_gettop(L)>=4 then
      lineCountToCopy:=lua_tointeger(L,4)
    else
      linecountToCopy:=20;

    if lua_gettop(L)>=5 then
      farjmp:=lua_toboolean(L,5)
    else
      farjmp:=false;

    try
      GenerateAOBInjectionScript(script, address, symbolname, lineCountToCopy);
      lua_pushboolean(L,true);
      result:=1;
    except
    end;
  end;
end;

function lua_GenerateFullInjectionScript(L: PLua_state): integer; cdecl;
var
  script: TStrings;
  address: string;
  lineCountToCopy: integer;
  farjmp: boolean;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    script:=lua_ToCEUserData(L,1);

    if lua_gettop(L)>=2 then
      address:=Lua_ToString(L,2)
    else
      address:=inttohex(MemoryBrowser.disassemblerview.SelectedAddress,8);

    if lua_gettop(L)>=3 then
      lineCountToCopy:=lua_tointeger(L,3)
    else
      linecountToCopy:=20;

    if lua_gettop(L)>=4 then
      farjmp:=lua_toboolean(L,4)
    else
      farjmp:=false;

    try
      GenerateFullInjectionScript(script, address, lineCountToCopy);
      lua_pushboolean(L,true);
      result:=1;
    except
    end;
  end;
end;

function lua_unloadLoadedFont(L: PLua_state): integer; cdecl;
begin
  {$IFDEF windows}
  if lua_isnumber(L, 1) then
    RemoveFontMemResourceEx(lua_tointeger(L,1));
  {$ENDIF}

  result:=0;
end;

function lua_loadFontFromStream(L: PLua_state): integer; cdecl;
var
  s: TStream;
  ms: TMemoryStream absolute s;
  f: TFont;
  pc: dword;

  h: THandle;
begin
  result:=0;

  {$IFDEF windows}
  if lua_isuserdata(L, 1) then
  begin
    s:=lua_toceuserdata(L, 1);
    if s is TMemoryStream then
    begin
      pc:=1;
      h:=AddFontMemResourceEx(ms.Memory, ms.Size, nil, @pc);
      lua_pushinteger(L, h);
      result:=1;
    end;
  end;
  {$ENDIF}
end;

function lua_speakex(engLang: boolean; L: Plua_State): integer; cdecl;
var
  pc: integer;
  s: widestring;
begin
  result:=0;
  {$IFDEF windows}
  pc:=lua_gettop(L);

  if pc>=1 then
    s:=Lua_ToString(L, 1)
  else
    exit(0);


  if engLang then
  begin
    s:=StringReplace(s,'&','&amp;',[rfReplaceAll]);
    s:=StringReplace(s,'<','&lt;',[rfReplaceAll]);
    s:=StringReplace(s,'>','&gt;',[rfReplaceAll]);
    //s:='<speak version="1.0" xml:lang="en">'+s+'</speak>';
    //s:='<lang langid="409">'+s+'</lang>';
    s:='<voice required="Language=409">'+s+'</voice>';   //413=dutch (while running under a debugger, this is slow. But normal usage is fine)
  end;

  if pc>=2 then
  begin
    if lua_isboolean(l,2) then
    begin
      lua_pushinteger(L, speak(s, lua_toboolean(L,2)));
      exit(1);
    end
    else
    begin
      lua_pushinteger(L, speak(s, lua_tointeger(L,2)));
      exit(1);
    end;
  end
  else
  begin
    lua_pushinteger(L, speak(s));
    exit(1);
  end;
  {$ENDIF}

end;


function lua_speak(L: Plua_State): integer; cdecl;
begin
  lua_speakEx(false,L);
  result:=0;
end;

function lua_speakEnglish(L: Plua_State): integer; cdecl;
begin
  lua_speakEx(true,L);
  result:=0;
end;


function lua_getFileVersion(L: Plua_State): integer; cdecl;
{$IFDEF windows}
var
  filepath: string;
  h: THandle;
  size: integer;
  data: pointer;
  ffi: ^VS_FIXEDFILEINFO;
  t: integer;

  v: qword;
  s: UINT;
{$ENDIF}
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)=1 then
  begin
    filepath:=Lua_ToString(l,1);
    size:=GetFileVersionInfoSize(pchar(filepath), @h);
    if size<>0 then
    begin
      getmem(data, size);

      if GetFileVersionInfo(pchar(filepath), h, size, data) then
      begin
        s:=sizeof(ffi);
        if VerQueryValue(data, pchar('\'), @ffi, @s) then
        begin
          v:=(qword(ffi^.dwFileVersionMS) shl 32) or ffi^.dwFileVersionLS;
          result:=2;
          lua_pushinteger(L, v);
          lua_newtable(L);
          t:=lua_gettop(L);


          lua_setbasictableentry(L, t, 'major', (v shr 48) and $ffff);
          lua_setbasictableentry(L, t, 'minor', (v shr 32) and $ffff);
          lua_setbasictableentry(L, t, 'release', (v shr 16) and $ffff);
          lua_setbasictableentry(L, t, 'build', v and $ffff);
        end;

      end;
      FreeMemAndNil(data);
    end;
  end;
  {$ENDIF}
end;

function lua_getCheatEngineFileVersion(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L,application.ExeName);
  exit(lua_getFileVersion(L));
end;


function lua_hookWndProc(L: Plua_State): integer; cdecl;
{$IFDEF windows}
var
  s: tstringlist;
  hWnd: THandle;
  f: integer;

  orig: qword;
  ts: string;

  wndhooklist_table, hwnd_table: integer;
  i: integer;
  b: boolean;

  async: boolean;

  pc: TLuaPipeClient;
{$ENDIF}
begin
  result:=0;
  {$IFDEF windows}
  lua_getglobal(L, 'CEWindowProcEvent_Internal');


  if lua_isnil(L, -1) then
  begin
    lua_pop(L, 1);

    s:=TStringList.Create;
    s.add('wndhooklist={}');
    s.add('function CEWindowProcEvent_Internal(hWnd, Msg, lParam, wParam)');
    s.add('  if (wndhooklist[hWnd].f) then');
    s.add('    local r, hWnd2, Msg2, lParam2, wParam2');
    s.add('    r,hWnd2,Msg2,lParam2,wParam2=wndhooklist[hWnd].f(hWnd, Msg, lParam, wParam)');
    s.add('    if (r==0) or (r==1) then return r,(hWnd2 or hWnd),(Msg2 or Msg),(lParam2 or lParam),(wParam2 or wParam) end');
    s.add('  end');
    s.add('  return wndhooklist[hWnd].orig,(hWnd2 or hWnd),(Msg2 or Msg),(lParam2 or lParam),(wParam2 or wParam)');
    s.add('end');

    LUA_DoScript(s.Text);
    s.free;
  end
  else
    lua_pop(L, 1);

  //setup the server if needed
  ts:='CEWINHOOK'+inttostr(processid);
  if luaserverExists(ts)=false then
  begin
    tluaserver.create(ts);

    if processhandler.is64Bit then
      ts:='x86_64'
    else
      ts:='i386';

    try
      cefuncproc.InjectDll(CheatEngineDir+'winhook-'+ts+'.dll');
    except
    end;
  end;


  //get the windowhandle and function parameter
  if (lua_gettop(L)>=2) and lua_isnumber(L, 1) and lua_isfunction(L, 2) then
  begin
    if lua_gettop(L)>=3 then
      async:=lua_toboolean(L, 3)
    else
      async:=false;

    hWnd:=lua_tointeger(L, 1);
    pc:=TLuaPipeClient.create('CEWINHOOKC'+inttostr(processid));
    try
      pc.writeByte(1);
      pc.writeQword(hWnd);
      orig:=pc.readQword;

      lua_getglobal(L, 'wndhooklist');
      wndhooklist_table:=lua_gettop(L);

      lua_pushinteger(L, hWnd);
      lua_newtable(L);

      //fill the hWnd table entry
      hwnd_table:=lua_gettop(L);
      lua_pushstring(L, 'f');
      lua_pushvalue(L, 2);
      lua_settable(L, hwnd_table);

      lua_pushstring(L, 'orig');
      lua_pushinteger(L, orig);
      lua_settable(L, hwnd_table);

      lua_settable(L, wndhooklist_table);


      pc.writeByte(4); //set async state
      pc.writeByte(ifthen(async,1,0));
      pc.readByte;

      pc.writeByte(2); //hook
      pc.writeQword(hWnd);
      if pc.readByte=1 then
      begin
        lua_pushboolean(L, true);
        result:=1;
      end;

    finally
      pc.free;
    end;
  end;

  //get the old proc event of the window
  {$ENDIF}

end;

function lua_unhookWndProc(L: Plua_State): integer; cdecl;
{$IFDEF windows}
var pc: TLuaPipeClient;
{$ENDIF}
begin
  result:=0;
  {$IFDEF windows}
  if (lua_gettop(L)=1) and lua_isnumber(L, 1) then
  begin
    lua_getglobal(L, 'wndhooklist');

    lua_pushinteger(L, lua_tointeger(L, 1));
    lua_gettable(L,-2);
    if lua_istable(L, -1) then
    begin
      lua_pushstring(L, 'orig');
      lua_gettable(L, -2);
      if lua_isnumber(L, -1) then
      begin
        pc:=TLuaPipeClient.create('CEWINHOOKC'+inttostr(processid));
        try
          pc.writeByte(3);
          pc.writeQword(lua_tointeger(L, 1));
          pc.writeQword(lua_tointeger(L, -1));
          if pc.readByte=1 then
          begin
            lua_pushboolean(L, true);
            result:=1;
          end;
        finally
          pc.free;
        end;
      end;
    end;
  end;
  {$ENDIF}
end;

function lua_registerEXETrainerFeature(L: Plua_State): integer; cdecl;
var i,j: integer;
begin
  result:=0;
  if (lua_gettop(L)=2) and lua_isstring(L,1) and lua_isfunction(L,2) then
  begin
    j:=length(exeTrainerFeatures);
    for i:=0 to length(exeTrainerFeatures)-1 do
      if exeTrainerFeatures[i].featurename='' then
      begin
        j:=i;
        break;
      end;

    if j=length(exeTrainerFeatures) then
      setlength(exeTrainerFeatures,j+1);

    exeTrainerFeatures[j].featurename:=Lua_ToString(L, 1);
    exeTrainerFeatures[j].functionid:=luaL_ref(L, LUA_REGISTRYINDEX);

    lua_pushinteger(L, j);
    result:=1;
  end;
end;

function lua_unregisterEXETrainerFeature(L: Plua_State): integer; cdecl;
var i: integer;
begin
  if (lua_gettop(L)=1) and lua_isnumber(L, 1) then
  begin
    i:=lua_tointeger(L,1);
    if i<length(exeTrainerFeatures) then
    begin
      exeTrainerFeatures[i].featurename:='';
      luaL_unref(L, LUA_REGISTRYINDEX, exeTrainerFeatures[i].functionid);
      exeTrainerFeatures[i].functionid:=0;
    end;
  end;

  result:=0;
end;

function lwriter(L: Plua_State; const p: Pointer; sz: size_t; ud: Pointer): Integer; cdecl;
var s: TStream;
begin
  s:=tstream(ud);
  s.WriteBuffer(p^,sz);
  result:=0;
end;



function lua_encodeFunction(L: Plua_State): integer; cdecl;
var
  s: TMemoryStream;
  cs: Tcompressionstream;
  i: integer;

  output: pchar;

  new: pchar;
begin
  s:=TMemoryStream.Create;
  cs:=Tcompressionstream.create(clmax, s);


  if (lua_gettop(L)=1) and (lua_isfunction(L, 1)) then
    lua_dump(L, @lwriter, cs, 1);

  cs.free;

  getmem(output, (s.size div 4) * 5 + 5 );
  BinToBase85(pchar(s.Memory), output, s.size);

  lua_pushstring(L, output);
  FreeMemAndNil(output);

  s.free;

  result:=1;
end;

function lua_encodeFunctionEx(L: Plua_State): integer; cdecl;
//takes a string and an optional lua dll and encode it with that dll instead
var
  s: TMemoryStream;
  cs: Tcompressionstream;
  script,luadll: string;
  hm: HModule;

  _luaL_newstate: function : Plua_State; cdecl;
  _luaL_openlibs: procedure(L: Plua_State); cdecl;
  _luaL_loadstring: function(L: Plua_State; const s: PChar): Integer; cdecl;
  _lua_dump: function(L: Plua_State; writer: lua_Writer; data: Pointer; strip: integer): Integer; cdecl;
  _lua_close: procedure(L: Plua_State); cdecl;

  l2: Plua_State;
  r: integer;

  rs:  string;
  output: pchar;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    script:=Lua_ToString(L,1);

    if lua_gettop(L)>=2 then
      luadll:=Lua_ToString(L,2)
    else
      luadll:=LUA_LIB_NAME;

    hm:=LoadLibrary(pchar(luadll));
    if hm<>0 then
    begin
      _luaL_newstate:=getprocaddress(hm,'luaL_newstate');
      _luaL_openlibs:=getprocaddress(hm,'luaL_openlibs');
      _luaL_loadstring:=getprocaddress(hm,'luaL_loadstring');
      _lua_dump:=getprocaddress(hm,'lua_dump');
      _lua_close:=getprocaddress(hm,'lua_close');



      l2:=_luaL_newstate;
      _luaL_openlibs(l2);
      if _luaL_loadstring(l2, pchar(script))=0 then
      begin
        //encode the function.
        s:=TMemoryStream.Create;
        cs:=Tcompressionstream.create(clmax, s);

        _lua_dump(L2, @lwriter, cs, 1);

        cs.free;
        getmem(output, (s.size div 4) * 5 + 5 );
        BinToBase85(pchar(s.Memory), output, s.size);

        lua_pushstring(L, output);
        FreeMemAndNil(output);

        s.free;

        result:=1;

        _lua_close(l2);

        FreeLibrary(hm);
      end;
    end;
  end;
end;


function lreader(L: Plua_State; ud: Pointer; sz: Psize_t): PChar; cdecl;
var s: TMemoryStream;
begin
  s:=TMemoryStream(ud);

  result:=pchar(ptruint(s.Memory)+s.Position);
  sz^:=s.Size-s.position;

  s.position:=s.position+sz^;
end;

function lua_decodeFunction(L: Plua_State): integer; cdecl;
var
  ds: Tdecompressionstream;
  s: TMemoryStream;

  decompressed: TMemoryStream;
  i: integer;

  size: integer;
  input, output: pchar;
begin
  result:=1;
  if (lua_gettop(L)=1) and lua_isstring(L,1) then
  begin
    input:=lua.lua_tostring(L,1);

    size:=(length(input) div 5)*4+(length(input) mod 5);
    getmem(output, size*2);


    size:=Base85ToBin(input, output);
    lua_pushlstring(L, output, size);

    s:=TMemoryStream.Create;
    s.WriteBuffer(output^, size);
    s.position:=0;
    ds:=Tdecompressionstream.create(s);

    decompressed:=TMemoryStream.create;
    decompressed.CopyFrom(ds,0);

    decompressed.position:=0;
    lua_load(L, @lreader, decompressed,'cechunk', 'b');


    freeandnil(decompressed);
    freeandnil(ds);
    freeandnil(s);
    FreeMemAndNil(output);

    result:=1;
  end;

end;

function lua_getFileList(L: Plua_State): integer; cdecl;
var
  list: Tstringlist;
  path: string;
  mask: string;
  subdirs: boolean;
  attrib: word;

  paramcount: integer;
  i: integer;
begin
  paramcount:=lua_gettop(L);
  if paramcount=0 then exit(0);

  path:=lua_tostring(L,1);
  if paramcount>1 then
    mask:=Lua_ToString(L, 2)
  else
    mask:='';

  if paramcount>2 then
    subdirs:=lua_toboolean(L,3)
  else
    subdirs:=false;

  if paramcount>3 then
    attrib:=lua_tointeger(L, 4)
  else
    attrib:=faDirectory;

  lua_pop(L, paramcount);

  list:=FindAllFiles(path,mask,subdirs,attrib);

  lua_createtable(L, list.count, 0);
  result:=1;

  for i:=0 to list.count-1 do
  begin
    lua_pushinteger(L, i+1);
    lua_pushstring(L, list[i]);
    lua_settable(L, 1);
  end;

  list.free;
end;

function lua_getDirectoryList(L: Plua_State): integer; cdecl;
var
  list: Tstringlist;
  path: string;
  mask: string;
  subdirs: boolean;
  attrib: word;

  paramcount: integer;
  i: integer;
begin
  paramcount:=lua_gettop(L);
  if paramcount=0 then exit(0);

  path:=lua_tostring(L,1);

  if paramcount>1 then
    subdirs:=lua_toboolean(L,2)
  else
    subdirs:=false;

  lua_pop(L, paramcount);

  list:=FindAllDirectories(path,subdirs);

  lua_createtable(L, list.count, 0);
  result:=1;

  for i:=0 to list.count-1 do
  begin
    lua_pushinteger(L, i+1);
    lua_pushstring(L, list[i]);
    lua_settable(L, 1);
  end;

  list.free;
end;

function lua_connectToCEServer(L: Plua_State): integer; cdecl;
var
  hostname: string;
  port: integer;
begin
  result:=0;
  if lua_gettop(L)=2 then
  begin
    hostname:=Lua_ToString(L, 1);
    port:=lua_tointeger(L, 2);
    CEconnect(hostname, port);
  end;
end;

function lua_copyMemory(L: Plua_State): integer; cdecl;
var
  sourceAddress: ptruint;
  destinationAddress: ptruint;
  Method: integer;
  size: integer;

  pc: integer;
  temp: Pointer;
  ar: ptruint;
begin
  result:=0;
  pc:=lua_gettop(L);
  if pc=0 then exit;

  sourceAddress:=lua_toaddress(L,1);

  if pc>1 then
    size:=lua_tointeger(L,2);

  if pc>3 then
    Method:=lua_tointeger(L, 4)
  else
    Method:=0;

  if (pc>2) and (not lua_isnil(L,3)) then
    destinationAddress:=lua_toaddress(L, 3)
  else
  begin
    //allocate the memory
    case method of
      0,2:
      begin
        //target process
        destinationAddress:=ptruint(VirtualAllocEx(processhandle, nil,size,MEM_COMMIT or MEM_RESERVE,PAGE_EXECUTE_READWRITE));
        if destinationAddress=0 then exit;
      end;

      1,3:
      begin
        //ce memory
        destinationAddress:=ptruint(VirtualAlloc(nil,size,MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE));
        if destinationAddress=0 then exit;
      end;
    end;
  end;


  case method of
    0: //target to target
    begin
      temp:=VirtualAlloc(nil,size,MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
      try
        if readprocessmemory(processhandle, pointer(sourceAddress), temp, size, ar) then
        begin
          if ar=size then
          begin
            if WriteProcessMemory(processhandle, pointer(destinationAddress), temp, size, ar) then
            begin
              if ar=size then
              begin
                lua_pushinteger(L, destinationAddress);
                exit(1);
              end;
            end;
          end;
        end;

      finally
        VirtualFree(temp,0, MEM_RELEASE);
      end;
    end;

    1: //target to CE
    begin
      if readprocessmemory(processhandle, pointer(sourceAddress), pointer(destinationAddress), size, ar) then
      begin
        if ar=size then
        begin
          lua_pushinteger(L, destinationAddress);
          exit(1);
        end;
      end;
    end;

    2: //CE to target
    begin
      if writeprocessmemory(processhandle, pointer(destinationAddress), pointer(sourceAddress), size, ar) then
      begin
        if ar=size then
        begin
          lua_pushinteger(L, destinationAddress);
          exit(1);
        end;
      end;
    end;

    3: //CE to CE
    begin
      try
        {$ifdef windows}
        RtlCopyMemory(pointer(destinationAddress), pointer(sourceAddress), size);
        {$else}
        copymemory(pointer(destinationAddress), pointer(sourceAddress), size);
        {$endif}
        lua_pushinteger(L, destinationAddress);
        exit(1);
      except
      end;
    end;
  end;
end;


function lua_compareMemory(L: PLua_state): integer; cdecl;
var
  address1,address2: ptruint;
  Method: integer;
  size,i: integer;

  pc: integer;
  temp: Pointer;
  ar: ptruint;

  buf1, buf2: PByteArray;
begin
  result:=0;
  pc:=lua_gettop(L);
  if pc<3 then exit;


  if pc>=4 then
    method:=lua_tointeger(L,4)
  else
    method:=0;

  if method=2 then
    address1:=lua_toaddress(L,1,true)
  else
    address1:=lua_toaddress(L,1);

  if method>0 then
    address2:=lua_toaddress(L,2,true)
  else
    address2:=lua_toaddress(L,2);

  size:=lua_tointeger(L,3);

  if size=0 then exit;

  buf1:=nil;
  buf2:=nil;

  case method of
    0:
    begin
      getmem(buf1,size);
      getmem(buf2,size);
      ReadProcessMemory(processhandle, pointer(address1),buf1,size,ar);
      ReadProcessMemory(processhandle, pointer(address2),buf2,size,ar);
    end;

    1:
    begin
      getmem(buf1,size);
      ReadProcessMemory(processhandle, pointer(address1),buf1,size,ar);
      buf2:=pointer(address2);
    end;

    2:
    begin
      buf1:=pointer(address1);
      buf2:=pointer(address2);
    end;
  end;

  //compare
  result:=1;


  {$ifdef windows}
  i:=RtlCompareMemory(buf1,buf2,size);
  {$else}
  i:=CompareMemRange(buf1,buf2,size);
  {$endif}

  lua_pushboolean(L,i=size);
  if i<>size then
  begin
    lua_pushinteger(L, i);
    result:=2;
  end;


  case method of
    0:
    begin
      if buf1<>nil then
        FreeMemAndNil(buf1);

      if buf2<>nil then
        FreeMemAndNil(buf2);
    end;

    1:
    begin
      if buf1<>nil then
        freeMemAndNil(buf1);
    end;
  end;


end;


function lua_enableDRM(L: Plua_State): integer; cdecl;
var
  PreferedAltitude: word;
  ProtectedProcess: dword; //pid
begin
  result:=0;

  {$IFDEF windows}
  DBK32Initialize;

  if lua_gettop(L)>=1 then
    PreferedAltitude:=lua_tointeger(L,1)
  else
    PreferedAltitude:=0;

  if lua_gettop(L)>=2 then
  begin
    if lua_isstring(L,2) then
      ProtectedProcess:=ce_getProcessIDFromProcessName(pchar(Lua_ToString(L,2)))
    else
      ProtectedProcess:=lua_tointeger(L,2);
  end
  else
    ProtectedProcess:=0;

  result:=1;
  lua_pushboolean(L, dbk_enabledrm(preferedAltitude, GetPEProcess(ProtectedProcess)));
  {$ENDIF}
end;


function lua_saveOpenedFile(L: Plua_State): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
    filehandler.commitchanges(Lua_ToString(L,1))
  else
    Filehandler.commitChanges;

  result:=0;
end;

function lua_getOpenedFileSize(L: Plua_State): integer; cdecl;
begin
  if (filehandler.filedata<>nil) then
  begin
    lua_pushinteger(L, filehandler.filedata.Size);
    result:=1;
  end
  else
    result:=0;
end;

function lua_openFileAsProcess(L: Plua_State): integer; cdecl;
var
  filename: string;
  is64bit: boolean;
  oldprocessname: string;
  oldprocess: dword;
  oldprocesshandle: thandle;
  parameters: integer;
  startaddress: ptruint;

begin
  result:=0;

  result:=1;
  parameters:=lua_gettop(L);
  if parameters>=1 then
  begin
    filename:=Lua_ToString(L,1);

    if parameters>=2 then
      is64bit:=lua_toboolean(L,2)
    else
      is64bit:=false;

    if parameters>=3 then
      startaddress:=lua_tointeger(L,3)
    else
      startaddress:=0;

    DetachIfPossible;
    oldprocessname := copy(mainform.ProcessLabel.Caption, pos('-', mainform.ProcessLabel.Caption) + 1, length(mainform.ProcessLabel.Caption));
    oldprocess := processID;
    oldprocesshandle := processhandle;

    try
      DBKFileAsMemory(filename,startaddress);

    except
      lua_pushboolean(L,false);
      exit;
    end;

    ProcessHandler.ProcessHandle:=THandle(-2);

    MainForm.ProcessLabel.caption:=extractfilename(filename);
    MainForm.miSaveFile.visible:=true;

    ProcessHandler.processid:=$FFFFFFFF;

    ProcessHandler.is64Bit:=is64bit;

    mainform.openProcessEpilogue(oldprocessname, oldprocess, oldprocesshandle,true);

    lua_pushboolean(L,true);
  end;
end;

function lua_getPEB(L: Plua_State): integer; cdecl;
var peb: qword;
  pbi: TProcessBasicInformation;
  x: ulong;
begin
  result:=0;


 {$IFDEF windows}
 if DBKLoaded then
    peb:=dbk_getPEB(GetPEProcess(processid))
  else
  begin
    if NtQueryInformationProcess(processhandle, ProcessBasicInformation, @pbi, sizeof(pbi), @x)=STATUS_SUCCESS then
      peb:=qword(pbi.PebBaseAddress)
    else
      peb:=0;
  end;

  lua_pushinteger(L, peb);
  result:=1;
 {$ENDIF}
end;

function lua_createAPC(L: Plua_State): integer; cdecl;
var address: ptruint;
begin
  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=1 then
  begin
    address:=lua_toaddress(L,1);
    CreateRemoteAPC(getathreadid(processid), pointer(address));
  end;
  {$ENDIF}
end;

function lua_setAssemblerMode(L: Plua_State): integer; cdecl;
var mode: integer;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    mode:=lua_tointeger(L,1);
    if mode=0 then
      processhandler.is64Bit:=false;

    if mode=1 then
      processhandler.is64Bit:=true;
  end;
end;

function lua_allocateMemory(l: Plua_State): integer; cdecl;
var
  size: integer;
  a: pointer;
  base: pointer;
  prot: dword;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    size:=lua_tointeger(L,1);
    if lua_gettop(L)>=2 then
      base:=pointer(lua_tointeger(L,2))
    else
      base:=nil;

    if lua_gettop(L)>=3 then
      prot:=lua_tointeger(L,3)
    else
    begin
      if SystemSupportsWritableExecutableMemory then
        prot:=PAGE_EXECUTE_READWRITE
      else
        prot:=PAGE_READWRITE;
    end;


    a:=VirtualAllocEx(processhandle,base,size,MEM_COMMIT or MEM_RESERVE, prot);
    if a=nil then
    begin
      //try to fix a mistake
      base:=FindFreeBlockForRegion(ptruint(base),size);
      a:=VirtualAllocEx(processhandle,base,size,MEM_COMMIT or MEM_RESERVE, prot);
      if a=nil then exit(0);
    end;

    lua_pushinteger(L, ptruint(a));
    result:=1;
  end;
end;

function lua_try(l: Plua_State): integer; cdecl;
//nice idea, but the return value of this function seems broken after an raise exception in a function called by pcall (lua_error does function properly)
begin
  result:=2;
  if lua_gettop(L)>=1 then
  begin
    if lua_isfunction(L,1) then
    begin

      try
        if lua.lua_pcall(L,lua_gettop(L)-1,LUA_MULTRET,0)=0 then
        begin
          lua_pushboolean(L,true);
          lua_insert(L,1);
          result:=lua_gettop(L);
        end
        else
        begin
          lua_pushboolean(L,false);
          lua_pushvalue(L,-2);
        end;
      except
        on e:exception do
        begin
          lua_pop(L,lua_gettop(L));
          lua_pop(L,1);
          lua_pushboolean(L,false);
          lua_pushstring(L,e.message);
        end;
      end;

    end
    else
    begin
      lua_pushboolean(L,false);
      lua_pushstring(L,'First parameter for try is not a function');
    end;
  end
  else
  begin
    lua_pushboolean(L,false);
    lua_pushstring(L,'No parameter given');
  end;
end;

function lua_getSystemMetrics(l: Plua_State): integer; cdecl;
var parameters, nIndex: integer;
begin
  result:=0;
  parameters:=lua_gettop(L);
  if parameters=1 then
  begin
    nIndex:=lua_tointeger(L,-1);
    lua_pop(L, parameters);
    lua_pushinteger(L, GetSystemMetrics(nIndex));
    result:=1;
  end else lua_pop(L, parameters);
end;


function lua_getScreenDPI(l: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, Screen.PixelsPerInch);
  result:=1;
end;


function lua_getScreenHeight(l: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, screen.Height);
  result:=1;
end;

function lua_getScreenWidth(l: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, screen.Width);
  result:=1;
end;

function lua_getWorkAreaHeight(l: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, screen.WorkAreaHeight);
  result:=1;
end;

function lua_getWorkAreaWidth(l: Plua_State): integer; cdecl;
begin
  lua_pushinteger(L, screen.WorkAreaWidth);
  result:=1;
end;

var screencanvas: TCanvas=nil;
function lua_getScreenCanvas(L: PLua_state): integer; cdecl;
begin
  if screencanvas=nil then
  begin
    screencanvas:=tcanvas.Create;
    screencanvas.Handle:=GetDC(0);
  end;

  luaclass_newClass(L, screencanvas);
  result:=1;
end;

function lua_getHandleList(L: PLua_state): integer; cdecl;
var
  shi: PSYSTEM_HANDLE_INFORMATION=nil;
  rl: ulong;
  r: ntstatus;
  i,j: integer;

  filter: integer;
  peprocess: ptruint=0;
begin
  result:=0;

  {$IFDEF windows}
  i:=sizeof(SYSTEM_HANDLE_TABLE_ENTRY_INFO)+128*sizeof(SYSTEM_HANDLE_TABLE_ENTRY_INFO);
  getmem(shi,i);

  repeat
    zeromemory(shi,i);
    r:=NtQuerySystemInformation(SystemHandleInformation, shi,i,@rl);
    if r=STATUS_INFO_LENGTH_MISMATCH then
    begin
      FreeMemAndNil(shi);
      i:=i*2-2;
      getmem(shi,i);
    end;

  until (r<>STATUS_INFO_LENGTH_MISMATCH) or (i>256*1024*1024);

  if r=STATUS_INFO_LENGTH_MISMATCH then exit(0);
  if r<>0 then exit(0);

  if lua_gettop(L)>=1 then
  begin
    filter:=lua_tointeger(L,1);
    //0=everything
    //1=all handles from the opened process
    //2=all handles to the opened process
    //3=all handles to cheat engine

    case filter of
      2:
      begin
        LoadDBK32;
        peprocess:=GetPEProcess(processid);
      end;

      3:
      begin
        LoadDBK32;
        peprocess:=GetPEProcess(GetCurrentProcessId);
        filter:=2;
      end;
    end;
  end
  else
    filter:=0;

  lua_pop(l, lua_gettop(L));


  lua_createtable(L, shi.HandleCount,0);
  j:=0;
  for i:=0 to shi.HandleCount-1 do
  begin
    if (filter=0) or
       ((filter=1) and (shi.list[i].ProcessId=processid)) or
       ((filter=2) and (ptruint(shi.list[i].obj)=peprocess)) then
    begin
      inc(j);
      lua_pushinteger(L,j);
      lua_createtable(L,0,6);

      lua_pushstring(L, 'ProcessID');
      lua_pushinteger(L, shi.list[i].ProcessId );
      lua_settable(L,-3);

      lua_pushstring(L, 'ObjectTypeIndex');
      lua_pushinteger(L, shi.list[i].ObjectTypeIndex );
      lua_settable(L,-3);

      lua_pushstring(L, 'HandleAttributes');
      lua_pushinteger(L, shi.list[i].HandleAttributes );
      lua_settable(L,-3);

      lua_pushstring(L, 'HandleValue');
      lua_pushinteger(L, shi.list[i].HandleValue );
      lua_settable(L,-3);

      lua_pushstring(L, 'Object');
      lua_pushinteger(L, ptruint(shi.list[i].obj));
      lua_settable(L,-3);

      lua_pushstring(L, 'GrantedAccess');
      lua_pushinteger(L, shi.list[i].GrantedAccess );
      lua_settable(L,-3);

      lua_settable(L,1);

    end;
  end;

  if shi<>nil then
    FreeMemAndNil(shi);

  result:=1;
  {$ENDIF}
end;

function lua_closeRemoteHandle(L: PLua_state): integer; cdecl;
var
  handle, newhandle: THandle;
  ph: thandle;
begin
  result:=0;

  {$IFDEF windows}
  if lua_gettop(L)>=1 then
  begin
    handle:=lua_tointeger(L,1);

    if lua_gettop(L)>=2 then
    begin
      ph:=newkernelhandler.openProcess(PROCESS_DUP_HANDLE,false,lua_tointeger(L,2));
      if ph=0 then exit
    end
    else
      ph:=processhandle;

    DuplicateHandle(ph, handle, 0,nil, 0, false, DUPLICATE_CLOSE_SOURCE);

    if (lua_gettop(L)>=2) then
      closehandle(ph);
  end;
  {$ENDIF}
end;

function lua_showSelectionList(L: PLua_state): integer; cdecl;
var
  title,
  caption: string;
  list: Tstringlist;
  output: string;
  custominput: boolean;
  formname: string;
  r: integer;
begin
  if lua_gettop(L)>=3 then
  begin
    title:=Lua_ToString(L,1);
    caption:=Lua_ToString(L,2);
    list:=lua_ToCEUserData(L,3);


    if lua_gettop(L)>=4 then
      custominput:=lua_toboolean(L,4)
    else
      custominput:=false;

    if lua_gettop(L)>=5 then
      formname:=Lua_ToString(L,5)
    else
      formname:='';


    r:=ShowSelectionList(application,title, caption,list, output, custominput,nil,formname);

    lua_pushinteger(L,r);
    lua_pushstring(L, output);

    result:=2;
  end
  else result:=0;

end;

function lua_cpuid(L: PLua_state): integer; cdecl;
var a,c: dword;
var r: TCPUIDResult;
i: integer;
begin
  if lua_gettop(L)>=1 then
    a:=lua_tointeger(L,1)
  else
    exit(0);


  if lua_gettop(L)>=2 then
    c:=lua_tointeger(L,2)
  else
    c:=0;

  r:=CPUID(a,c);
  lua_newtable(L);
  i:=lua_gettop(L);
  lua_setbasictableentry(L, i, 'EAX', r.eax);
  lua_setbasictableentry(L, i, 'EBX', r.ebx);
  lua_setbasictableentry(L, i, 'ECX', r.ecx);
  lua_setbasictableentry(L, i, 'EDX', r.edx);
  result:=1;
end;

function lua_gc_setPassive(L: PLua_state): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
    mainform.tLuaGCPassive.enabled:=lua_toboolean(L,1);

  result:=0;
end;

function lua_gc_setActive(L: PLua_state): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
    mainform.tLuaGCActive.enabled:=lua_toboolean(L,1);

  if lua_gettop(L)>=2 then
    mainform.tLuaGCActive.interval:=lua_tointeger(L,2);

  if lua_gettop(L)>=3 then
    luagc_MinSize:=lua_tointeger(L,3);

  result:=0;
end;

function lua_getHotkeyHandlerThread(L: PLua_state): integer; cdecl;
begin
  luaclass_newClass(L, hotkeythread);
  result:=1;
end;


function lua_setForceCR3VirtualQueryEx(L: PLua_state): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
    forceCR3VirtualQueryEx:=lua_toboolean(L,1);

  result:=0;
end;

function lua_enumMemoryRegionsCR3(L: PLua_state): integer; cdecl;
var
  mbi: TMEMORYBASICINFORMATION;
  cr3: qword;
  maxaddress: qword;
  address: ptruint;
  oldaddress: ptruint;
  i: integer;
begin
  {$ifdef windows}
  address:=0;

  if lua_gettop(L)>=2 then
    cr3:=lua_tointeger(L,1)
  else
    cr3:=0;

  if lua_gettop(L)>=2 then
    maxAddress:=lua_tointeger(L,2)
  else
    maxaddress:=0;


  lua_newtable(L);
  i:=1;
  while ((maxaddress=0) or (address<maxaddress) ) and (newkernelhandler.VirtualQueryExCR3(cr3, pointer(address),mbi,sizeof(mbi))=sizeof(mbi)) do
  begin
    lua_pushinteger(L,i);
    lua_newtable(L);

    lua_pushstring(L,'BaseAddress');
    lua_pushinteger(L,ptruint(mbi.BaseAddress));
    lua_settable(L,-3);

    lua_pushstring(L,'AllocationBase');
    lua_pushinteger(L,ptruint(mbi.AllocationBase));
    lua_settable(L,-3);

    lua_pushstring(L,'AllocationProtect');
    lua_pushinteger(L,mbi.AllocationProtect);
    lua_settable(L,-3);

    lua_pushstring(L,'RegionSize');
    lua_pushinteger(L,mbi.RegionSize);
    lua_settable(L,-3);

    lua_pushstring(L,'State');
    lua_pushinteger(L,mbi.State);
    lua_settable(L,-3);

    lua_pushstring(L,'Protect');
    lua_pushinteger(L,mbi.Protect);
    lua_settable(L,-3);

    lua_pushstring(L,'Type');
    lua_pushinteger(L,mbi._Type);
    lua_settable(L,-3);

    lua_settable(L,-3);

    oldaddress:=address;
    address:=address+mbi.RegionSize;
    if address<oldaddress then break;

    inc(i);
  end;

  result:=1;
  {$else}
  result:=2;
  lua_pushnil(L);
  lua_pushstring(L,'Not yet implemented');
  {$endif}
end;

function lua_enumMemoryRegions(L: PLua_state): integer; cdecl;
var
  mbi: TMEMORYBASICINFORMATION;
  address: ptruint;
  oldaddress: ptruint;
  i: integer;
  maxAddress: ptruint;
begin
  address:=0;
  if lua_gettop(L)>=1 then
    maxAddress:=lua_tointeger(L,1)
  else
    maxaddress:=0;

  lua_newtable(L);
  i:=1;
  while ((maxaddress=0) or (address<maxaddress) ) and (newkernelhandler.VirtualQueryEx(processhandle, pointer(address),mbi,sizeof(mbi))=sizeof(mbi)) do
  begin
    lua_pushinteger(L,i);
    lua_newtable(L);

    lua_pushstring(L,'BaseAddress');
    lua_pushinteger(L,ptruint(mbi.BaseAddress));
    lua_settable(L,-3);

    lua_pushstring(L,'AllocationBase');
    lua_pushinteger(L,ptruint(mbi.AllocationBase));
    lua_settable(L,-3);

    lua_pushstring(L,'AllocationProtect');
    lua_pushinteger(L,mbi.AllocationProtect);
    lua_settable(L,-3);

    lua_pushstring(L,'RegionSize');
    lua_pushinteger(L,mbi.RegionSize);
    lua_settable(L,-3);

    lua_pushstring(L,'State');
    lua_pushinteger(L,mbi.State);
    lua_settable(L,-3);

    lua_pushstring(L,'Protect');
    lua_pushinteger(L,mbi.Protect);
    lua_settable(L,-3);

    lua_pushstring(L,'Type');
    lua_pushinteger(L,mbi._Type);
    lua_settable(L,-3);

    lua_settable(L,-3);

    oldaddress:=address;
    address:=address+mbi.RegionSize;
    if address<oldaddress then break;

    inc(i);
  end;

  result:=1;
end;




function lua_enableWindowsSymbols(L: PLua_state): integer; cdecl;
begin
  EnableWindowsSymbols(false);
  result:=0;
end;

function lua_enableKernelSymbols(L: PLua_state): integer; cdecl;
begin
  result:=0;
  symhandler.kernelsymbols:=true;
  MemoryBrowser.Kernelmodesymbols1.checked:=true;
  symhandler.reinitialize(true);
end;

function lua_enumExports(L: PLua_state): integer; cdecl;
var
  address: ptruint;
  path: string;
  self: boolean;
  e: boolean;
  list: Tstringlist;
  i: integer;
begin

  result:=0;
  {$IFDEF windows}
  if lua_gettop(L)>=1 then
  begin
    list:=Tstringlist.create;
    try
      if lua_gettop(L)>=2 then
        self:=lua_toboolean(L,2);

      try
        address:=lua_toaddress(L,1,self);
        try
          peinfo_getExportList(address,list);
        except
          exit(0); //no export list
        end;
      except
        if lua_isstring(L,1) then
        begin
          path:=Lua_ToString(L,1);
          try
            peinfo_getExportList(path,list);
          except
            exit(0);
          end;
        end;
      end;

      lua_newtable(L);
      for i:=0 to list.count-1 do
      begin
        lua_pushstring(L,list[i]);
        lua_pushinteger(L,ptruint(list.Objects[i]));
        lua_settable(L,-3);
      end;
      result:=1;


    finally
      list.free;
    end;

  end;
  {$ENDIF}
end;

function lua_duplicateHandle(L: PLua_state): integer; cdecl;
//three formats:
//(handle): Duplicates a CE handle to the target process
//(handle, mode) : where mode is 0: CE to Target, 1: Target to CE
//(handle, frompid,topid)
var
  fromprocess: THandle;
  toprocess: THandle;

  sourcehandle: THandle;
  newhandle: THandle;

  frompid, topid: qword;

  openedfromprocess: boolean=false;
  openedtoprocess: boolean=false;
begin
  result:=0;
  {$IFDEF windows}
  try

    fromprocess:=GetCurrentProcess;
    toprocess:=processhandle;
    if lua_gettop(L)>=1 then
    begin
      sourcehandle:=lua_tointeger(L,1);

      if lua_gettop(L)>=2 then
      begin
        if lua_gettop(L)>=3 then
        begin
          //frompid,topid
          frompid:=lua_tointeger(L,2);
          topid:=lua_tointeger(L,3);

          if frompid=GetCurrentProcessId then
            fromprocess:=GetCurrentProcess
          else
          if frompid=processid then
            fromprocess:=processhandle
          else
          begin
            fromprocess:=newkernelhandler.openProcess(PROCESS_DUP_HANDLE,false,frompid);
            if fromprocess<>0 then
              openedfromprocess:=true
            else
            begin
              lua_pushnil(L);
              lua_pushstring(L,'Failure opening process '+inttostr(frompid)+' ('+SysErrorMessage(GetLastOSError)+')');
              exit(2); //failed to open the process
            end;
          end;

          if topid=GetCurrentProcessId then
            toprocess:=GetCurrentProcess
          else
          if topid=processid then
            toprocess:=processhandle
          else
          begin
            toprocess:=newkernelhandler.openProcess(PROCESS_DUP_HANDLE,false,topid);
            if toprocess<>0 then
              openedtoprocess:=true
            else
            begin
              lua_pushnil(L);
              lua_pushstring(L,'Failure opening process '+inttostr(topid)+' ('+SysErrorMessage(GetLastOSError)+')');
              exit(2); //failed to open the process
            end;
          end;
        end
        else
        begin
          //mode
          if lua_tointeger(L,2)=1 then //target to CE, so switch processes
          begin
            toprocess:=GetCurrentProcess;
            fromprocess:=processhandle;
          end; //else as it was
        end;
      end;

      if DuplicateHandle(fromprocess, sourcehandle,toprocess,@newhandle, 0, false, DUPLICATE_SAME_ACCESS) then
      begin
        lua_pushinteger(L,newhandle);
        exit(1);
      end
      else
      begin
        lua_pushnil(L);
        lua_pushstring(L, 'Duplication failed due to :'+SysErrorMessage(GetLastOSError));
        exit(2);
      end;

    end;

  finally
    if openedfromprocess then closehandle(fromprocess);
    if openedtoprocess then closehandle(toprocess);
  end;
  {$ENDIF}
end;

function lua_getOperatingSystem(L: PLua_state): integer; cdecl;
begin
  lua_pushinteger(L,{$ifdef windows}0{$else}1{$endif});
  result:=1;
end;

function lua_createColorDialog(L: Plua_State): integer; cdecl;
var
  owner: TComponent;
  cd: TColorDialog;
begin
  result:=0;

  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  cd:=TColorDialog.Create(owner);
  luaclass_newClass(L, cd);
  result:=1;
end;

function lua_createColorBox(L: Plua_State): integer; cdecl;
var
  owner: TComponent;
  cb: TColorBox;
begin
  result:=0;

  if lua_gettop(L)=1 then
    owner:=lua_toceuserdata(L, 1)
  else
    owner:=nil;

  cb:=TColorBox.Create(owner);
  luaclass_newClass(L, cb);
  result:=1;
end;

function lua_createAutoAssemblerForm(L: Plua_State): integer; cdecl;
var f: TfrmAutoInject;
begin
  f:=TfrmAutoInject.Create(application);
  if lua_gettop(L)>=1 then
    f.assemblescreen.Text:=Lua_ToString(L,1);

  f.show;

  luaclass_newClass(L, f);
  result:=1;
end;


function lua_getRTTIClassName(L: Plua_State): integer; cdecl;
var address: ptruint;
  classname: string;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    address:=lua_tointeger(L,1);
    if getRTTIClassName(address, classname) then
    begin
      lua_pushstring(L,classname);
      exit(1);
    end;
  end;

end;

function lua_getAutoRunPath(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L,autorunpath);
  exit(1);
end;

function lua_extractfilenamewithoutext(L: Plua_State): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
  begin
    lua_pushstring(L, ExtractFileNameWithoutExt(Lua_ToString(L,1)));
    exit(1);
  end
  else
    exit(0);
end;

function lua_extractfileext(L: Plua_State): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
  begin
    lua_pushstring(L, ExtractFileExt(Lua_ToString(L,1)));
    exit(1);
  end
  else
    exit(0);
end;


function lua_extractFileName(L: Plua_State): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
  begin
    lua_pushstring(L, ExtractFileName(Lua_ToString(L,1)));
    exit(1);
  end
  else
    exit(0);
end;

function lua_extractFilePath(L: Plua_State): integer; cdecl;
begin
  if lua_gettop(L)>=1 then
  begin
    lua_pushstring(L, ExtractFilePath(Lua_ToString(L,1)));
    exit(1);
  end
  else
    exit(0);

end;

function lua_trim(L: Plua_State): integer; cdecl;
var s: string;
begin
  if lua_gettop(L)>=1 then
  begin
    s:=Lua_ToString(L,1);
    lua_pushstring(L,trim(s));
  end
  else
    lua_pushnil(L);

  result:=1;
end;

function lua_string_split(L: Plua_State): integer; cdecl;
var
  s: string;
  sep: string;

  arr: TStringDynArray;
  i: integer;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    s:=Lua_ToString(L,1);
    sep:=Lua_ToString(L,2);

    arr:=SplitString(s,sep);

    lua_pop(L,lua_gettop(L));
    if lua_checkstack(L, length(arr)) then
    begin
      for i:=0 to length(arr)-1 do
        lua_pushstring(L, arr[i]);

      result:=length(arr);
    end;
  end;
end;

function lua_string_endswith(L: Plua_State): integer; cdecl;
var
  s, endswith: string;
  ignoreCase: boolean;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    s:=Lua_ToString(L,1);
    endswith:=Lua_ToString(L,2);

    if lua_gettop(L)>=3 then
      ignorecase:=lua_toboolean(L,3)
    else
      ignorecase:=false;

    lua_pushboolean(L, s.EndsWith(s,ignorecase));
    result:=1;
  end;
end;

function lua_string_startswith(L: Plua_State): integer; cdecl;
var
  s, searchstring: string;
  ignoreCase: boolean;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    s:=Lua_ToString(L,1);
    searchstring:=Lua_ToString(L,2);

    if lua_gettop(L)>=3 then
      ignorecase:=lua_toboolean(L,3)
    else
      ignorecase:=false;

    lua_pushboolean(L, s.StartsWith(searchstring,ignorecase));
    result:=1;
  end;
end;

function lua_registerLuaFunctionHighlight(L: Plua_State): integer; cdecl;
begin
  if luasyntaxStringHashList<>nil then
    luasyntaxStringHashList.Add(Lua_ToString(L,-1));

  result:=0;
end;

function lua_unregisterLuaFunctionHighlight(L: Plua_State): integer; cdecl;
begin
  if luasyntaxStringHashList<>nil then
    luasyntaxStringHashList.Remove(Lua_ToString(L,-1));

  result:=0;
end;

function lua_setProgressState(L: Plua_State): integer; cdecl;
begin
  {$ifdef windows}
  if lua_gettop(L)>=1 then
    SetProgressState(TTaskBarProgressState(lua_tointeger(L,1)));
  {$endif}
  result:=0;
end;

function lua_setProgressValue(L: Plua_State): integer; cdecl;
begin
  {$ifdef windows}
  if lua_gettop(L)>=2 then
    SetProgressValue(lua_tointeger(L,1), lua_tointeger(L,2));
  {$endif}

  result:=0;
end;


function _lua_compile(L: Plua_State; isfile: boolean): integer; cdecl;
var
  s: string;
  a: ptruint=0;

  bytes: tmemorystream=nil;
  symbollist: TStringlist=nil;

  errorlog: tstringlist=nil;
  bw: size_t;


  targetself: boolean;

  ph: THandle;

  i: integer;
  list: TStringlist=nil;
  count: integer;
  useKernelAlloc: boolean;

  NoDebug: boolean;
  ln: TSourceCodeInfo;
  tr: TTCCRegionList;
  oldprotect: dword;
begin
  if lua_gettop(L)>=1 then
  begin
    if lua_isstring(L,1) then
    begin
      s:=Lua_ToString(L,1);
    end
    else
    if lua_istable(L,1) then
    begin
      list:=tstringlist.create;
      count:=lua_objlen(L,1);

      for i:=1 to count do
      begin
        lua_pushinteger(L,i);
        lua_gettable(L,1);
        list.add(Lua_ToString(L,-1));
        lua_pop(L,1);
      end;
    end;

    if (s='') and ((list=nil) or (list.count=0)) then
    begin
      lua_pushnil(L);
      lua_pushstring(L,'Nothing to compile');
      exit(2);
    end;

    if lua_gettop(L)>=2 then
      a:=lua_toaddress(L,2)
    else
      a:=0;

    if lua_gettop(L)>=3 then
      targetself:=lua_toboolean(L,3)
    else
      targetself:=false;

    if targetself then
      ph:=GetCurrentProcess
    else
      ph:=processhandle;

    if lua_gettop(L)>=4 then
      useKernelAlloc:=lua_toboolean(L,4)
    else
      useKernelAlloc:=false;

    if lua_gettop(L)>=5 then
      NoDebug:=lua_toboolean(L,5)
    else
      NoDebug:=false;


    bytes:=tmemorystream.Create;
    errorlog:=tstringlist.create;

    if isfile and (list=nil) then
    begin
      list:=tstringlist.create;
      list.add(s);
    end;

    try

      if a=0 then //allocate myself
      begin
        //test compile to get the size
        if ((list=nil) and (tcc.compileScript(s,$00400000,bytes,nil,nil,nil,errorlog,nil,targetself)=false)) or
           ((list<>nil) and (
                             ((isfile=false) and (tcc.compileScripts(list,$00400000,bytes,nil,nil,nil,errorlog,targetself)=false) ) or
                             ((isfile=true) and (tcc.compileProject(list,$00400000,bytes,nil,nil,nil,errorlog,targetself)=false) )
                             ))
        then
        begin
          lua_pop(L,lua_gettop(L));
          lua_pushnil(L);
          lua_pushstring(L, errorlog.Text);
          if list<>nil then
            freeandnil(list);
          exit(2);
        end;

{$ifdef windows}
        if useKernelAlloc then
          a:=ptruint(kernelalloc(bytes.size*2))
        else
{$endif}
        begin
          if SystemSupportsWritableExecutableMemory then
            a:=ptruint(VirtualAllocEx(ph,nil, bytes.Size*2,MEM_RESERVE or MEM_COMMIT,PAGE_EXECUTE_READWRITE))
          else
            a:=ptruint(VirtualAllocEx(ph,nil, bytes.Size*2,MEM_RESERVE or MEM_COMMIT,PAGE_READWRITE))
        end;

        if a=0 then
        begin
          lua_pop(L,lua_gettop(L));
          lua_pushnil(L);
          lua_pushstring(L, 'Allocation error.  Failed to allocate '+inttostr(bytes.size)+' bytes of memory');

          if list<>nil then
            freeandnil(list);
          exit(2);
        end;

        bytes.Clear;
        errorlog.Clear;
      end;

      symbollist:=TStringlist.create;


      //actual compile

      if nodebug=false then
        ln:=TSourceCodeInfo.create
      else
        ln:=nil;

      tr:=TTCCRegionList.Create;

      if ((list=nil) and (tcc.compileScript(s,a,bytes,symbollist,tr,ln,errorlog,nil,targetself)=false)) or
         ((list<>nil) and (
                           ((isfile=false) and (tcc.compileScripts(list,a,bytes,symbollist,tr,ln,errorlog,targetself)=false) ) or
                           ((isfile=true) and (tcc.compileProject(list,a,bytes,symbollist,tr,ln,errorlog,targetself)=false) )
                           )) then
      begin
        lua_pop(L,lua_gettop(L));
        lua_pushnil(L);
        lua_pushstring(L, errorlog.Text);

        if list<>nil then
          freeandnil(list);

        if symbollist<>nil then
          freeandnil(symbollist);

        exit(2);
      end;
      if writeprocessmemory(ph,pointer(a),bytes.memory,bytes.size, bw) then
      begin
        lua_newtable(L);

        for i:=0 to symbollist.count-1 do
        begin
          if ptruint(symbollist.objects[i])<a+bytes.size then
          begin
            lua_pushstring(L,symbollist[i]);
            lua_pushinteger(L,ptruint(symbollist.objects[i]));
            lua_settable(L,-3);
          end;
        end;

        result:=1;

        if errorlog.count>0 then
        begin
          lua_pushstring(L,errorlog.text);
          result:=2;
        end;

        if not SystemSupportsWritableExecutableMemory then
        begin
          OutputDebugString('Setting protections accordingly');
          for i:=0 to tr.Count-1 do
          begin
            OutputDebugString(format('%p : %d',[pointer(tr[i].address), tr[i].protection]));
            virtualprotectex(processhandle, pointer(tr[i].address), tr[i].size, tr[i].protection, oldprotect);
          end;
        end;
      end
      else
      begin
        lua_pop(L,lua_gettop(L));
        lua_pushnil(L);
        lua_pushstring(L, 'Failure writing memory');
        exit(2);
      end;

    finally
      if symbollist<>nil then freeandnil(symbollist);
      freeandnil(bytes);
      freeandnil(errorlog);
    end;
  end;
end;

function lua_compile(L: Plua_State): integer; cdecl;
begin
  exit(_lua_compile(L,false));
end;

function lua_compilefiles(L: Plua_State): integer; cdecl;
begin
  exit(_lua_compile(L,true));
end;

function lua_addCIncludePath(L: Plua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    tcc_addCIncludePath(Lua_ToString(L,1));
end;

function lua_removeCIncludePath(L: Plua_State): integer; cdecl;
begin
  result:=0;
  if lua_gettop(L)>=1 then
    tcc_removeCIncludePath(Lua_ToString(L,1));
end;



function lua_compiletcclib(L: Plua_State): integer; cdecl;
var
  libfile: string;
  sname: string;
  address: ptruint;
  slist: TSymbolListHandler;

  lowestAddress: ptruint=0;
  highestAddress: ptruint=0;
begin

  libfile:=CheatEngineDir+'tcclib'+PathDelim+'lib'+PathDelim+'libtcc1.c';  //release
  if not fileexists(libfile) then
    libfile:=CheatEngineDir+'..'+PathDelim+'tcclib'+PathDelim+'lib'+PathDelim+'libtcc1.c'; //development


  if fileexists(libfile) then
  begin
    lua_settop(L,0);
    lua_newtable(L);
    lua_pushinteger(L,1);
    lua_pushstring(L, libfile);
    lua_settable(L,-3);
    result:=lua_compilefiles(L);

    if result>0 then
    begin

      if lua_istable(L,-1) then
      begin
        //succesful compilation, register the symbols
        slist:=TSymbolListHandler.create;
        slist.PID:=processhandler.processid;
        slist.name:='TCC Library';

        lua_pushnil(L);  //first key (nil)
        while lua_next(L, -2)<>0 do
        begin
          sname:=Lua_ToString(L,-2);
          address:=lua_tointeger(L,-1);
          if lowestAddress=0 then lowestAddress:=address else lowestAddress:=min(lowestaddress, address);
          if highestAddress=0 then highestAddress:=address else highestAddress:=max(highestAddress, address);

          try
            slist.AddSymbol('tcclib',sname,address,1);
            symhandler.DeleteUserdefinedSymbol(sname);
            symhandler.AddUserdefinedSymbol(inttohex(address,8),sname,true);
          except
          end;
          lua_pop(L,1);
        end;

        slist.AddModule('tcc.lib',libfile,lowestAddress,highestAddress-lowestAddress, processhandler.is64Bit);
        symhandler.AddSymbolList(slist);


        lua_pushboolean(L, true);
        exit(1);
      end
      else exit;
    end
    else
    begin
      lua_pushnil(L);
      lua_pushstring(L,'invalid compilation result');
      exit(2);
    end;
  end
  else
  begin
    lua_pushnil(L);
    lua_pushstring(L, libfile+' could not be found');
    exit(2);
  end;
end;


function lua_dotNetExecuteClassMethod(L: Plua_State): integer; cdecl;
var
  path: string;
  namespace: string;
  methodname: string;
  classname: string;
  parameters: string;

  r: integer;
begin
  //function DotNetExecuteClassMethod(assemblypath: string; namespace: string; classname: string; methodname: string; parameters: string): integer;
  if lua_gettop(L)<5 then
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Incorrect parameter count (needed 5: path,namespace,classname,methodname, parameters)');
    exit(2);
  end;

  path:=Lua_ToString(L,1);
  namespace:=Lua_ToString(L,2);
  classname:=Lua_ToString(L,3);
  methodname:=Lua_ToString(L,4);
  parameters:=Lua_ToString(L,5);
  r:=DotNetExecuteClassMethod(path,namespace,classname,methodname, parameters);
  lua_pushinteger(L,r);
  result:=1;
end;

function lua_compilecs(L: Plua_State): integer; cdecl;
var
  references: tstringlist;
  script: string;
  fn: string;

  coreAssembly: string;
begin
  try
    if lua_gettop(L)<1 then raise exception.create('script parameter missing');

    script:=Lua_ToString(L,1);
    references:=tstringlist.create;

    try
      if lua_gettop(L)>1 then
      begin
        if lua_isstring(L,2) then //only one string
          references.add(lua_tostring(L,2))
        else
        begin
          //go through the whole list
          lua_pushnil(L);  //first key (nil)
          while lua_next(L, 2)<>0 do
          begin
            references.add(Lua_ToString(L,-1));
            lua_pop(L,1);
          end;
        end;
      end;

      if lua_gettop(L)>2 then
        coreAssembly:=Lua_ToString(L,3)
      else
        coreAssembly:='';


      fn:=compilecsharp(script, references, coreAssembly);
      lua_pushstring(L,fn);
      result:=1;
    finally
      references.free;
    end;

  except
    on e: exception do
    begin
      lua_pushnil(L);
      lua_pushstring(L,e.message);
      exit(2);
    end;
  end;
end;

function lua_signExtend(L: Plua_State): integer; cdecl;
var
  value: qword;
  mostsignificantBit: integer;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    value:=lua_tointeger(L,1);
    mostsignificantBit:=lua_tointeger(L,2);
    if (value shr mostSignificantBit)=1 then //needs to be sign extended
      value:=value or ((qword($ffffffffffffffff) shl mostSignificantBit));

    lua_pushinteger(L,value);
    result:=1;
  end
  else
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Incorrect paremeters: signExtend(value,mostSignificantBit)');
    exit(2);
  end;
end;

function lua_darkMode(L: Plua_State): integer; cdecl;
begin
  result:=1;
  lua_pushboolean(L, ShouldAppsUseDarkMode);
end;


function lua_getCEName(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, strCheatEngine);
  exit(1);
end;

function lua_getTempFolder(L: Plua_State): integer; cdecl;
begin
  lua_pushstring(L, GetTempDir);
  exit(1);
end;

function lua_deleteFile(L: Plua_State): integer; cdecl;
var s: string;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    s:=Lua_ToString(L,1);
    lua_pushboolean(L, DeleteFile(s));
    exit(1);
  end;
end;

function lua_fileExists(L: Plua_State): integer; cdecl;
var s: string;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    s:=Lua_ToString(L,1);
    lua_pushboolean(L, FileExists(s) );
    exit(1);
  end;
end;

function lua_getNextReadablePageCR3(L: Plua_State): integer; cdecl;
var
  cr3: qword;
  address: qword;
  newaddress: ptruint;
begin

  result:=0;
  {$ifdef windows}
  if lua_gettop(L)>=2 then
  begin
    cr3:=lua_tointeger(L,1) and MAXPHYADDRMASKPB;
    address:=lua_toaddress(L,2);

    if GetNextReadablePageCR3(cr3, address, newaddress) then
    begin
      lua_pushinteger(L,newaddress);
      result:=1;
    end;

  end;
  {$endif}
end;

function lua_getPageInfoCR3(L: Plua_State): integer; cdecl;
var
  cr3: qword;
  address: qword;
  mbi: TMEMORYBASICINFORMATION;

begin
  result:=0;
  {$ifdef windows}
  if lua_gettop(L)>=2 then
  begin
    cr3:=lua_tointeger(L,1) and MAXPHYADDRMASKPB;
    address:=lua_toaddress(L,2);

    if GetPageInfoCR3(cr3,address,mbi) then
    begin
      lua_newtable(L);

      lua_pushstring(L,'BaseAddress');
      lua_pushinteger(L,ptruint(mbi.BaseAddress));
      lua_settable(L,-3);

      lua_pushstring(L,'RegionSize');
      lua_pushinteger(L,mbi.RegionSize);
      lua_settable(L,-3);

      lua_pushstring(L,'Protect');
      lua_pushinteger(L,mbi.Protect);
      lua_settable(L,-3);

      result:=1;
    end;
  end;
  {$endif}
end;

function lua_growMemoryRegion(L: Plua_State): integer; cdecl;
//Not very useful at the moment.
var
  address: qword;
  paddress: pointer;
  newsize: integer;
  mbi, mbi2: TMemoryBasicInformation;

  mem: pointer;
  bc:  size_t;
  p: dword;
  r: boolean;
begin
  {$ifdef windows}
  if processid=GetCurrentProcessId then
  begin
    lua_pushnil(L);
    lua_pushstring(L,'Can''t grow memoryregions inside myself');
    exit(2);
  end;

  result:=0;
  if lua_Gettop(L)>=2 then
  begin
    address:=lua_toaddress(L,1);
    newsize:=lua_tointeger(L,2);

    ntsuspendProcess(processhandle);

    try
      if virtualqueryex(processhandle, pointer(address), mbi, sizeof(mbi))=sizeof(mbi) then
      begin
        if mbi._Type<>MEM_PRIVATE then
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Can only resize private type memory');
          exit(2);
        end;

        if mbi.State<>MEM_COMMIT then
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Must be commited memory');
          exit(2);
        end;

        if mbi.RegionSize>newsize then
        begin
          lua_pushnil(L);
          lua_pushstring(L,'New size is smaller than the given region');
          exit(2);
        end;

        if (ptruint(mbi.BaseAddress) and qword(not qword($fff)))<>ptruint(mbi.AllocationBase) then
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Can only resize a standalone memoryregion');
          exit(2);
        end;

        if virtualqueryex(processhandle, pointer(address+mbi.RegionSize), mbi2, sizeof(mbi2))<>sizeof(mbi2) then
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Failure obtaining subsequent memoryregion info');
          exit(2);
        end;

        if mbi2.State<>MEM_FREE then
        begin
          lua_pushnil(L);
          lua_pushstring(L,'Failure growing the region as the next region is not free');
          exit(2);
        end;

        mem:=getmem(newsize);
        ZeroMemory(mem,newsize);

        try

          r:=ReadProcessMemory(processhandle, pointer(mbi.BaseAddress), mem, mbi.RegionSize,bc);

          if (not r) or (bc<>mbi.RegionSize) then
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Failure reading original memory');
            exit(2);
          end;

          if VirtualFreeEx(processhandle, mbi.BaseAddress, 0, MEM_RELEASE)=false then
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Failure releasing the original memoryregion');
            exit(2);
          end;

          paddress:=VirtualAllocEx(processhandle,mbi.BaseAddress,newsize,MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);

          if (paddress=nil) or (paddress<>mbi.BaseAddress) then
          begin
            //fuuuuuuuuuuuuuu
            errorbeep;

            //try to restore it...
            paddress:=VirtualAllocEx(processhandle,mbi.BaseAddress,mbi.RegionSize,MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);

            if (paddress=nil) or (paddress<>mbi.BaseAddress) then
            begin
              //double fuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu
              errorbeep;
              errorbeep;

              lua_pushnil(L);
              lua_pushstring(L,'Failure allocating memory at the original region. Failed to restore original allocation. ALL DATA LOST, EXPECT MEMORY ERRORS!');
              exit(2);
            end;

            //managed to restore the original memory
            if WriteProcessMemory(processhandle, mbi.BaseAddress,mem,mbi.RegionSize,bc)=false then
            begin
              lua_pushnil(L);
              lua_pushstring(L,'Failure allocating memory at the original region. Restoration of data failed. ALL DATA LOST!');
              exit(2);
            end;

            VirtualProtectEx(processhandle, mbi.BaseAddress, mbi.RegionSize, mbi.Protect,p);

            lua_pushnil(L);
            lua_pushstring(L,'Failure allocating memory at the original region. No data lost');
            exit(2);
          end;

          r:=WriteProcessMemory(processhandle, mbi.BaseAddress,mem,newsize,bc);

          if (r=false) or (bc<>newsize) then
          begin
            lua_pushnil(L);
            lua_pushstring(L,'Failure restoring data in reallocated block. ALL DATA LOST!');
            exit(2);
          end;

          lua_pushinteger(L,ptruint(mbi.BaseAddress)+mbi.RegionSize);
          exit(1);
        finally
          freemem(mem);
        end;
      end
      else
      begin
        lua_pushnil(L);
        lua_pushstring(L,'Failure obtaining target memory information');
        exit(2);
      end;
    finally
      ntResumeProcess(processhandle);
    end
  end;
  {$else}
  exit(0);
  {$endif}
end;

function lua_getGlobalVariable(L: Plua_State):integer; cdecl;
var s: string;
begin
  result:=0;
  if lua_gettop(L)>=1 then
  begin
    s:=Lua_ToString(L,1);

    lua_getglobal(LuaVM, pchar(s));

    result:=1;

    case lua_type(LuaVM,-1) of
      LUA_TNIL: lua_pushnil(L);
      LUA_TBOOLEAN: lua_pushboolean(L, lua_toboolean(LuaVM,-1));
      LUA_TLIGHTUSERDATA: lua_pushlightuserdata(L, lua_ToCEUserData(LuaVM,-1));
      LUA_TNUMBER:  lua_pushnumber(L, lua_tonumber(LuaVM,-1));
      LUA_TSTRING:  lua_pushstring(L, Lua_ToString(LuaVM,-1));
      LUA_TUSERDATA: lua_pushlightuserdata(L, lua_touserdata(LuaVM,-1));
      else
        result:=0;
    end;

    lua_pop(LuaVM,1);
  end;

end;

function lua_setGlobalVariable(L: Plua_State):integer; cdecl;
var s: string;
begin
  result:=0;
  if lua_gettop(L)>=2 then
  begin
    s:=Lua_ToString(L,1);

    case lua_type(L,2) of
      LUA_TNIL: lua_pushnil(LuaVM);
      LUA_TBOOLEAN: lua_pushboolean(LuaVM, lua_toboolean(L,-1));
      LUA_TLIGHTUSERDATA: lua_pushlightuserdata(L, lua_ToCEUserData(LuaVM,-1));
      LUA_TNUMBER:  lua_pushnumber(LuaVM, lua_tonumber(L,-1));
      LUA_TSTRING:  lua_pushstring(LuaVM, Lua_ToString(L,-1));
      LUA_TUSERDATA: lua_pushlightuserdata(LuaVM, lua_touserdata(L,-1));
      else exit(0);
    end;

    lua_setglobal(LuaVM, pchar(s));
    lua_pushboolean(L,true);
    exit(1);
  end;
end;

function lua_releaseDebugFiles(L: Plua_State):integer; cdecl;
begin
  symhandler.StopSymbolLoaderThread;
  exit(0);
end;

function lua_enumRegisteredSymbols(L: Plua_State):integer; cdecl;
var
  list: TUserdefinedSymbolsList;
  s: integer;
  i: integer;
begin
  list:=[];
  symhandler.EnumerateUserdefinedSymbols(list);

  lua_createtable(L,length(list),0);
  for i:=0 to length(list)-1 do
  begin
    s:=2;
    if list[i].allocsize>0 then inc(s,2);
    if list[i].doNotSave then inc(s);

    lua_pushinteger(L,i+1);
    lua_createtable(L,0,s);

    lua_pushstring(L,'symbolname');
    lua_pushstring(L,list[i].symbolname);
    lua_settable(L,-3);
    lua_pushstring(L,'address');
    lua_pushinteger(L,list[i].address);
    lua_settable(L,-3);

    if list[i].allocsize>0 then
    begin
      lua_pushstring(L,'allocsize');
      lua_pushinteger(L,list[i].allocsize);
      lua_settable(L,-3);
      lua_pushstring(L,'processid');
      lua_pushinteger(L,list[i].processid);
      lua_settable(L,-3);
    end;

    if list[i].doNotSave then
    begin
      lua_pushstring(L,'donotsave');
      lua_pushboolean(L,list[i].donotsave);
      lua_settable(L,-3);
    end;

    lua_settable(L,-3);
  end;

  result:=1;
end;

function lua_deleteAllUserdefinedSymbols(L: Plua_State):integer; cdecl;
begin
  symhandler.deleteAllUserdefinedSymbols;
  exit(0);
end;

procedure InitLimitedLuastate(L: Plua_State);
begin
  //don't put functioncallback events in here, as limited luastates can be destroyed
  luaL_openlibs(L);

  lua_register(L, 'print', print);
  lua_register(L, 'sleep', lua_sleep);
  lua_register(L, 'cheatEngineIs64Bit', cheatEngineIs64Bit);
  lua_register(L, 'targetIs64Bit', targetIs64Bit);

  lua_register(L, 'readBytes', readbytes);
  lua_register(L, 'writeBytes', writebytes);
  lua_register(L, 'readByte', readShortInteger);
  lua_register(L, 'readShortInteger', readShortInteger);
  lua_register(L, 'readSmallInteger', readSmallInteger);
  lua_register(L, 'readInteger', readInteger);
  lua_register(L, 'readQword', readQword);
  lua_register(L, 'readPointer', readPointer);
  lua_register(L, 'readFloat', readFloat);
  lua_register(L, 'readDouble', readDouble);
  lua_register(L, 'readString', readString);
  lua_register(L, 'readByteLocal', readShortIntegerLocal);
  lua_register(L, 'readShortIntegerLocal', readShortIntegerLocal);
  lua_register(L, 'readSmallIntegerLocal', readSmallIntegerLocal);
  lua_register(L, 'readIntegerLocal', readIntegerLocal);
  lua_register(L, 'readQwordLocal', readQwordLocal);
  lua_register(L, 'readPointerLocal', readPointerLocal);
  lua_register(L, 'readFloatLocal', readFloatLocal);
  lua_register(L, 'readDoubleLocal', readDoubleLocal);
  lua_register(L, 'readStringLocal', readStringLocal);

  lua_register(L, 'writeShortInteger', writeShortInteger);
  lua_register(L, 'writeByte', writeShortInteger);
  lua_register(L, 'writeSmallInteger', writeSmallInteger);
  lua_register(L, 'writeInteger', writeInteger);
  lua_register(L, 'writeQword', writeQword);
  lua_register(L, 'writePointer', writePointer);
  lua_register(L, 'writeFloat', writeFloat);
  lua_register(L, 'writeDouble', writeDouble);
  lua_register(L, 'writeString', writeString);
  lua_register(L, 'writeByteLocal', writeShortIntegerLocal);
  lua_register(L, 'writeShortIntegerLocal', writeShortIntegerLocal);
  lua_register(L, 'writeSmallIntegerLocal', writeSmallIntegerLocal);
  lua_register(L, 'writeIntegerLocal', writeIntegerLocal);
  lua_register(L, 'writeQwordLocal', writeQwordLocal);
  lua_register(L, 'writePointerLocal', writePointerLocal);
  lua_register(L, 'writeFloatLocal', writeFloatLocal);
  lua_register(L, 'writeDoubleLocal', writeDoubleLocal);
  lua_register(L, 'writeStringLocal', writeStringLocal);


  lua_register(L, 'readBytesLocal', readbyteslocal);
  lua_register(L, 'writeBytesLocal', writebyteslocal);
  lua_register(L, 'getAddress', getAddress);
  lua_register(L, 'getAddressSafe', getAddressSafe);

  lua_register(L, 'getCurrentThreadID', lua_getCurrentThreadID);
  lua_register(L, 'inMainThread', inMainThread);
  lua_register(L, 'synchronize', lua_synchronize);
  lua_register(L, 'queue', lua_queue);


  lua_register(L, 'pause', pause);
  lua_register(L, 'unpause', unpause);

  lua_register(L, 'autoAssemble', autoAssemble_lua);
  lua_register(L, 'autoAssembleCheck', AutoAssembleCheck_lua);
  lua_register(L, 'assemble', lua_assemble);
  lua_register(L, 'deAlloc', deAlloc_lua);
  lua_register(L, 'deAllocLocal', deAllocLocal_lua);
  lua_register(L, 'showMessage', showMessage_lua);
  lua_register(L, 'inputQuery', inputQuery_lua);
  lua_register(L, 'getPixel', getPixel);
  lua_register(L, 'getMousePos', getMousePos);
  lua_register(L, 'setMousePos', setMousePos);
  lua_register(L, 'createTableEntry', createTableEntry);
  lua_register(L, 'getTableEntry', getTableEntry);

  lua_register(L, 'createSection',lua_createSection);
  lua_register(L, 'mapViewOfSection',lua_MapViewOfSection);
  lua_register(L, 'unMapViewOfSection', lua_unMapViewOfSection);

  lua_register(L, 'getSystemMetrics', lua_getSystemMetrics);

  lua_register(L, 'getScreenHeight', lua_getScreenHeight);
  lua_register(L, 'getScreenWidth', lua_getScreenWidth);
  lua_register(L, 'getScreenDPI', lua_getScreenDPI);

  lua_register(L, 'getWorkAreaHeight', lua_getWorkAreaHeight);
  lua_register(L, 'getWorkAreaWidth', lua_getWorkAreaWidth);


  lua_register(L, 'getScreenCanvas', lua_getScreenCanvas);
  lua_register(L, 'getHandleList', lua_getHandleList);
  lua_register(L, 'closeRemoteHandle', lua_closeRemoteHandle);

  lua_register(L, 'injectDLL', injectDLL);
  lua_register(L, 'injectLibrary', injectDLL);

  lua_register(L, 'getGlobalVariable', lua_getGlobalVariable);
  lua_register(L, 'setGlobalVariable', lua_setGlobalVariable);


  lua_register(L, 'loadPlugin', loadPlugin);

  lua_register(L, 'getCEVersion', getCEVersion);

  lua_register(L, 'utf8ToAnsi', lua_Utf8ToAnsi);
  lua_register(L, 'UTF8ToAnsi', lua_Utf8ToAnsi);
  lua_register(L, 'ansiToUtf8', lua_AnsiToUtf8);
  lua_register(L, 'ansiToUTF8', lua_AnsiToUtf8);



  lua_register(L, 'fullAccess', fullAccess);
  lua_register(L, 'setMemoryProtection', lua_setMemoryProtection);

  lua_register(L, 'waitForSections', waitForSections);
  lua_register(L, 'waitForExports', waitForExports);
  lua_register(L, 'waitForDotNet', waitForDotNet);
  lua_register(L, 'waitForPDB', waitForPDB);
  lua_register(L, 'waitforExports', waitForExports);
  lua_register(L, 'waitforDotNet', waitForDotNet);
  lua_register(L, 'waitforPDB', waitForPDB);
  lua_register(L, 'searchPDBWhileLoading', searchPDBWhileLoading);
  lua_register(L, 'reinitializeSymbolhandler', reinitializeSymbolhandler);
  lua_register(L, 'reinitializeDotNetSymbolhandler', reinitializeDotNetSymbolhandler);
  lua_register(L, 'reinitializeSelfSymbolhandler', reinitializeSelfSymbolhandler);
  lua_register(L, 'enumModules', enumModules);


  initializeLuaDisassembler(L);
  initializeLuaCanvas(L);


  lua_register(L, 'releaseDebugFiles', lua_releaseDebugFiles);
  lua_register(L, 'enumRegisteredSymbols', lua_enumRegisteredSymbols);

  lua_register(L, 'deleteAllRegisteredSymbols', lua_deleteAllUserdefinedSymbols);



end;

procedure InitializeLua;
var
  s: tstringlist;
  k32: THandle;
  i: integer;
  l: PLUA_STATE;
begin
  {$ifdef darwin}
  if assigned(luaL_newstate)=false then
  begin
    lua.initializeLua;

    if assigned(luaL_newstate)=false then
    begin
      outputdebugstring('Invalid lua config');
      exit;
    end;
  end;
  {$endif}

  if Thread_LuaVM<>nil then
    Thread_LuaVM:=nil;

  if _LuaVM<>nil then
    _LuaVM:=nil;

  _LuaVM:=lua_open();

  L:=LUAVM;
  if L<>nil then
  begin


    lua_atpanic(L, LuaPanic);
    InitLimitedLuastate(L);





    initializeLuaMemoryRecord;


    lua_register(L, 'mouse_event', lua_mouse_event);
    lua_register(L, 'isKeyPressed', isKeyPressed);
    lua_register(L, 'keyDown', keyDown);
    lua_register(L, 'keyUp', keyUp);
    lua_register(L, 'doKeyPress', doKeyPress);
    lua_register(L, 'getProcessIDFromProcessName', getProcessIDFromProcessName);
    lua_register(L, 'openProcess', openProcess);
    lua_register(L, 'debugProcess', debugProcess);
    lua_register(L, 'debug_getBreakpointList', debug_getBreakpointList);
    lua_register(L, 'debug_isDebugging', debug_isDebugging);
    lua_register(L, 'debug_getCurrentDebuggerInterface', debug_getCurrentDebuggerInterface);
    lua_register(L, 'debug_canBreak', debug_canBreak);
    lua_register(L, 'debug_breakThread', debug_breakThread);
    lua_register(L, 'debug_isBroken', debug_isBroken);
    lua_register(L, 'debug_isStepping', debug_isStepping);
    lua_register(L, 'debug_setBreakpoint', debug_setBreakpoint);
    lua_register(L, 'debug_setBreakpointForThread', debug_setBreakpointForThread);
    lua_register(L, 'debug_removeBreakpoint', debug_removeBreakpoint);
    lua_register(L, 'debug_continueFromBreakpoint', debug_continueFromBreakpoint);

    lua_register(L, 'debug_addThreadToNoBreakList', debug_addThreadToNoBreakList);
    lua_register(L, 'debug_removeThreadFromNoBreakList', debug_removeThreadFromNoBreakList);

    lua_register(L, 'debug_getContext', getDebugContext);
    lua_register(L, 'debug_setContext', setDebugContext);
    lua_register(L, 'debug_updateGUI', debug_updateGUI);

    lua_register(L, 'getDebugContext', getDebugContext);
    lua_register(L, 'setDebugContext', setDebugContext);



    lua_register(L, 'closeCE', closeCE);
    lua_register(L, 'hideAllCEWindows', hideAllCEWindows);
    lua_register(L, 'unhideMainCEwindow', unhideMainCEwindow);


    initializeLuaGroupbox;


    lua_register(L, 'createLabel', createLabel);
    lua_register(L, 'createSplitter', createSplitter);
    lua_register(L, 'createPaintBox', createPaintBox);

    lua_register(L, 'messageDialog', messageDialog);
    lua_register(L, 'speedhack_setSpeed', speedhack_setSpeed);
    lua_register(L, 'speedhack_getSpeed', speedhack_getSpeed);

    lua_register(L, 'getAutoAttachList', getAutoAttachList);


    lua_register(L, 'generateAPIHookScript', generateAPIHookScript_lua);
    lua_register(L, 'createProcess', createProcess);
    lua_register(L, 'AOBScan', AOBScan);
    lua_register(L, 'AOBScanUnique', AOBScanUnique);
    lua_register(L, 'AOBScanModuleUnique', AOBScanModuleUnique);

    lua_register(L, 'getOpenedProcessID', getOpenedProcessID);
    lua_register(L, 'getOpenedProcessHandle', getOpenedProcessHandle);

    lua_register(L, 'getModuleSize', getModuleSize);







    //ce6.1
    lua_register(L, 'getNameFromAddress', getNameFromAddress);
    lua_register(L, 'inModule', inModule);
    lua_register(L, 'inSystemModule', inSystemModule);
    lua_register(L, 'getCommonModuleList', getCommonModuleList);

    initializeLuaImage;

    initializeLuaRasterImage;

    initializeLuaGenericHotkey;



    initializeLuaObject;
    InitializeLuaComponent;


    InitializeLuaControl;

    initializeLuaWinControl;

    initializeLuaStrings;
    initializeLuaStringlist;

    initializeLuaForm;
    initializeLuaPanel;

    initializeLuaEdit;





    initializeLuaMemo;


    InitializeLuaButton;

    lua_register(L, 'createToggleBox', createToggleBox);

    initializeLuaCheckbox;
    initializeLuaRadioGroup;
    initializeLuaListbox;
    initializeLuaCheckListbox;
    initializeLuaCombobox;
    initializeLuaProgressbar;
    initializeLuaTrackbar;


    initializeLuaListColumn;
    initializeLuaCollection;

    initializeLuaListColumns;


    initializeLuaListItem;


    initializeLuaListItems;


    initializeLuaListview;
    initializeLuaTreeview;
    initializeLuaTimer;





    lua_register(L, 'openDialog_execute', openDialog_execute);
    lua_register(L, 'createOpenDialog', createOpenDialog);
    lua_register(L, 'createSaveDialog', createSaveDialog);
    lua_register(L, 'createSelectDirectoryDialog', createSelectDirectoryDialog);


    lua_register(L, 'createMemoryStream', createMemoryStream);
    lua_register(L, 'createFileStream', createFileStream);
    lua_register(L, 'createStringStream', createStringStream);



    Lua_register(L, 'getSettingsForm', getSettingsForm);
    Lua_register(L, 'getMemoryViewForm', getMemoryViewForm);
    Lua_register(L, 'getMainForm', getMainForm);
    Lua_register(L, 'getAddressList', getAddressList);
    Lua_register(L, 'getFreezeTimer', getFreezeTimer);
    Lua_register(L, 'getUpdateTimer', getUpdateTimer);

    lua_register(L, 'setGlobalKeyPollInterval', setGlobalKeyPollInterval);
    lua_register(L, 'setGlobalDelayBetweenHotkeyActivation', setGlobalDelayBetweenHotkeyActivation);

    initializeLuaMemoryview;
    initializeLuaTableFile;

    InitializeLuaXMPlayer;


    Lua_register(L, 'writeRegionToFile', writeRegionToFile);
    Lua_register(L, 'readRegionFromFile', readRegionFromFile);

    Lua_register(L, 'registerSymbol', registersymbol);
    Lua_register(L, 'unregisterSymbol', unregistersymbol);
    Lua_register(L, 'getSymbolInfo', getSymbolInfo);

    Lua_register(L, 'resetLuaState', resetLuaState);
    Lua_register(L, 'reloadSettingsFromRegistry', reloadSettingsFromRegistry);


    InitializeLuaCheatComponent;


    initializeMemoryRecordHotkey;

    InitializeLuaAddresslist;



    Lua_register(L, 'createMemScan', createMemScan);
    Lua_register(L, 'getCurrentMemscan', getCurrentMemscan);

    InitializeMemscan;
    InitializeFoundlist;


    Lua_register(L, 'supportCheatEngine', supportCheatEngine);
    Lua_register(L, 'fuckCheatEngine', fuckCheatEngine);



    lua_register(L, 'inheritsFromObject', inheritsFromObject);
    lua_register(L, 'inheritsFromComponent', inheritsFromComponent);
    lua_register(L, 'inheritsFromControl', inheritsFromControl);
    lua_register(L, 'inheritsFromWinControl', inheritsFromWinControl);

    Lua_register(L, 'beep', beep);

    lua_register(L, 'dbk_initialize', dbk_initialize);
    lua_register(L, 'dbk_useKernelmodeOpenProcess', dbk_useKernelmodeOpenProcess);
    lua_register(L, 'dbk_useKernelmodeProcessMemoryAccess', dbk_useKernelmodeProcessMemoryAccess);
    lua_register(L, 'dbk_useKernelmodeQueryMemoryRegions', dbk_useKernelmodeQueryMemoryRegions);
    lua_register(L, 'dbk_usePhysicalMemoryAccess', dbk_usePhysicalMemoryAccess);
    lua_register(L, 'dbk_setSaferPhysicalMemoryScanning', dbk_setSaferPhysicalMemoryScanning);


    lua_register(L ,'dbk_readPhysicalMemory', lua_dbk_readphysicalmemory);
    lua_register(L ,'dbk_writePhysicalMemory', lua_dbk_writephysicalmemory);


    lua_register(L, 'dbk_getPEProcess', dbk_getPEProcess);
    lua_register(L, 'dbk_getPEThread', dbk_getPEThread);
    lua_register(L, 'dbk_executeKernelMemory', dbk_executeKernelMemory);
    lua_register(L, 'dbk_readMSR', dbk_readMSR);
    lua_register(L, 'dbk_writeMSR', dbk_writeMSR);


    lua_register(L, 'dbk_getCR0', dbk_getCR0);
    lua_register(L, 'dbk_getCR3', dbk_getCR3);
    lua_register(L, 'dbk_getCR4', dbk_getCR4);
    lua_register(L, 'dbk_test', lua_dbk_test);
    lua_register(L, 'dbvm_getCR0', dbvm_getCR0);
    lua_register(L, 'dbvm_getCR3', dbvm_getCR3);
    lua_register(L, 'dbvm_getCR4', dbvm_getCR4);
    lua_register(L, 'dbvm_readMSR', lua_dbvm_readMSR);
    lua_register(L, 'dbvm_writeMSR', lua_dbvm_writeMSR);
    lua_register(L, 'dbvm_jtagbp', lua_dbvm_jtagbp);
    lua_register(L ,'dbvm_readPhysicalMemory', lua_dbvm_readphysicalmemory);
    lua_register(L ,'dbvm_writePhysicalMemory', lua_dbvm_writephysicalmemory);
    lua_register(L ,'dbvm_psod', lua_dbvm_psod);
    lua_register(L ,'dbvm_getNMIcount', lua_dbvm_getNMIcount);
    lua_register(L, 'dbvm_get_statistics',lua_dbvm_get_statistics);
    lua_register(L, 'dbvm_debug_setSpinlockTimeout', lua_dbvm_debug_setSpinlockTimeout);
    lua_register(L, 'dbvm_watch_writes', lua_dbvm_watch_writes);
    lua_register(L, 'dbvm_watch_reads', lua_dbvm_watch_reads);
    lua_register(L, 'dbvm_watch_executes', lua_dbvm_watch_executes);
    lua_register(L, 'dbvm_watch_retrievelog', lua_dbvm_watch_retrievelog);
    lua_register(L, 'dbvm_watch_disable', lua_dbvm_watch_disable);
    lua_register(L, 'dbvm_watch_getstatus', lua_dmvm_watch_getstatus);

    lua_register(L, 'dbvm_cloak_activate', lua_dbvm_cloak_activate);
    lua_register(L, 'dbvm_cloak_deactivate', lua_dbvm_cloak_deactivate);
    lua_register(L, 'dbvm_cloak_readOriginal', lua_dbvm_cloak_readOriginal);
    lua_register(L, 'dbvm_cloak_writeOriginal', lua_dbvm_cloak_writeOriginal);
    lua_register(L, 'dbvm_changeregonbp', lua_dbvm_changeregonbp);
    lua_register(L, 'dbvm_removechangeregonbp', lua_dbvm_removechangeregonbp);

    lua_register(L, 'dbvm_traceonbp', lua_dbvm_traceonbp);
    lua_register(L, 'dbvm_traceonbp_getstatus', lua_dbvm_traceonbp_getstatus);
    lua_register(L, 'dbvm_traceonbp_stoptrace', lua_dbvm_traceonbp_stoptrace);
    lua_register(L, 'dbvm_traceonbp_remove', lua_dbvm_traceonbp_remove);
    lua_register(L, 'dbvm_traceonbp_retrievelog', lua_dbvm_traceonbp_retrievelog);


    lua_register(L, 'dbvm_bp_getBrokenThreadListSize', lua_dbvm_bp_getBrokenThreadListSize);
    lua_register(L, 'dbvm_bp_getBrokenThreadEventShort', lua_dbvm_bp_getBrokenThreadEventShort);
    lua_register(L, 'dbvm_bp_getBrokenThreadEventFull', lua_dbvm_bp_getBrokenThreadEventFull);
    lua_register(L, 'dbvm_bp_setBrokenThreadEventFull', lua_dbvm_bp_setBrokenThreadEventFull);
    lua_register(L, 'dbvm_bp_resumeBrokenThread', lua_dbvm_bp_resumeBrokenThread);
    lua_register(L, 'dbvm_bp_getProcessAndThreadIDFromEvent', lua_dbvm_bp_getProcessAndThreadIDFromEvent);




    lua_register(L, 'dbvm_ept_reset', lua_dbvm_ept_reset);
    lua_register(L, 'dbvm_log_cr3_start', lua_dbvm_log_cr3_start);
    lua_register(L, 'dbvm_log_cr3_stop', lua_dbvm_log_cr3_stop);
    lua_register(L, 'dbvm_registerPlugin', lua_dbvm_registerPlugin);
    lua_register(L, 'dbvm_raisePMI', lua_dbvm_raisePMI); //mostly just for debugging
    lua_register(L, 'dbvm_ultimap2_hideRangeUsage', lua_dbvm_ultimap2_hideRangeUsage); //same
    lua_register(L, 'dbvm_ultimap_getDebugInfo', lua_dbvm_ultimap_getDebugInfo); //more debugging

    lua_register(L, 'dbvm_setTSCAdjust', lua_dbvm_setTSCAdjust);
    lua_register(L, 'dbvm_speedhack_setSpeed', lua_dbvm_speedhack_setSpeed);


    lua_register(L, 'dbvm_enableTSCHook', lua_dbvm_enableTSCHook);
    lua_register(L, 'dbvm_disableTSCHook', lua_dbvm_disableTSCHook);

    lua_register(L, 'dbvm_findCR3', lua_dbvm_findCR3);
    lua_register(L, 'dbvm_hidephysicalmemory', lua_dbvm_hidephysicalmemory);
    lua_register(L, 'dbvm_hidephysicalmemoryall', lua_dbvm_hidephysicalmemoryall);

    lua_register(L, 'getPageInfoCR3', lua_getPageInfoCR3);
    lua_register(L, 'getNextReadablePageCR3', lua_getNextReadablePageCR3);




    lua_register(L, 'dbk_getPhysicalAddress', dbk_getPhysicalAddress);
    lua_register(L, 'dbk_writesIgnoreWriteProtection', dbk_writesIgnoreWriteProtection);

    lua_register(L, 'getPhysicalAddressCR3', lua_getPhysicalAddressCR3);
    lua_register(L, 'readProcessMemoryCR3', lua_readProcessMemoryCR3);
    lua_register(L, 'writeProcessMemoryCR3', lua_writeProcessMemoryCR3);



    lua_register(L, 'allocateSharedMemoryLocal', allocateSharedMemoryLocal);
    lua_register(L, 'deallocateSharedMemoryLocal', deallocateSharedMemoryLocal);

    lua_register(L, 'allocateSharedMemory', allocateSharedMemory);
    lua_register(L, 'deallocateSharedMemory', deallocateSharedMemory);
    lua_register(L, 'getCheatEngineDir', getCheatEngineDir);
    lua_register(L, 'getCheatEngineProcessID', lua_getCheatEngineProcessID);

    lua_register(L, 'disassemble', disassemble_lua);
    lua_register(L, 'splitDisassembledString', splitDisassembledString);
    lua_register(L, 'getInstructionSize', getInstructionSize);
    lua_Register(L, 'getPreviousOpcode', getPreviousOpcode);

    initializegraphiccontrol;

    lua_register(L, 'disassemblerview_getSelectedAddress', disassemblerview_getSelectedAddress);
    lua_register(L, 'disassemblerview_setSelectedAddress', disassemblerview_setSelectedAddress);
    lua_register(L, 'disassemblerview_onSelectionChange', disassemblerview_onSelectionChange);

    lua_register(L, 'hexadecimalview_getTopAddress', hexadecimalview_getTopAddress);
    lua_register(L, 'hexadecimalview_setTopAddress', hexadecimalview_setTopAddress);
    lua_register(L, 'hexadecimalview_onAddressChange', hexadecimalview_onAddressChange);
    lua_register(L, 'hexadecimalview_onByteSelect', hexadecimalview_onByteSelect);



    lua_register(L, 'getForegroundProcess', getForegroundProcess);



    lua_register(L, 'getFormCount', getFormCount);
    lua_register(L, 'getForm', getForm);
    lua_register(L, 'registerFormAddNotification', registerFormAddNotification);
    Lua_Register(L, 'unregisterFormAddNotification', unregisterFormAddNotification);


    lua_register(L, 'onAutoGuess', onAutoGuess);
    lua_register(L, 'onAPIPointerChange', onAPIPointerChange);

    lua_register(L, 'setAPIPointer', setAPIPointer);

    lua_register(L, 'dbvm_initialize', dbvm_initialize);
    lua_register(L, 'dbvm_setKeys', dbvm_setKeys);
    lua_register(L, 'dbvm_getMemory', lua_dbvm_getMemory);
    lua_register(L, 'dbvm_addMemory', dbvm_addMemory);

    lua_register(L, 'shellExecute', lua_shellExecute);
    lua_register(L, 'runCommand', lua_runCommand);

    lua_register(L, 'getTickCount', getTickCount_lua);
    lua_register(L, 'rdtsc', lua_rdtsc);


    lua_register(L, 'processMessages', processMessages);

    lua_register(L, 'integerToUserData', integerToUserData);
    lua_register(L, 'userDataToInteger', userDataToInteger);



    lua_register(L, 'writeToClipboard', writeToClipboard);
    lua_register(L, 'readFromClipboard', readFromClipboard);

    lua_register(L, 'createBitmap', createBitmap);
    lua_register(L, 'createPNG', createPNG);
    lua_register(L, 'createJpeg', createJpeg);
    lua_register(L, 'createIcon', createIcon);
    lua_register(L, 'errorOnLookupFailure', errorOnLookupFailure);
    lua_register(L, 'waitforsymbols', lua_waitforsymbols);



    lua_register(L, 'getWindowlist', getWindowList_lua);
    lua_register(L, 'getWindowList', getWindowList_lua);

    lua_register(L, 'getProcesslist', getProcessList_lua);
    lua_register(L, 'getProcessList', getProcessList_lua);
    lua_register(L, 'getThreadlist', getThreadlist_lua);
    lua_register(L, 'getThreadList', getThreadlist_lua);

    Lua_register(L, 'loadTable', lua_loadTable);
    Lua_register(L, 'saveTable', lua_saveTable);
    Lua_register(L, 'signTable', lua_signTable);

    Lua_register(L, 'detachIfPossible', lua_DetachIfPossible);
    Lua_register(L, 'getComment', getComment);
    Lua_register(L, 'setComment', setComment);
    Lua_register(L, 'getHeader', getHeader);
    Lua_register(L, 'setHeader', setHeader);

    lua_register(L, 'createClass', lua_createClass);
    lua_register(L, 'createComponentClass', lua_createComponentClass);

    lua_register(L, 'openLuaServer', openLuaServer);

    lua_register(L, 'registerAutoAssemblerCommand', lua_registerAutoAssemblerCommand);
    lua_register(L, 'unregisterAutoAssemblerCommand', lua_unregisterAutoAssemblerCommand);

    lua_register(L, 'registerSymbolLookupCallback', lua_registerSymbolLookupCallback);
    lua_register(L, 'unregisterSymbolLookupCallback', lua_unregisterSymbolLookupCallback);
    lua_register(L, 'registerAddressLookupCallback', lua_registerAddressLookupCallback);
    lua_register(L, 'unregisterAddressLookupCallback', lua_unregisterAddressLookupCallback);

    lua_register(L, 'registerGlobalDisassembleOverride', lua_registerGlobalDisassembleOverride);
    lua_register(L, 'unregisterGlobalDisassembleOverride', lua_unregisterGlobalDisassembleOverride);



    lua_register(L, 'registerGlobalStructureListUpdateNotification', lua_registerGlobalStructureListUpdateNotification);
    lua_register(L, 'unregisterGlobalStructureListUpdateNotification', lua_unregisterGlobalStructureListUpdateNotification);


    lua_register(L, 'registerStructureDissectOverride', lua_registerStructureDissectOverride);
    lua_register(L, 'unregisterStructureDissectOverride', lua_unregisterStructureDissectOverride);

    lua_register(L, 'registerStructureNameLookup', lua_registerStructureNameLookup);
    lua_register(L, 'unregisterStructureNameLookup', lua_unregisterStructureNameLookup);

    lua_register(L, 'registerAssembler', lua_registerAssembler);
    lua_register(L, 'unregisterAssembler', lua_unregisterAssembler);

    lua_register(L, 'registerAutoAssemblerPrologue', lua_registerAutoAssemblerPrologue);
    lua_register(L, 'unregisterAutoAssemblerPrologue', lua_unregisterAutoAssemblerPrologue);

    lua_register(L, 'registerStructureAndElementListCallback', lua_registerStructureAndElementListCallback);
    lua_register(L, 'unregisterStructureAndElementListCallback', lua_unregisterStructureAndElementListCallback);




    lua_register(L, 'shortCutToText', lua_shortCutToText);
    lua_register(L, 'textToShortCut', lua_textToShortCut);



    lua_register(L, 'checkSynchronize', lua_checkSynchronize);

    lua_register(L, 'playSound', lua_playSound);

    lua_register(L, 'outputDebugString', lua_outputDebugString);
    lua_register(L, 'broadcastEnvironmentUpdate', broadcastEnvironmentUpdate);
    lua_register(L, 'getUserRegistryEnvironmentVariable', getUserRegistryEnvironmentVariable);
    lua_register(L, 'setUserRegistryEnvironmentVariable', setUserRegistryEnvironmentVariable);

    lua_register(L, 'createRef', createRef);
    lua_register(L, 'getRef', getRef);
    lua_register(L, 'destroyRef', destroyRef);

    lua_register(L, 'activateProtection', activateProtection);
    lua_register(L, 'getLuaEngine', getLuaEngine);
    lua_register(L, 'createLuaEngine', createLuaEngine);
    lua_register(L, 'getApplication', getApplication);

    lua_Register(L, 'stringToMD5String', lua_stringToMD5String);
    lua_register(L, 'convertKeyComboToString', lua_ConvertKeyComboToString);
    lua_register(L, 'restoreSeDebugPrivilege', restoreSeDebugPrivilege);

    lua_register(L, 'translate', lua_translate);
    lua_register(L, 'translateID', lua_translateid);
    lua_register(L, 'loadPOFile', lua_loadPOFile);
    lua_register(L, 'getTranslationFolder', lua_getTranslationFolder);

    lua_register(L, 'registerBinUtil', lua_registerBinUtil);
    lua_register(L, 'setPointerSize', setPointerSize);


    lua_register(L, 'executeCode', executeCode);
    lua_register(L, 'executeCodeEx', executeCodeEx);
    lua_register(L, 'executeMethod', executeMethod);

    lua_register(L, 'createExecuteMethodStub', createExecuteMethodStub);
    lua_register(L, 'createExecuteCodeExStub', createExecuteCodeExStub);



    lua_register(L, 'executeCodeLocal', executeCodeLocal);
    lua_register(L, 'executeCodeLocalEx', executeCodeLocalEx);

    lua_register(L, 'md5file', md5file);
    lua_register(L, 'md5memory', md5memory);

    lua_register(L, 'allocateKernelMemory', allocateKernelMemory);
    lua_register(L, 'freeKernelMemory', freeKernelMemory);

    lua_register(L, 'mapMemory', lua_mapMemory);
    lua_register(L, 'unmapMemory', lua_unmapMemory);

    lua_register(L, 'lockMemory', lua_lockMemory);
    lua_register(L, 'unlockMemory', lua_unlockMemory);

    lua_register(L, 'sendMessage', lua_sendMessage);
    lua_register(L, 'sendMessageTimeout', lua_sendMessageTimeout);

    lua_register(L, 'findWindow', lua_findWindow);
    lua_register(L, 'getWindow', lua_getWindow);
    lua_register(L, 'getWindowProcessID', lua_getWindowProcessID);
    lua_register(L, 'getWindowCaption', lua_getWindowCaption);
    lua_register(L, 'getWindowClassName', lua_getWindowClassName);
    lua_register(L, 'getForegroundWindow', lua_getForegroundWindow);


    lua_register(L, 'getXBox360ControllerKeyPress', getXBox360ControllerKeyPress);
    lua_register(L, 'getXBox360ControllerState', getXBox360ControllerState);
    lua_register(L, 'setXBox360ControllerVibration', setXBox360ControllerVibration);

    lua_register(L, 'registerAutoAssemblerTemplate', lua_registerAutoAssemblerTemplate);
    lua_register(L, 'unregisterAutoAssemblerTemplate', lua_unregisterAutoAssemblerTemplate);
    lua_register(L, 'getUniqueAOB', lua_getUniqueAOB);
    lua_register(L, 'addSnapshotAsComment',lua_addSnapshotAsComment);
    lua_register(L, 'getNextAllocNumber',lua_getNextAllocNumber);



    lua_register(L, 'generateCodeInjectionScript', lua_GenerateCodeInjectionScript);
    lua_register(L, 'generateAOBInjectionScript', lua_GenerateAOBInjectionScript);
    lua_register(L, 'generateFullInjectionScript', lua_GenerateFullInjectionScript);

    lua_register(L, 'loadFontFromStream', lua_loadFontFromStream);
    lua_register(L, 'unloadLoadedFont', lua_unloadLoadedFont);

    lua_register(L, 'speak', lua_speak);
    lua_register(L, 'speakEnglish', lua_speakEnglish);

    lua_register(L, 'getFileVersion', lua_getFileVersion);
    lua_register(L, 'getCheatEngineFileVersion', lua_getCheatEngineFileVersion);


    lua_register(L, 'hookWndProc', lua_hookWndProc);
    lua_register(L, 'unhookWndProc', lua_unhookWndProc);

    lua_register(L, 'registerEXETrainerFeature', lua_registerEXETrainerFeature);
    lua_register(L, 'unregisterEXETrainerFeature', lua_unregisterEXETrainerFeature);

    lua_register(L, 'encodeFunction', lua_encodefunction);
    lua_register(L, 'decodeFunction', lua_decodeFunction);

    lua_register(L, 'encodeFunctionEx', lua_encodefunctionEx);

    lua_register(L, 'getFileList', lua_getFileList);
    lua_register(L, 'getDirectoryList', lua_getDirectoryList);

    lua_register(L, 'connectToCEServer', lua_connectToCEServer);

    lua_register(L, 'copyMemory', lua_copyMemory);
    lua_register(L, 'enableDRM', lua_enableDRM);

    lua_register(L, 'openFileAsProcess', lua_openFileAsProcess);
    lua_register(L, 'getOpenedFileSize', lua_getOpenedFileSize);
    lua_register(L, 'saveOpenedFile', lua_saveOpenedFile);


    lua_register(L, 'getPEB', lua_getPEB);

    lua_register(L, 'createAPC', lua_createAPC);
    lua_register(L, 'setAssemblerMode', lua_setAssemblerMode);
    lua_register(L, 'allocateMemory', lua_allocateMemory);

    lua_register(L, 'try', lua_try);



    lua_register(L, 'showSelectionList', lua_showSelectionList);
    lua_register(L, 'cpuid', lua_cpuid);
    lua_register(L, 'gc_setPassive', lua_gc_setPassive);
    lua_register(L, 'gc_setActivate', lua_gc_setPassive);
    lua_register(L, 'gc_setActive', lua_gc_setPassive);

    lua_register(L, 'getHotkeyHandlerThread', lua_getHotkeyHandlerThread);
    lua_register(L, 'enumMemoryRegions', lua_enumMemoryRegions);
    lua_register(L, 'enumMemoryRegionsCR3', lua_enumMemoryRegionsCR3);
    lua_register(L, 'setForceCR3VirtualQueryEx', lua_setForceCR3VirtualQueryEx);


    lua_register(L, 'enableWindowsSymbols', lua_enableWindowsSymbols);
    lua_register(L, 'enableKernelSymbols', lua_enableKernelSymbols);

    lua_register(L, 'compareMemory', lua_compareMemory);

    lua_register(L, 'enumExports', lua_enumExports);
    lua_register(L, 'duplicateHandle', lua_duplicateHandle);

    lua_register(L, 'getOperatingSystem', lua_getOperatingSystem);
    lua_register(L, 'createColorDialog', lua_createColorDialog);
    lua_register(L, 'createColorBox', lua_createColorBox);
    lua_register(L, 'createAutoAssemblerForm', lua_createAutoAssemblerForm);
    lua_register(L, 'getRTTIClassName', lua_getRTTIClassName);

    lua_register(L, 'getAutoRunPath', lua_getAutoRunPath);
    lua_register(L, 'getAutorunPath', lua_getAutoRunPath);
    lua_register(L, 'extractFileName', lua_extractFileName);
    lua_register(L, 'extractFileExt', lua_extractFileExt);
    lua_register(L, 'extractFileNameWithoutExt', lua_extractFileNameWithoutExt);
    lua_register(L, 'extractFilePath', lua_extractFilePath);

    lua_register(L, 'registerLuaFunctionHighlight', lua_registerLuaFunctionHighlight);
    lua_register(L, 'unregisterLuaFunctionHighlight', lua_unregisterLuaFunctionHighlight);

    lua_register(L, 'setProgressState', lua_SetProgressState);
    lua_register(L, 'setProgressValue', lua_SetProgressValue );

    lua_register(L, 'compile', lua_compile);
    lua_register(L, 'compileFiles', lua_compilefiles);
    lua_register(L, 'compileTCCLib', lua_compiletcclib);


    lua_register(L, 'addCIncludePath', lua_addCIncludePath);
    lua_register(L, 'removeCIncludePath', lua_removeCIncludePath);

    lua_register(L, 'dotNetExecuteClassMethod', lua_dotNetExecuteClassMethod);
    lua_register(L, 'compileCS', lua_compilecs);
    lua_register(L, 'compileCSharp', lua_compilecs);
    lua_register(L, 'compilecsharp', lua_compilecs);


    lua_register(L, 'signExtend', lua_signExtend);

    lua_register(L, 'darkMode', lua_darkMode);
    lua_register(L, 'getCEName', lua_getCEName);
    lua_register(L, 'getTempFolder', lua_getTempFolder);
    lua_register(L, 'fileExists', lua_fileExists);
    lua_register(L, 'deleteFile', lua_deleteFile);

    lua_register(L, 'growMemoryRegion', lua_growMemoryRegion);





    initializeLuaRemoteThread;

    initializeLuaCustomControl;
    initializeLuaPicture;
    initializeLuaPen;
    initializeLuaBrush;
    initializeLuaFont;

    initializeLuaMenu;

    initializeLuaDebug; //eventually I should add a LuaLuaDebug...
    initializeLuaThread;
    initializeLuaGraphic;
    initializeLuaOldD3DHook;
    initializeLuaD3DHook;
    initializeLuaStructure;
    initializeLuaRegion;
    initializeLuaDisassembler(L);
    initializeLuaDissectCode;
    initializeLuaByteTable;
    initializeLuaBinary;
    initializeLuaPipeClient;
    {$IFDEF windows}

    initializeLuaPipeServer;
    {$ENDIF}
    initializeLuaSymbolListHandler;
    initializeLuaFindDialog;
    initializeLuaSettings;
    initializeLuaPageControl;

    initializeLuaCalendar;
    initializeLuaRipRelativeScanner;


    initializeLuaStructureFrm;
    initializeLuaInternet;
    initializeLuaCustomType;
    initializeLuaSQL;
    {$IFDEF windows}
    initializeLuaModuleLoader;
    {$ENDIF}
    initializeLuaPointerValueList;
    initializeLuaWriteLog;

    initializeLuaDiagram;
    initializeLuaUltimap2;
    initializeLuaCodeFilter;
    initializeLuaSynEdit;
    initializeLuaCustomImageList;
    initializeLuaDotNetPipe;
    InitializeLuaRemoteExecutor;
    initializeLuaCECustomButton;





    s:=tstringlist.create;
    try
      //ce 6.0 compatibility. 6.0 has these methods in the stringlist instead of the strings class
      s.add('package.path = package.path .. ";?.lua";');
      {$ifdef darwin}
      s.add('package.path = package.path .. [[;'+getcedir+'?.lua]]');
      s.add('package.path = package.path .. [[;'+extractfiledir(extractfiledir(application.exename))+'/Lua/?.lua]]');
      {$endif}


{$ifdef cpu64}
      s.add('package.cpath = package.cpath .. [[;'+getcedir+'clibs64'+pathdelim+'?'+{$ifdef windows}'.dll'{$endif}{$ifdef darwin}'.dylib'{$endif}+']]');
      s.add('package.cpath = package.cpath .. [[;.'+pathdelim+'clibs64'+pathdelim+'?'+{$ifdef windows}'.dll'{$endif}{$ifdef darwin}'.dylib'{$endif}+']]');
{$else}
      s.add('package.cpath = package.cpath .. [[;'+getcedir+'clibs32'+pathdelim+'?'+{$ifdef windows}'.dll'{$endif}{$ifdef darwin}'.dylib'{$endif}+']]');
      s.add('package.cpath = package.cpath .. [[;.'+pathdelim+'clibs32'+pathdelim+'?'+{$ifdef windows}'.dll'{$endif}{$ifdef darwin}'.dylib'{$endif}+']]');
{$endif}
      s.add('stringlist_getCount=strings_getCount');
      s.add('stringlist_getString=strings_getString');
      s.add('stringlist_add=strings_add');
      s.add('stringlist_remove=strings_remove');

      //same for the rename of memrec to memoryrecord
      s.add('memrec_setDescription = memoryrecord_setDescription');
      s.add('memrec_getDescription = memoryrecord_getDescription');
      s.add('memrec_getAddress = memoryrecord_getAddress');
      s.add('memrec_setAddress = memoryrecord_setAddress');
      s.add('memrec_getType = memoryrecord_getType');
      s.add('memrec_setType = memoryrecord_setType');
      s.add('memrec_getValue = memoryrecord_getValue');
      s.add('memrec_setValue = memoryrecord_setValue');
      s.add('memrec_getScript = memoryrecord_getScript');
      s.add('memrec_isActive = memoryrecord_isActive');
      s.add('memrec_freeze = memoryrecord_freeze');
      s.add('memrec_unfreeze = memoryrecord_unfreeze');
      s.add('memrec_setColor = memoryrecord_setColor');
      s.add('memrec_appendToEntry = memoryrecord_appendToEntry');
      s.add('memrec_delete = memoryrecord_delete');
      s.add('getAddressFromName = getAddress');

      //timer onInterval has been renamed to timer onTimer
      s.add('timer_onInterval = timer_onTimer');


      {$ifdef windows}
      k32:=loadlibrary('kernel32.dll');
      s.add('windows_OpenProcess=0x'+inttohex(ptruint(getProcAddress(k32, 'OpenProcess')),8));
      s.add('windows_ReadProcessMemory=0x'+inttohex(ptruint(getProcAddress(k32, 'ReadProcessMemory')),8));
      s.add('windows_WriteProcessMemory=0x'+inttohex(ptruint(getProcAddress(k32, 'WriteProcessMemory')),8));
      s.add('windows_VirtualQueryEx=0x'+inttohex(ptruint(getProcAddress(k32, 'VirtualQueryEx')),8));


      s.add('dbk_OpenProcess=0x'+inttohex(ptruint(@DBK32functions.OP),8));
      s.add('dbk_NtOpenProcess=0x'+inttohex(ptruint(@DBK32functions.NOP),8));
      s.add('dbk_ReadProcessMemory=0x'+inttohex(ptruint(@DBK32functions.RPM),8));
      s.add('dbk_WriteProcessMemory=0x'+inttohex(ptruint(@DBK32functions.WPM),8));
      s.add('dbk_VirtualQueryEx=0x'+inttohex(ptruint(@DBK32functions.VQE),8));
      s.add('dbk_ReadPhysicalMemory=0x'+inttohex(ptruint(@DBK32functions.ReadPhysicalMemory),8));
      s.add('dbk_WritePhysicalMemory=0x'+inttohex(ptruint(@DBK32functions.WritePhysicalMemory),8));
      s.add('VirtualQueryExPhysical=0x'+inttohex(ptruint(@VirtualQueryExPhysical),8));

      s.add('dbvm_ReadPhysicalMemory=0x'+inttohex(ptruint(@vmxfunctions.dbvm_read_physical_memory),8));
      s.add('dbvm_WritePhysicalMemory=0x'+inttohex(ptruint(@vmxfunctions.dbvm_write_physical_memory),8));


      s.add('dbvm_block_interrupts=0x'+inttohex(ptruint(@vmxfunctions.dbvm_block_interrupts),8));
      s.add('dbvm_raise_privilege=0x'+inttohex(ptruint(@vmxfunctions.dbvm_raise_privilege),8));
      s.add('dbvm_restore_interrupts=0x'+inttohex(ptruint(@vmxfunctions.dbvm_restore_interrupts),8));
      s.add('dbvm_changeselectors=0x'+inttohex(ptruint(@vmxfunctions.dbvm_changeselectors),8));

      s.add('clWindow=0x'+inttohex(clWindow,8));
      s.add('clWindowText=0x'+inttohex(clWindowtext,8));
      {$endif}

      //5.2 backward compatibility:
      s.add('math.log10=function(v) return math.log(v,10) end');
      s.add('loadstring=load');
      s.add('unpack=table.unpack');
      s.add('package.loaders=package.searchers');

      //5.3 backward compatibility:
      s.add('math.pow=function(x,y) return x^y end');
      s.add('math.atan2=math.atan');
      s.add('math.ldexp=function(x,exp) return x * 2.0^exp end');
      s.add('math.mod=math.fmod');
      s.add('string.gfind=string.gmatch');

      s.add('BinUtils={}');
      s.add('math.randomseed(os.time())');

      lua_doscript(s.text);

      s.clear;
      s.add('function printf(...) print(string.format(...)) end');
      s.add('registerLuaFunctionHighlight("printf")');
      lua_doscript(s.text);

      lua_getglobal(L, 'string');
      lua_pushstring(L,'split');
      lua_pushcfunction(L, lua_string_split);
      lua_settable(L,-3);

      lua_pushstring(L,'endsWith');
      lua_pushcfunction(L, lua_string_endswith);
      lua_settable(L,-3);

      lua_pushstring(L,'startsWith');
      lua_pushcfunction(L, lua_string_startswith);
      lua_settable(L,-3);

      lua_pushstring(L,'trim');
      lua_pushcfunction(L, lua_trim);
      lua_settable(L,-3);
      lua_pop(L,1);

      lua_getglobal(L, 'math');
      i:=lua_gettop(L);

      lua_pushstring(L, 'frexp');
      lua_pushcfunction(L, lua_frexp);
      lua_settable(L,i);

      lua_pushstring(L, 'cosh');
      lua_pushcfunction(L, lua_cosh);
      lua_settable(L,i);

      lua_pushstring(L, 'sinh');
      lua_pushcfunction(L, lua_sinh);
      lua_settable(L,i);

      lua_pushstring(L, 'tanh');
      lua_pushcfunction(L, lua_tanh);
      lua_settable(L,i);

      lua_settop(L,i-1);


      {$ifdef darwin}
      autorunpath:=extractfiledir(extractfiledir(Application.ExeName))+'/Lua/Autorun/';
      {$else}
      autorunpath:=CheatEngineDir+'autorun'+pathdelim;
      {$endif}


    finally
      s.free;
    end;
  end;

end;

var
  tm: TThreadManager;
  oldReleaseThreadVars: procedure;




procedure ReleaseLuaThreadVars;
var s: pstring;
begin
  if Thread_LuaVM<>nil then
  begin
    if Thread_LuaRef<>0 then
      lua_unref(Thread_LuaVM, Thread_LuaRef);

    Thread_LuaVM:=nil;
  end;

  if assigned(oldReleaseThreadVars) then
    oldReleaseThreadVars();


end;


initialization
  _LuaCS:=TCriticalSection.create;
  luarefcs:=TCriticalSection.create;



  GetThreadManager(tm);
  oldReleaseThreadVars:=tm.ReleaseThreadVars;
  tm.ReleaseThreadVars:=@ReleaseLuaThreadVars;


  SetThreadManager(tm);


  InitializeLua;



finalization
  if _LuaVM<>nil then
  begin
    lua_close(_LuaVM);
    _LuaVM:=nil;
  end;

  if _LuaCS<>nil then
  begin
    _LuaCS.free;
    _LuaCS:=nil;
  end;

end.


