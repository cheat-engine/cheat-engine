unit frmLuaEngineUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Menus, ExtCtrls, SynMemo, SynCompletion, SynEdit, lua,
  lauxlib, lualib, LuaSyntax, luahandler, cefuncproc, strutils, InterfaceBase,
  ComCtrls, SynGutterBase, SynEditMarks, PopupNotifier, SynEditHighlighter,
  AvgLvlTree;

type

  { TfrmLuaEngine }

  TfrmLuaEngine = class(TForm)
    btnExecute: TButton;
    GroupBox1: TGroupBox;
    tShowHint: TIdleTimer;
    ilLuaDebug: TImageList;
    ilSyneditDebug: TImageList;
    MainMenu1: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miView: TMenuItem;
    cbShowOnPrint: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    mOutput: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mScript: TSynEdit;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pmEditor: TPopupMenu;
    dlgReplace: TReplaceDialog;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    tbDebug: TToolBar;
    tbRun: TToolButton;
    tbSingleStep: TToolButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure dlgReplaceFind(Sender: TObject);
    procedure dlgReplaceReplace(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure mScriptChange(Sender: TObject);
    procedure mScriptGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure mScriptKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure mScriptMouseEnter(Sender: TObject);
    procedure mScriptMouseLeave(Sender: TObject);
    procedure mScriptMouseLink(Sender: TObject; X, Y: Integer;
      var AllowMouseLink: Boolean);
    procedure mScriptMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mScriptShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure Panel2Resize(Sender: TObject);
    procedure tbRunClick(Sender: TObject);
    procedure tbSingleStepClick(Sender: TObject);
    procedure tShowHintTimer(Sender: TObject);
  private
    { private declarations }
    hintwindow:THintWindow;
    continue: integer;
  public
    { public declarations }
    synhighlighter: TSynLuaSyn;
  end; 

var
  frmLuaEngine: TfrmLuaEngine;

implementation

{ TfrmLuaEngine }

uses luaclass;

resourcestring
  rsError = 'Script Error';

var
  LuaDebugForm: TfrmLuaEngine;
  LuaDebugSingleStepping: boolean;
  LuaDebugInfo: Plua_Debug;
  LuaDebugVariables: TStringToStringTree;


procedure TfrmLuaEngine.Panel2Resize(Sender: TObject);
begin
  btnexecute.Height:=panel2.clientheight-(2*btnexecute.top);
end;

procedure TfrmLuaEngine.tbRunClick(Sender: TObject);
begin
  continue:=1;
  tbDebug.enabled:=false;
  tbRun.enabled:=false;
  tbSingleStep.enabled:=false;
end;

procedure TfrmLuaEngine.tbSingleStepClick(Sender: TObject);
begin
  continue:=2;
  tbDebug.enabled:=false;
  tbRun.enabled:=false;
  tbSingleStep.enabled:=false;
end;

procedure TfrmLuaEngine.tShowHintTimer(Sender: TObject);
var r: trect;
  description: string;
  p,p2,p3: tpoint;
  token: string;
  attr: TSynHighlighterAttributes;
  o: TObject;
begin
  if LuaDebugForm=self then
  begin
    //figure out what is currently focused by the mouse
    p:=mouse.cursorpos;

    p2:=mScript.ScreenToClient(p);
    if p2.x<mscript.Gutter.Width then exit; //gutter stuff

    p3:=mscript.PixelsToLogicalPos(p2);

    mscript.GetHighlighterAttriAtRowCol(p3, token, attr);

    if (attr=synhighlighter.IdentifierAttri) or (attr=synhighlighter.KeyAttri) then
    begin

      token:=mscript.GetWordAtRowCol(p3);
      description:=LuaDebugVariables[token];


      if description='' then //check if it's a global
      begin
        //look up
        LuaCS.Enter;
        try
          lua_getfield(LuaVM, LUA_GLOBALSINDEX, pchar(token));
          description:=LuaValueToDescription(LuaVM, -1);

          lua_pop(luavm, 1);
        finally
          luacs.Leave;
        end;

        if description='' then
        begin
          description:=' ';
          LuaDebugVariables.Add(token, description); //could not be found
        end;
      end;



      if description=' ' then
      begin
        //means nothing was found
        exit;
      end;



      if hintwindow=nil then
        hintwindow:=THintWindow.Create(self);

      r:=hintwindow.CalcHintRect(mscript.width,description, nil);


      r.Top:=r.top+p.y;
      r.Left:=r.left+p.x;
      r.Right:=r.right+p.x;
      r.Bottom:=r.Bottom+p.y;

      hintwindow.ActivateHint(r, description);
    end;
  end;

//  mScript.ShowHint:=;
end;

function onerror(L: PLua_State): integer; cdecl;
var ld: lua_Debug;
  frm: TfrmLuaEngine;
  r: integer;
  t: integer;
begin
  //todo: Try to get this to work (might be a lua bug)
  {

  result:=0;
  frm:=luaclass_getClassObject(L);


  t:=lua_gettop(L);

  ZeroMemory(@ld, sizeof(ld));


  r:=lua_getstack(L, 1, @ld);
  if r=1 then
  begin
    lua_getinfo(L, '>l', @ld);
    lua_pushstring(L, 'Error in line '+inttostr(ld.currentline));
  end
  else
    lua_pushstring(L, 'Undefined error');
             }
  result:=1;


end;

function hasLuaBreakpoint(linenumber: integer): boolean;
begin

  result:=LuaDebugSingleStepping or (LuaDebugForm.mScript.Marks.Line[linenumber]<>nil);
end;

procedure LineHook(L: Plua_State; ar: Plua_Debug); cdecl;
var i,j: integer;
  s,s2: integer;
  disabled: tlist;
  mark: TSynEditMark;

  name: pchar;
  value: string;
begin

  if lua_getinfo(L,'nSl', ar)<>0 then
  begin
    if (ar.what='main') and (hasLuaBreakpoint(ar.currentline)) then
    begin
      //break
     // frmLuaEngine.visible:=false;
     // frmLuaEngine.ShowModal;
     // frmLuaEngine.show;

      LuaDebugForm.show;
      LuaDebugForm.SetFocus;


      if LuaDebugForm.mScript.Marks.Line[ar.currentline]<>nil then
      begin
        //update the icon for the current line
        if LuaDebugForm.mScript.Marks.Line[ar.currentline][0].ImageIndex = 0 then
          LuaDebugForm.mScript.Marks.Line[ar.currentline][0].ImageIndex:=2;
      end
      else
      begin
        mark:=TSynEditMark.Create(LuaDebugForm.mscript);
        mark.line:=ar.currentline;
        mark.ImageList:=LuaDebugForm.ilSyneditDebug;
        mark.ImageIndex:=1;
        mark.Visible:=true;
        LuaDebugForm.mscript.Marks.Add(mark);
      end;

      //This somehow doesn't work:
      //disabled:=screen.DisableForms(LuaDebugForm);
      //so do it manually:



      {disabled:=tlist.create;
      for i:=0 to screen.CustomFormCount-1 do
      begin
        if (screen.CustomForms[i]<>LuaDebugForm) and screen.CustomForms[i].visible and screen.CustomForms[i].enabled then
        begin
          disabled.add(screen.CustomForms[i]);
          EnableWindow(screen.CustomForms[i].handle,false);
        end;

      end;  }

      LuaDebugForm.show;
      //activate the debug gui
      LuaDebugForm.tbDebug.Visible:=true;
      LuaDebugForm.tbDebug.enabled:=true;
      LuaDebugForm.tbRun.enabled:=true;
      LuaDebugForm.tbSingleStep.enabled:=true;
      LuaDebugForm.mScript.ReadOnly:=true;

      LuaDebugForm.continue:=0;

      LuaDebugInfo:=ar;
      LuaDebugVariables:=TStringToStringTree.Create(true);

      i:=1;

      repeat
        name:=lua_getlocal(L, ar, i);
        if name<>nil then
        begin
          value:=Lua_ToString(L, -1);
          lua_pop(L, 1);
          LuaDebugVariables.Add(name, value);

          inc(i);
        end;

      until name=nil;




      while LuaDebugForm.continue=0 do
      begin
        application.ProcessMessages;

        if application.Terminated or (LuaDebugForm.Visible=false) then break;
        application.Idle(true);
      end;

      LuaDebugForm.mScript.ReadOnly:=false;


    {  for i:=0 to disabled.Count-1 do
      begin
        EnableWindow(tcustomform(disabled[i]).handle,true);
        //tcustomform(disabled[i]).Enabled:=true;
      end;}


      //clear the current instruction pointer
      if LuaDebugForm.mScript.Marks.Line[ar.currentline]<>nil then
      begin
        if LuaDebugForm.mScript.Marks.Line[ar.currentline][0].ImageIndex = 2 then  //bp with the current bp set
          LuaDebugForm.mScript.Marks.Line[ar.currentline][0].ImageIndex:=0  //set back to normal bp
        else
          LuaDebugForm.mScript.Marks.Line[ar.currentline][0].Free; //clear bp


      end;

     { disabled.free;   }

      LuaDebugSingleStepping:=false;

      case LuaDebugForm.continue of
        1: ;//continue (normal bp's only)
        2: LuaDebugSingleStepping:=true;  //single step next instruction
      end;


    end;
//    frmLuaEngine.moutput.lines.add('called:'+ar.what+' ('+inttostr(ar.currentline)+')');

  end;


end;

procedure TfrmLuaEngine.btnExecuteClick(Sender: TObject);
var pc: pchar;
  i,j: integer;

  oldprintoutput: Tstrings;
  c: tobject;

  err: integer;

  oldstack: integer;
  dodebug: boolean;
begin
  dodebug:=false;

  for i:=0 to mScript.Marks.Count-1 do
    if mscript.Marks[i].ImageIndex=0 then
    begin
      dodebug:=true;
      break;
    end;


  if dodebug then
  begin

    if LuaDebugForm=nil then
    begin
      for i:=0 to mScript.Marks.Count-1 do
        if mscript.Marks[i].ImageIndex=0 then
        begin
          //this script wishes to get debugged
          dodebug:=true;

          LuaDebugForm:=self;
          LuaDebugSingleStepping:=false;

          LuaDebugForm.btnExecute.enabled:=false;

          mscript.OnMouseEnter:=mScriptMouseEnter;
          mscript.OnMouseLeave:=mScriptMouseLeave;

          if LuaDebugVariables<>nil then
            LuaDebugVariables.Clear;

          break;
        end;

    end
    else
    begin
      dodebug:=false;
      if MessageDlg('Only one script can be debugged at a time. Continue executing this script without the debugger?', mtConfirmation, [mbyes, mbno], 0)<>mryes then exit;
    end;
  end;

  luacs.Enter;

  oldstack:=lua_gettop(luavm);

  oldprintoutput:=lua_oldprintoutput;
  try
    mOutput.lines.add(mscript.text);


    lua_setPrintOutput(mOutput.lines);

    i:=0;
{    luaclass_newClass(Luavm, self);
    lua_pushcclosure(Luavm, onerror,1);
    err:=lua_gettop(Luavm);

    if luaL_loadstring(Luavm, pchar(mScript.text))=0 then
      i := lua_pcall(Luavm, 0, LUA_MULTRET, err);

     lua_remove(luavm, err); }

    if dodebug then
      lua_sethook(luavm, linehook, LUA_MASKLINE, 0);

    if lua_dostring(Luavm, pchar(mScript.text))=0 then
    begin


      j:=lua_gettop(luavm);
      if j>oldstack then
      begin
        for i:=oldstack+1 to j do
        begin

          mOutput.lines.add(':'+LuaValueToDescription(luavm, i));
                           {
          pc:=lua_tolstring(luavm, i,nil);
          if pc<>nil then
            mOutput.lines.add(':'+pc)
          else
          begin
            if lua_islightuserdata(luavm,i) then //shouldn't occur anymore
              moutput.lines.add(':'+p->'+inttohex(ptruint(lua_touserdata(luavm,i)),1))
            else
            if lua_isboolean(luavm,i) then
              moutput.lines.add(':(boolean)'+BoolToStr(lua_toboolean(Luavm, i),'true','false'))
            else
            if lua_isnil(luavm,i) then
              moutput.lines.add(':'+'nil')
            else
            if lua_istable(luavm, i) then
              moutput.lines.add(':'+'table')
            else
            if lua_isfunction(luavm,i) then
              moutput.lines.add(':'+'function')
            else
            if lua_isuserdata(luavm,i) then
            begin
              try
                c:=lua_ToCEUserData(luavm, i);
                moutput.lines.add(':'+'class object ('+c.ClassName+')')
              except
                moutput.lines.add(':'+'class object (corrupt)')
              end;
            end
            else
              moutput.lines.add(':'+'unknown')

          end;}
        end;


      end;
    end
    else
    begin
      i:=lua_gettop(luavm);
      if i>oldstack then
      begin

        //is currently shown inside the pcall function
        pc:=lua_tolstring(luavm, -1,nil);
        if pc<>nil then
          mOutput.lines.add(rsError+':'+pc)
        else
          moutput.lines.add(rsError+':'+'nil');
      end else moutput.lines.add(rsError);

    end;
  finally

    if dodebug then
    begin
      LuaDebugForm.btnExecute.enabled:=true;
      lua_sethook(luavm, linehook, 0, 0);
      LuaDebugForm:=nil;

      mscript.OnMouseEnter:=nil;
      mscript.OnMouseLeave:=nil;

      if hintwindow<>nil then
        hintwindow.hide;


    end;

    lua_settop(luavm, oldstack);

    lua_setPrintOutput(oldprintoutput);
    luacs.Leave;



  end;
end;

procedure TfrmLuaEngine.dlgReplaceFind(Sender: TObject);
var
  s: string;
  i: integer;
begin
  //find
  s:=dlgReplace.FindText;

  i:=PosEx(s, mscript.Text, mscript.selstart+1);

  if i>0 then
  begin
    mScript.SelStart:=i;
    mscript.SelEnd:=i+length(s);
  end
  else
    beep;//weeeeee
end;

procedure TfrmLuaEngine.dlgReplaceReplace(Sender: TObject);
var oldselstart: integer;
begin
  //replace
  repeat
    oldselstart:=mScript.SelStart;
    dlgReplaceFind(sender);
    if oldselstart=mScript.SelStart then break;  //nothing found


    if mscript.SelEnd>mscript.SelStart then
    begin
      oldselstart:=mScript.SelStart;
      mScript.SelText:=dlgReplace.ReplaceText;
      mscript.selstart:=oldselstart;
      mscript.SelEnd:=oldselstart+length(dlgreplace.replacetext);
    end
    else
      break;
  until (frReplaceAll in dlgReplace.Options=false);

end;

procedure TfrmLuaEngine.FormCreate(Sender: TObject);
var x: array of integer;
begin
  synhighlighter:=TSynLuaSyn.Create(self);
  mscript.Highlighter:=synhighlighter;

  setlength(x,1);
  if LoadFormPosition(self, x) then
    panel1.height:=x[0];
end;

procedure TfrmLuaEngine.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self, [panel1.height]);
end;


procedure TfrmLuaEngine.MenuItem10Click(Sender: TObject);
begin
  mscript.Undo;
end;

procedure TfrmLuaEngine.MenuItem11Click(Sender: TObject);
var f: TfrmLuaEngine;
begin
  f:=TfrmLuaEngine.create(application);
  f.miView.visible:=false;

  f.show;
end;

procedure TfrmLuaEngine.MenuItem2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    mscript.Lines.LoadFromFile(opendialog1.filename);

end;

procedure TfrmLuaEngine.MenuItem3Click(Sender: TObject);
begin
  if savedialog1.execute then
    mscript.lines.SaveToFile(savedialog1.filename);
end;

procedure TfrmLuaEngine.MenuItem5Click(Sender: TObject);
begin
  moutput.Clear;
end;

procedure TfrmLuaEngine.MenuItem6Click(Sender: TObject);
begin
  dlgReplace.Execute;
end;

procedure TfrmLuaEngine.MenuItem7Click(Sender: TObject);
begin
  mscript.CutToClipboard;
end;

procedure TfrmLuaEngine.MenuItem8Click(Sender: TObject);
begin
  mscript.CopyToClipboard;
end;

procedure TfrmLuaEngine.MenuItem9Click(Sender: TObject);
begin
  mscript.PasteFromClipboard;
end;

procedure TfrmLuaEngine.mScriptChange(Sender: TObject);
begin

end;

procedure TfrmLuaEngine.mScriptGutterClick(Sender: TObject; X, Y,
  Line: integer; mark: TSynEditMark);
var
  ml: TSynEditMarkLine;
  i: integer;
  hasbp:boolean;
begin
  hasbp:=false;
  ml:=mscript.Marks.Line[line];

  if ml<>nil then
  begin
    for i:=0 to ml.Count-1 do
    begin
      if ml[i].ImageIndex=0 then
        hasbp:=true;
    end;

    if hasbp then
    begin
      //clear it
      i:=0;
      while i<ml.count do
      begin
        if ml[i].imageindex in [0,2] then //bp set or bp set and current line
        begin
          ml[i].Free;
          ml:=mscript.Marks.Line[line];
          if ml=nil then exit;
        end
        else
          inc(i);
      end;
    end;
  end;

  if not hasbp then
  begin
    //set it
    mark:=TSynEditMark.Create(mscript);
    mark.line:=line;
    mark.ImageList:=ilSyneditDebug;
    mark.ImageIndex:=0;
    mark.Visible:=true;
    mscript.Marks.Add(mark);
  end;
end;

procedure TfrmLuaEngine.mScriptKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  if (ssCtrl in shift) and (key=vk_return) then
  begin
    btnExecute.click;
    mScript.ClearAll;
  end;
end;

procedure TfrmLuaEngine.mScriptMouseEnter(Sender: TObject);
begin
  tShowHint.enabled:=false;
  tShowHint.AutoEnabled:=true;
end;

procedure TfrmLuaEngine.mScriptMouseLeave(Sender: TObject);
begin
  tShowHint.AutoEnabled:=false;
  tShowHint.enabled:=false;
end;

procedure TfrmLuaEngine.mScriptMouseLink(Sender: TObject; X, Y: Integer;
  var AllowMouseLink: Boolean);
begin
  AllowMouseLink:=false;
end;

procedure TfrmLuaEngine.mScriptMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if hintwindow<>nil then
    hintwindow.hide;
end;

procedure TfrmLuaEngine.mScriptShowHint(Sender: TObject; HintInfo: PHintInfo);
begin

end;


initialization
  {$I frmluaengineunit.lrs}

end.

