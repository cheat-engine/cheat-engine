// Copyright Cheat Engine. All Rights Reserved.

unit frmSourceDisplayUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Menus, SynEdit, SynEditMarks, SynHighlighterCpp, disassembler,
  MemoryBrowserFormUnit, tcclib, betterControls, SynGutterBase, debugeventhandler,
  BreakpointTypeDef;

type

  { TfrmSourceDisplay }

  TfrmSourceDisplay = class(TForm)
    itInfo: TIdleTimer;
    ilDebug: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    N1: TMenuItem;
    PopupMenu1: TPopupMenu;
    seSource: TSynEdit;
    SynCppSyn1: TSynCppSyn;
    tbDebug: TToolBar;
    tbRun: TToolButton;
    tbRunTill: TToolButton;
    tbSeparator1: TToolButton;
    tbSeparator2: TToolButton;
    tbSeparator3: TToolButton;
    tbStepInto: TToolButton;
    tbStepOut: TToolButton;
    tbStepOver: TToolButton;
    tbToggleBreakpoint: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure itInfoTimer(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure seSourceGutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    procedure seSourceMouseEnter(Sender: TObject);
    procedure seSourceMouseLeave(Sender: TObject);
    procedure seSourceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tbRunClick(Sender: TObject);
    procedure tbRunTillClick(Sender: TObject);
    procedure tbStepIntoClick(Sender: TObject);
    procedure tbStepOutClick(Sender: TObject);
    procedure tbStepOverClick(Sender: TObject);
    procedure tbToggleBreakpointClick(Sender: TObject);
  private
    hintwindow:THintWindow;
    LoadedPosition: boolean;
    SourceCodeInfo: TSourceCodeInfo;

    d: TDisassembler;
    stepIntoThread: TDebugThreadHandler;
    stepIntoCountdown: integer; //counter to make sure it doesn't step too long (15 instructions max)

    function StepIntoHandler(sender: TDebugThreadHandler; bp: PBreakpoint): boolean;
  public
    function updateMarks:boolean; //returns true if the current breakpoint matches a line in this source
  end;

  function getSourceViewForm(lni: PLineNumberInfo): TfrmSourceDisplay;   //creates or shows an existing sourcedisplay form

  procedure ApplySourceCodeDebugUpdate; //called by memoryview when a breakpoint is triggered and causes a break

implementation

uses maps, debughelper, cedebugger, CEFuncProc, SynHighlighterAA,
  debuggertypedefinitions, sourcecodehandler, ProcessHandlerUnit, byteinterpreter,
  commonTypeDefs, NewKernelHandler;

{ TfrmSourceDisplay }

var sourceDisplayForms: TMap;  //searchable by sourcefile strings object

function getSourceViewForm(lni: PLineNumberInfo): TfrmSourceDisplay;
var
  sc: tstringlist;

begin
  if lni=nil then exit(nil);

  if sourceDisplayForms=nil then
    sourceDisplayForms:=tmap.Create(ituPtrSize,sizeof(TfrmSourceDisplay));

  if sourcedisplayforms.GetData(ptruint(lni^.sourcefile), result)=false then
  begin
    //create a new form
    result:=TfrmSourceDisplay.Create(Application);
    sourceDisplayForms.Add(lni^.sourcefile, result);

    //load the source
    result.seSource.Lines.Assign(lni^.sourcefile);
    result.Tag:=ptruint(lni^.sourcefile);

    result.updateMarks;

    //the sourcefilename is in the first line of sourcecode
    sc:=tstringlist.create;
    sc.text:=lni^.sourcecode;
    result.caption:=sc[0].Split([':'])[0];
    sc.free;
  end;

  result.seSource.CaretY:=lni^.linenr;

  result.SourceCodeInfo:=SourceCodeInfoCollection.getSourceCodeInfo(lni^.functionaddress);

  if result.SourceCodeInfo<>nil then
    result.SourceCodeInfo.parseFullStabData;

end;

procedure ApplySourceCodeDebugUpdate;
var mi: TMapIterator;
  f: TfrmSourceDisplay;
  hasbroken: boolean;
begin
  if sourceDisplayForms<>nil then
  begin
    hasbroken:=(debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil);
    mi:=TMapIterator.Create(sourceDisplayForms);
    try
      while not mi.EOM do
      begin
        mi.GetData(f);

        f.tbDebug.BeginUpdate;
        f.tbRun.enabled:=hasbroken;
        f.tbStepInto.enabled:=hasbroken;
        f.tbStepOver.enabled:=hasbroken;
        f.tbStepOut.enabled:=hasbroken;
        f.tbRunTill.enabled:=hasbroken;
        f.tbDebug.EndUpdate;


        if f.updateMarks then
          f.show;

        mi.Next;
      end;
    finally
      mi.free;
    end;

  end;
end;

function TfrmSourceDisplay.updateMarks: boolean;
var
  mark: TSynEditMark;
  ml: TSynEditMarkLine;
  i: integer;
  a: ptruint;
  hasrip: boolean;
begin
  //mark the lines with addresses and breakpoints
  result:=false;

  for i:=0 to seSource.Lines.Count-1 do
  begin
    a:=ptruint(sesource.Lines.Objects[i]);
    ml:=sesource.Marks.Line[i+1];
    if ml<>nil then
      ml.Clear(true);

    if seSource.Lines.Objects[i]<>nil then
    begin
      mark:=TSynEditMark.Create(seSource);
      mark.line:=i+1;
      mark.ImageList:=ilDebug;

      if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) and (a=debuggerthread.CurrentThread.context^.{$ifdef CPU64}rip{$else}eip{$endif}) then
      begin
        mark.imageindex:=2;
        sesource.CaretY:=i+1;
        hasrip:=true;
        result:=true;
      end
      else
        hasrip:=false;


      if (debuggerthread<>nil) and (debuggerthread.isBreakpoint(a)<>nil) then
      begin
        //2(bp, no rip), or 3(bp, rip)
        if hasrip=false then
          mark.ImageIndex:=2
        else
          mark.ImageIndex:=3;
      end
      else
      begin
        //0(no rip) or 1(rip)
        if hasrip=false then
          mark.ImageIndex:=0
        else
          mark.ImageIndex:=1;
      end;

      mark.Visible:=true;
      seSource.Marks.Add(mark);
    end;
  end;
end;

procedure TfrmSourceDisplay.FormCreate(Sender: TObject);
begin
  SynCppSyn1.loadFromRegistryDefault;
  seSource.Color:=colorset.TextBackground;
  seSource.Font.color:=colorset.FontColor;
  seSource.Gutter.Color:=clBtnFace;
  seSource.Gutter.LineNumberPart.MarkupInfo.Background:=clBtnFace;
  seSource.Gutter.SeparatorPart.MarkupInfo.Background:=clBtnFace;
  seSource.LineHighlightColor.Background:=ColorToRGB(seSource.Color) xor $212121;

  LoadedPosition:=LoadFormPosition(self);

end;

procedure TfrmSourceDisplay.FormDestroy(Sender: TObject);
var
  l: TList;
  i: integer;
begin
  SaveFormPosition(self);

  itInfo.AutoEnabled:=false;
  itInfo.Enabled:=false;
  itinfo.free;
  itinfo:=nil;

  if sourceDisplayForms<>nil then //delete this form from the map
    sourceDisplayForms.Delete(tag);

  if d<>nil then
    freeandnil(d);

  if stepIntoThread<>nil then //it still has an onhandlebreak set
  begin
    //first make sure that the stepIntoThread is still alive
    if debuggerthread<>nil then
    begin
      l:=debuggerthread.lockThreadlist;

      if l.IndexOf(stepIntoThread)<>-1 then //still alive
        stepIntoThread.OnHandleBreakAsync:=nil;

      debuggerthread.unlockThreadlist;
    end;
  end;
end;

procedure TfrmSourceDisplay.FormShow(Sender: TObject);
begin
  if loadedposition=false then
  begin
    width:=(Screen.PixelsPerInch div 96)*800;
    height:=(Screen.PixelsPerInch div 96)*600;
  end;
end;

procedure TfrmSourceDisplay.itInfoTimer(Sender: TObject);
var
  p,p2,p3: tpoint;
  token: string;
  varinfo: TLocalVariableInfo;
  address,ptr: ptruint;

  ln: integer;
  i: integer;

  l1,l2: integer;

  str: string;
  offsettext: string;
  value: string;

  br: ptruint;

  r: trect;

begin
  //get the token under the mouse cursor and look it up
  itInfo.Enabled:=false;
  itInfo.AutoEnabled:=false;

  if hintwindow<>nil then
    hintwindow.hide;

  //if not Active then exit;


  str:='';
  p:=mouse.cursorpos;

  p2:=seSource.ScreenToClient(p);

  if (p2.x<0) or (p2.x>sesource.ClientWidth) or
     (p2.y<0) or (p2.y>sesource.ClientWidth) then exit;

  if p2.x<seSource.Gutter.Width then exit; //gutter stuff

  p3:=seSource.PixelsToLogicalPos(p2);

  token:=sesource.GetWordAtRowCol(p3);
  if trim(token)='' then exit;

  if SourceCodeInfo<>nil then
  begin
    ln:=p3.y-1;

    if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) then
    begin
      address:=debuggerthread.CurrentThread.context^.{$ifdef cpu64}Rip{$else}eip{$endif};
      if SourceCodeInfo.getVariableInfo(token, address, varinfo) then
      begin


        address:=debuggerthread.CurrentThread.context^.{$ifdef cpu64}Rbp{$else}ebp{$endif}+varinfo.offset;
        str:=varinfo.name+' at '+inttohex(address,8);
        if varinfo.ispointer then
        begin
          ptr:=0;
          if ReadProcessMemory(processhandle, pointer(address), @ptr,processhandler.pointersize,br) then
          begin
            address:=ptr;
            str:=str+'->'+inttohex(address,8);
          end
          else
          begin
            address:=0;
            str:=str+'->???';
          end;
        end;

        if address<>0 then
        begin

          value:='';

          case varinfo.vartype of
            1: value:=readAndParseAddress(address, vtDword,nil,false,true); //integer
            2: value:=readAndParseAddress(address,vtByte, nil,false,true); //char
            {$ifdef windows}
            3: value:=readAndParseAddress(address,vtDword, nil,false,true); //long.  4 byte on PE, 8 byte on elf when in 64-bit.
            {$else}
            3: value:=readAndParseAddress(address, ifthen<TVariableType>(processhandler.is64Bit, vtQword, vtDword), nil,false,true);
            {$endif}
            4: value:=readAndParseAddress(address, vtDword,nil,false,false); //unsigned integer
            {$ifdef windows}
            5: value:=readAndParseAddress(address,vtDword, nil,false,false); //long unsigned
            {$else}
            5: value:=readAndParseAddress(address, ifthen<TVariableType>(processhandler.is64Bit, vtQword, vtDword), nil,false,false);
            {$endif}
            6,8: value:=readAndParseAddress(address,vtQword, nil,false,true); //int64
            7,9: value:=readAndParseAddress(address,vtQword, nil,false,false); //uint64
            10: value:=readAndParseAddress(address,vtWord, nil,false,true); //signed word
            11: value:=readAndParseAddress(address,vtWord, nil,false,false); //unsigned word
            12: value:=readAndParseAddress(address,vtByte, nil,false,true); //signed byte
            13: value:=readAndParseAddress(address,vtByte, nil,false,false); //unsigned byte
            14,17: value:=readAndParseAddress(address,vtSingle, nil,false,false); //float
            15,18: value:=readAndParseAddress(address,vtDouble, nil,false,false); //double
            16: value:=readAndParseAddress(address,vtDouble, nil,false,false); //meh

            25: value:=readAndParseAddress(address,vtByte, nil,false,false); //unsigned char
            26: value:=readAndParseAddress(address,vtByte, nil,false,false); //bool
            27: value:='<void>';
          end;

          if value<>'' then
            str:=str+' value='+value;
        end;
      end
      else exit;
    end
    else
    begin
      address:=ptruint(seSource.Lines.Objects[ln]); //get basic info like the offset and type

      if address=0 then //look around
      begin
        i:=1;
        while i<25 do
        begin
          l1:=ln-i;
          l2:=ln+i;

          if (l1>0) and (l1<sesource.lines.count-1) and (ptruint(seSource.Lines.Objects[l1])<>0) then
          begin
            address:=ptruint(seSource.Lines.Objects[l1]);
            break;
          end;

          if (l2>0) and (l2<sesource.lines.count-1) and (ptruint(seSource.Lines.Objects[l2])<>0) then
          begin
            address:=ptruint(seSource.Lines.Objects[l2]);
            break;
          end;

          inc(i);
        end;
      end;

      if address=0 then exit;

      if SourceCodeInfo.getVariableInfo(token, address, varinfo) then
      begin
        //show basic info
        if varinfo.offset<0 then
          offsettext:='-'+IntToHex(-varinfo.offset,2)
        else
          offsettext:='+'+IntToHex(varinfo.offset,2);

        if processhandler.is64Bit then
          offsettext:='RBP'+offsettext
        else
          offsettext:='EBP'+offsettext;

        str:=varinfo.name+' at '+offsettext;
      end else exit;
    end;

    if str<>'' then
    begin
      if hintwindow=nil then
        hintwindow:=THintWindow.Create(self);

      r:=hintwindow.CalcHintRect(sesource.width, str, nil);


      r.Top:=r.top+p.y;
      r.Left:=r.left+p.x;
      r.Right:=r.right+p.x;
      r.Bottom:=r.Bottom+p.y;

      hintwindow.ActivateHint(r, str);
    end;

  end;



end;

procedure TfrmSourceDisplay.MenuItem1Click(Sender: TObject);
begin
  MemoryBrowser.disassemblerview.SelectedAddress:=ptruint(seSource.Lines.Objects[seSource.CaretY-1]);  ;
  MemoryBrowser.show;
end;

procedure TfrmSourceDisplay.seSourceGutterClick(Sender: TObject; X, Y,
  Line: integer; mark: TSynEditMark);
var
  address: ptruint;
  bp: PBreakpoint;
  seml: TSynEditMarkLine;
  i: integer;
begin
  address:=ptruint(seSource.Lines.Objects[line-1]);
  if address=0 then exit;


  seml:=seSource.Marks.Line[line];
  for i:=0 to seml.count-1 do
  begin
    if seml[i].IsBookmark then continue;
    mark:=seml[i];
  end;


  if (mark<>nil) then
  begin
    if mark.ImageIndex in [0,1] then
    begin
      //set breakpoint
      try
        if startdebuggerifneeded(true) then
        begin
          DebuggerThread.SetOnExecuteBreakpoint(address);
          MemoryBrowser.disassemblerview.Update;
          updateMarks;
        end;
      except
        on e:exception do MessageDlg(e.message,mtError,[mbok],0);
      end;

    end
    else
    begin
      //remove breakpoint
      if DebuggerThread<>nil then
      begin
        debuggerthread.lockbplist;
        bp:=DebuggerThread.isBreakpoint(address);
        if bp<>nil then
          debuggerthread.RemoveBreakpoint(bp);
        DebuggerThread.unlockbplist;
      end;

      updateMarks;
    end;
  end;
end;

procedure TfrmSourceDisplay.seSourceMouseEnter(Sender: TObject);
begin
  itInfo.enabled:=false;
  itInfo.AutoEnabled:=true;
end;

procedure TfrmSourceDisplay.seSourceMouseLeave(Sender: TObject);
begin
  itInfo.enabled:=false;
  itInfo.AutoEnabled:=false;
end;

procedure TfrmSourceDisplay.seSourceMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  itInfo.Enabled:=false;
  itInfo.AutoEnabled:=true;

  if hintwindow<>nil then
    hintwindow.hide;
end;

procedure TfrmSourceDisplay.tbRunClick(Sender: TObject);
begin
  MemoryBrowser.miDebugRun.Click;
end;

procedure TfrmSourceDisplay.tbRunTillClick(Sender: TObject);
var address: ptruint;
begin
  address:=ptruint(seSource.Lines.Objects[seSource.CaretY-1]);

  if address=0 then
  begin
    errorbeep;
    exit;
  end;

  if debuggerthread<>nil then
  begin
    debuggerthread.ContinueDebugging(co_runtill, address);
    MemoryBrowser.OnMemoryViewerRunning;
  end;
end;

function TfrmSourceDisplay.StepIntoHandler(sender: TDebugThreadHandler; bp: PBreakpoint): boolean;
var a: ptruint;
  sci: TSourceCodeInfo;
begin
  if stepintocountdown>0 then
  begin
    if bp<>nil then //nil when single stepping
    begin
      //a normal bp got hit, stop the stepping
      sender.OnHandleBreakAsync:=nil;
      stepIntoThread:=nil;
      stepIntoCountdown:=0;
      exit(false);
    end;

    //check if this is a call or ret, if so, set the stepintocountdown to 1 and set stepFinished:=true;
    if d=nil then
      d:=TDisassembler.Create; //even though this is async, it's still only 1 single thread, so this is safe


    a:=sender.context^.{$ifdef cpu64}rip{$else}eip{$endif};
    sci:=SourceCodeInfoCollection.getSourceCodeInfo(a);

    if (sci<>nil) and (sci.getLineInfo(a)<>nil) then
    begin
      //found a sourcecode line
      sender.OnHandleBreakAsync:=nil;
      stepIntoThread:=nil;
      stepIntoCountdown:=0;
      exit(false); //do an actual break
    end;



    d.disassemble(a);
    if d.LastDisassembleData.iscall or d.LastDisassembleData.isret then
    begin
      sender.OnHandleBreakAsync:=nil;
      stepIntoThread:=nil;
      stepIntoCountdown:=0;
      //followed by a single step
    end;

    dec(stepIntoCountdown);
    sender.continueDebugging(co_stepinto);
    exit(true);// handled
  end
  else
  begin
    sender.OnHandleBreakAsync:=nil;
    stepIntoThread:=nil;
    stepIntoCountdown:=0;
    sender.continueDebugging(co_run); //screw this , abandoning this stepping session
    exit(true);
  end;
end;

procedure TfrmSourceDisplay.tbStepIntoClick(Sender: TObject);
begin

  stepIntoCountdown:=15;
  stepIntoThread:=debuggerthread.CurrentThread;

  debuggerthread.CurrentThread.OnHandleBreakAsync:=@StepIntoHandler;


  debuggerthread.ContinueDebugging(co_stepinto);
  memorybrowser.OnMemoryViewerRunning;
end;

procedure TfrmSourceDisplay.tbStepOutClick(Sender: TObject);
begin
  memorybrowser.miDebugExecuteTillReturn.Click;
end;

procedure TfrmSourceDisplay.tbStepOverClick(Sender: TObject);
var
  i,linestart: integer;
  currentaddress,nextaddress: ptruint;
  sci: TSourceCodeInfo;
  lni, lni2: PLineNumberInfo;

begin
  if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) then
  begin
    currentaddress:=debuggerthread.CurrentThread.context^.{$ifdef CPU64}rip{$else}eip{$endif};
    sci:=SourceCodeInfoCollection.getSourceCodeInfo(currentaddress);

    if sci<>nil then
    begin
      lni:=sci.getLineInfo(currentaddress);
      if lni<>nil then
      begin
        linestart:=lni^.sourcefile.IndexOfObject(tobject(currentaddress));
        if linestart=-1 then exit; //never

        nextaddress:=0;
        for i:=linestart+1 to lni^.sourcefile.Count-1 do
        begin
          nextaddress:=ptruint(sesource.lines.Objects[i]);
          if nextaddress<>0 then
          begin
            lni2:=sci.getLineInfo(nextaddress);
            if lni2<>nil then //never
            begin
              if lni^.functionaddress<>lni2^.functionaddress then break; //different function.  So return

              debuggerthread.ContinueDebugging(co_runtill, nextaddress);
              memorybrowser.OnMemoryViewerRunning;
              exit;
            end;

            //still here, so not possible to do a step over
            break;
          end;

        end;
      end;

      //still here, so no next address, do a step
      tbStepIntoClick(tbStepInto);

    end;
  end;
end;

procedure TfrmSourceDisplay.tbToggleBreakpointClick(Sender: TObject);
begin
  seSourceGutterClick(seSource,0,0,seSource.CaretY,nil);
end;




initialization
  {$I frmsourcedisplayunit.lrs}

end.

