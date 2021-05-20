unit frmSourceDisplayUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, SynEdit, SynEditMarks, MemoryBrowserFormUnit, tcclib, betterControls;

type

  { TfrmSourceDisplay }

  TfrmSourceDisplay = class(TForm)
    ilDebug: TImageList;
    seSource: TSynEdit;
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
  private
    LoadedPosition: boolean;

  public
    function updateMarks:boolean; //returns true if the current breakpoint matches a line in this source
  end;

  function getSourceViewForm(lni: PLineNumberInfo): TfrmSourceDisplay;   //creates or shows an existing sourcedisplay form

  procedure ApplySourceCodeDebugUpdate; //called by memoryview when a breakpoint is triggered and causes a break

implementation

uses maps, debughelper, CEFuncProc;

{ TfrmSourceDisplay }

var sourceDisplayForms: TMap;  //searchable by sourcefile strings object

function getSourceViewForm(lni: PLineNumberInfo): TfrmSourceDisplay;
var sc: tstringlist;
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
end;

procedure ApplySourceCodeDebugUpdate;
var mi: TMapIterator;
  f: TfrmSourceDisplay;
begin
  if sourceDisplayForms<>nil then
  begin
    mi:=TMapIterator.Create(sourceDisplayForms);
    try
      while not mi.EOM do
      begin
        mi.GetData(f);

        if f.updateMarks then
          f.show;
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
begin
  //mark the lines with addresses and breakpoints
  result:=false;

  for i:=0 to seSource.Lines.Count-1 do
  begin
    ml:=sesource.Marks.Line[i+1];
    if ml<>nil then
      ml.Clear(true);

    if seSource.Lines.Objects[i]<>nil then
    begin
      mark:=TSynEditMark.Create(seSource);
      mark.line:=i+1;
      mark.ImageList:=ilDebug;

      a:= ptruint(seSource.Lines.Objects[i]) ;
      if (debuggerthread<>nil) and (debuggerthread.isBreakpoint(a)<>nil) then
        mark.ImageIndex:=1
      else
        mark.ImageIndex:=0;


      if (debuggerthread<>nil) and (debuggerthread.CurrentThread<>nil) and (a=debuggerthread.CurrentThread.context^.{$ifdef CPU64}rip{$else}eip{$endif}) then
      begin
        mark.imageindex:=2;
        sesource.CaretY:=i+1;
        result:=true;
      end;

      mark.Visible:=true;
      seSource.Marks.Add(mark);
    end;
  end;
end;

procedure TfrmSourceDisplay.FormCreate(Sender: TObject);
begin
  seSource.Color:=colorset.TextBackground;
  seSource.Font.color:=colorset.FontColor;
  seSource.Gutter.Color:=clBtnFace;
  seSource.Gutter.LineNumberPart.MarkupInfo.Background:=clBtnFace;
  seSource.Gutter.SeparatorPart.MarkupInfo.Background:=clBtnFace;
  seSource.LineHighlightColor.Background:=ColorToRGB(seSource.Color) xor $212121;

  LoadedPosition:=LoadFormPosition(self);

end;

procedure TfrmSourceDisplay.FormDestroy(Sender: TObject);

begin
  SaveFormPosition(self);

  if sourceDisplayForms<>nil then //delete this form from the map
    sourceDisplayForms.Delete(tag);
end;

procedure TfrmSourceDisplay.FormShow(Sender: TObject);
begin
  if loadedposition=false then
  begin
    width:=(Screen.PixelsPerInch div 96)*800;
    height:=(Screen.PixelsPerInch div 96)*600;
  end;
end;




initialization
  {$I frmsourcedisplayunit.lrs}

end.

