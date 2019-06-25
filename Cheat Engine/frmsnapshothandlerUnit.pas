unit frmsnapshothandlerUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, math, NewKernelHandler, FPImage, FPCanvas, FPImgCanv, FPReadPNG, FPWritePNG;

resourcestring
  rsSHView = 'View';
  rsSHDissectMemoryOfSelectedSnapshot = 'Dissect memory of selected snapshot';
  rsSHDissectAndCompareMemoryOfSelectedSnapshots = 'Dissect and compare memory of selected snapshots';
  rsSHCompare = 'Compare';
  rsSHFunctions = ' Function(s): ';
  rsSHTheStructuresListIsBroken = 'The structures list is broken';
  rsSHNewWindow = '<New window>';
  rsSHLockAndAddToStructureDissect = 'Lock and add to structure dissect';
  rsSHSelectTheStructureDissectWindowYouWishToAddThisRegionTo = 'Select the structure dissect window you wish to add this region to';

type

  { TfrmSnapshotHandler }

  //let's experiment with generics this time
  TSnapshot=class(tobject)
  private
  public
    filename: string;

    dxversion: integer;
    picturesize: integer;
    pic: TBitmap;

    stackbase: qword;
    stacksize: integer;
    stack: PByteArray;

    constantbuffersize: integer;
    constantbuffer: PByteArray;

    functionnamesize: integer;
    functionname: pchar;

    //graphical:
    selected: boolean;

    xpos: integer;
    width: integer;

    destructor destroy; override;
  end;

  TSnapshotList =  Tlist;


  TfrmSnapshotHandler = class(TForm)
    btnCompare: TButton;
    shImageList: TImageList;
    lblCompare: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miConfig: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    rbStack: TRadioButton;
    rbCB: TRadioButton;
    ScrollBar1: TScrollBar;
    procedure btnCompareClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure miConfigClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { private declarations }
    snapshots: TSnapshotList;
  public
    { public declarations }
    procedure loadsnapshots(list: TStrings);
    procedure clearlist;
  end;

var
  frmSnapshotHandler: TfrmSnapshotHandler;

implementation

{$R *.lfm}

uses mainunit, frmSaveSnapshotsUnit, d3dhookUnit, frmD3DHookSnapshotConfigUnit,
  StructuresFrm2, frmSelectionlistunit, frmStackViewUnit;

destructor TSnapshot.destroy;
begin
  if pic<>nil then
    freeandnil(pic);

  if constantbuffer<>nil then
    FreeMemAndNil(constantbuffer);

  if stack<>nil then
    FreeMemAndNil(stack);


  if functionname<>nil then
    FreeMemAndNil(functionname);

  inherited destroy;
end;

procedure TfrmSnapshotHandler.clearlist;
var i: integer;
begin
  for i:=0 to snapshots.count-1 do
    TSnapshot(snapshots[i]).free;

  snapshots.Clear;

  paintbox1.Repaint;

end;

procedure TfrmSnapshotHandler.loadsnapshots(list: TStrings);
var
  s: TSnapshot;
  i: integer;

  f: TFileStream;
  posAfterpicture: integer;
  fpi: TFPMemoryImage;
  fpr: TFPReaderPNG;
  fpw: TFPWriterPNG;


  c: TFPCustomCanvas;
  pictureformat: integer;
begin
  for i:=0 to list.count-1 do
  begin
    s:=TSnapshot.create;
    s.filename:=list[i];

    f:=tfilestream.Create(s.filename, fmOpenRead);
    try
      f.readbuffer(s.dxversion, sizeof(s.dxversion));
      f.readbuffer(pictureformat, sizeof(pictureformat));
      f.ReadBuffer(s.picturesize, sizeof(s.picturesize));
      posAfterpicture:=f.position+s.picturesize;

      s.pic:=tbitmap.create;

      if pictureformat=0 then
      begin
        s.pic.LoadFromStream(f, s.picturesize);
      end
      else
      if pictureformat=3 then
      begin
        fpi:=TFPMemoryImage.Create(0,0);
        fpr:=TFPReaderPNG.create;
        fpi.LoadFromStream(f, fpr);


        c:=TFPImageCanvas.create(fpi);

        s.pic.Width:=fpi.Width;
        s.pic.Height:=fpi.Height;
        TFPCustomCanvas(s.pic.Canvas).CopyRect(0,0, c, rect(0,0,fpi.width, fpi.height));

        c.free;
        fpr.free;
        fpi.free;

      end;


      f.position:=posAfterpicture;


      f.readbuffer(s.stackbase, sizeof(s.stackbase));
      f.readbuffer(s.stacksize, sizeof(s.stacksize));

      if s.stacksize>0 then
      begin
        getmem(s.stack, s.stacksize);
        f.readbuffer(s.stack^, s.stacksize);
      end
      else
        s.stack:=nil;


      f.readbuffer(s.constantbuffersize, sizeof(s.constantbuffersize));
      if s.constantbuffersize>0 then
      begin
        getmem(s.constantbuffer, s.constantbuffersize);
        f.ReadBuffer(s.constantbuffer^, s.constantbuffersize);
      end
      else
        s.constantbuffer:=nil;

      f.readbuffer(s.functionnamesize, sizeof(s.functionnamesize));
      if s.functionnamesize>0 then
      begin
        getmem(s.functionname, s.functionnamesize+1);
        f.ReadBuffer(s.functionname^, s.functionnamesize);
        s.functionname[s.functionnamesize]:=#0;
      end
      else
        s.functionname:=nil;




    finally
      f.free;
    end;

    snapshots.add(s);
  end;

  scrollbar1.Max:=snapshots.count;

  paintbox1.Repaint;

end;

procedure TfrmSnapshotHandler.ScrollBar1Change(Sender: TObject);
begin

end;

procedure TfrmSnapshotHandler.MenuItem4Click(Sender: TObject);
begin
  if opendialog1.Execute then
    loadsnapshots(opendialog1.Files);
end;

procedure TfrmSnapshotHandler.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
  selcount: integer;
  functionnames: tstringlist;
  fn: string;
begin
  selcount:=0;

  functionnames:=tstringlist.create;
  functionnames.Duplicates:=dupIgnore;

  for i:=scrollbar1.Position to snapshots.count-1 do
    if InRange(x, TSnapshot(snapshots[i]).xpos, TSnapshot(snapshots[i]).xpos+TSnapshot(snapshots[i]).width) then
      TSnapshot(snapshots[i]).selected:=not TSnapshot(snapshots[i]).selected;


  for i:=0 to snapshots.count-1 do
  begin
    if TSnapshot(snapshots[i]).selected then
    begin
      if TSnapshot(snapshots[i]).functionname<>nil then
      begin
        if functionnames.IndexOf(TSnapshot(snapshots[i]).functionname)=-1 then //for some reason dupIgnore isn't used
          functionnames.Add(TSnapshot(snapshots[i]).functionname);
      end;

      inc(selcount);
    end;
  end;


  PaintBox1.repaint;

  lblCompare.Enabled:=selcount>0;
  rbStack.enabled:=selcount>0;
  rbCB.enabled:=selcount>0;
  btnCompare.enabled:=selcount>0;

  if selcount=0 then
  begin
    lblCompare.caption:='';
    btnCompare.caption:=rsSHView;
  end
  else
  if selcount=1 then
  begin
    lblCompare.caption:=rsSHDissectMemoryOfSelectedSnapshot;
    btnCompare.caption:=rsSHView;
  end
  else
  if selcount>1 then
  begin
    lblCompare.caption:=rsSHDissectAndCompareMemoryOfSelectedSnapshots;
    btnCompare.caption:=rsSHCompare;

  end;

  fn:='';
  for i:=0 to functionnames.count-1 do
    fn :=fn+functionnames[i]+' ';


  lblCompare.caption:=lblCompare.Caption+rsSHFunctions+fn;


  functionnames.free;
end;

procedure TfrmSnapshotHandler.PaintBox1Paint(Sender: TObject);
var
  i: integer;
  xpos: integer;
  aspectratio: single;
  currentw: integer;
  h: integer;
begin
  paintbox1.canvas.Clear;
  xpos:=0;

  h:=paintbox1.Height;

  for i:=0 to snapshots.count-1 do
  begin
    aspectratio:=TSnapshot(snapshots[i]).pic.Width/TSnapshot(snapshots[i]).pic.Height;

    currentw:=ceil(h*aspectratio);
    paintbox1.Canvas.CopyRect(rect(xpos, 0, xpos+currentw, h), TSnapshot(snapshots[i]).pic.Canvas, rect(0,0,TSnapshot(snapshots[i]).pic.width, TSnapshot(snapshots[i]).pic.height));

   TSnapshot(snapshots[i]).xpos:=xpos;
    TSnapshot(snapshots[i]).width:=currentw;

    if TSnapshot(snapshots[i]).selected then
    begin
      paintbox1.Canvas.Pen.Width:=3;
      paintbox1.canvas.pen.Color:=clAqua;
      paintbox1.canvas.Brush.Style:=bsClear;
      paintbox1.Canvas.Rectangle(rect(xpos, 0, xpos+currentw, h));
      paintbox1.canvas.Brush.Style:=bsSolid;
    end;

    inc(xpos, currentw+1);
    if xpos>paintbox1.Width then
      exit; //done

  end;
end;

procedure TfrmSnapshotHandler.Panel1Click(Sender: TObject);
begin

end;

procedure TfrmSnapshotHandler.FormCreate(Sender: TObject);
begin
  snapshots:=TSnapshotList.create;
  panel2.DoubleBuffered:=true;
  DoubleBuffered:=true;
end;

procedure TfrmSnapshotHandler.MenuItem8Click(Sender: TObject);
begin
  clearList;
end;

procedure TfrmSnapshotHandler.btnCompareClick(Sender: TObject);
var
  i: integer;
  s: tstringlist;
  f: TfrmSelectionList;

  structurefrm: TfrmStructures2;
  new: boolean;
  size: integer;
  hasselection: boolean;
begin
  hasselection:=false;
  for i:=0 to snapshots.count-1 do
    if TSnapshot(snapshots[i]).selected then
    begin
      hasSelection:=true;
      break;
    end;

  if hasselection then
  begin


    //find out which data dissect windows are open
    s:=tstringlist.create;

    if frmStructures2=nil then
      raise exception.create(rsSHTheStructuresListIsBroken);

    for i:=0 to frmStructures2.Count-1 do
      s.add(TfrmStructures2(frmStructures2[i]).Caption);

    s.add(rsSHNewWindow);

    f:=TfrmSelectionList.Create(self, s);

    f.caption:=rsSHLockAndAddToStructureDissect;
    f.label1.Caption:=rsSHSelectTheStructureDissectWindowYouWishToAddThisRegionTo;

    if f.showmodal=mrok then
    begin
      if f.itemindex=-1 then f.itemindex:=0;

      if f.itemindex>=frmStructures2.Count then       //new window
      begin
        structurefrm:=tfrmstructures2.create(application);
        structurefrm.show;
      end
      else
        structurefrm:=TfrmStructures2(frmStructures2[f.itemindex]);

      //add this as a locked address
      size:=0;

      for i:=0 to snapshots.count-1 do
        if TSnapshot(snapshots[i]).selected then
        begin

          if rbstack.checked then
          begin
            structurefrm.addLockedAddress(TSnapshot(snapshots[i]).stackbase, TSnapshot(snapshots[i]).stack, TSnapshot(snapshots[i]).stacksize);
            size:=max(TSnapshot(snapshots[i]).stacksize, size);
          end
          else
          begin
            structurefrm.addLockedAddress(0, TSnapshot(snapshots[i]).constantbuffer, TSnapshot(snapshots[i]).constantbuffersize);
            size:=max(TSnapshot(snapshots[i]).constantbuffersize, size);
          end;

        end;

      structurefrm.show;

      if structurefrm.mainStruct=nil then //if no structure is selected define it then
        structurefrm.DefineNewStructure(size);

    end;

  end;
end;



procedure TfrmSnapshotHandler.miConfigClick(Sender: TObject);
var frmD3DHookSnapshotConfig: TfrmD3DHookSnapshotConfig;
    pf: integer;
begin
  frmd3dhooksnapshotconfig:=TfrmD3DHookSnapshotConfig.create(self);
  try
    if frmd3dhooksnapshotconfig.showmodal=mrok then
    begin

      safed3dhook;
      mainform.updated3dgui;


      case frmd3dhooksnapshotconfig.rgPictureFormat.itemindex of
        0: pf:=3;
        1: pf:=0;
      end;



      if d3dhook<>nil then
        d3dhook.setSnapshotOptions(frmd3dhooksnapshotconfig.dirSnapshot.Text, frmd3dhooksnapshotconfig.fullsnapshotkey, frmd3dhooksnapshotconfig.smallsnapshotkey, frmd3dhooksnapshotconfig.cbProgressive.checked, frmd3dhooksnapshotconfig.cbClearDepth.checked, frmd3dhooksnapshotconfig.cbAlsoOutputPng.checked, pf);

    end;

  finally
    frmd3dhooksnapshotconfig.free;
  end;
end;

{
procedure TfrmSnapshotHandler.initialize(path: string; count: integer);
begin

end;
}
end.

