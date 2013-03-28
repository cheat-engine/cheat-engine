unit frmSaveSnapshotsUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, math, LuaCanvas;

type

  { TfrmSaveSnapshots }

  TfrmSaveSnapshots = class(TForm)
    btnSave: TButton;
    btnDone: TButton;
    Label1: TLabel;
    lblDeselectAll: TLabel;
    lblSelectAll: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    ScrollBar1: TScrollBar;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDoneClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lblDeselectAllClick(Sender: TObject);
    procedure lblSelectAllClick(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
  private
    { private declarations }
    snapshots: array of record
      filename: string;
      pic: TPortableNetworkGraphic;
      selected: boolean;
      xpos: integer;
      width: integer;
    end;
    fsaved: tstringlist;

    procedure loadSnapshot(index: integer);
  public
    { public declarations }
    procedure initialize(path: string; max: integer);
    property saved: TStringlist read fsaved;
  end;

var
  frmSaveSnapshots: TfrmSaveSnapshots;

implementation

{$R *.lfm}

{ TfrmSaveSnapshots }

procedure TfrmSaveSnapshots.loadSnapshot(index: integer);
var
  s: Tfilestream;
  pngsize: integer;
  error: string;
begin
  try
    if snapshots[index].pic<>nil then
      FreeAndNil(snapshots[index].pic);

    s:=tfilestream.Create(snapshots[index].filename, fmOpenRead);
    try
      s.position:=4; //to the png size
      s.ReadBuffer(pngsize, sizeof(pngsize));


      snapshots[index].pic:=TPortableNetworkGraphic.Create;
      snapshots[index].pic.LoadFromStream(s, pngsize);
    finally
      s.free;
    end;

  except
    //this is called from onpaint. Exceptions in onpaint crash apps
    on e: exception do
    begin
      error:=e.Message;
      ShowMessage(error);
    end;
  end;
end;

procedure TfrmSaveSnapshots.initialize(path: string; max: integer);
var i: integer;
begin
  scrollbar1.position:=0;
  scrollbar1.Max:=max-1;
  setlength(snapshots, max);

  for i:=0 to max-1 do
  begin
    snapshots[i].filename:=path+'snapshot'+inttostr(i+1)+'.ce3dsnapshot';
    snapshots[i].pic:=nil;
    snapshots[i].selected:=false;
  end;
end;

procedure TfrmSaveSnapshots.Panel2Resize(Sender: TObject);
begin
  btnsave.left:=panel2.Width div 2-btnsave.width div 2;
  btnDone.left:=panel2.Width div 2-btnDone.width div 2;
end;

procedure TfrmSaveSnapshots.ScrollBar1Change(Sender: TObject);
begin
  PaintBox1.Repaint;
end;



procedure TfrmSaveSnapshots.FormCreate(Sender: TObject);
begin
  DoubleBuffered:=true;
  Panel1.DoubleBuffered:=true;
  fsaved:=TStringList.create;
end;

procedure TfrmSaveSnapshots.FormDestroy(Sender: TObject);
var i: integer;
begin
  if fsaved<>nil then
    freeandnil(fsaved);

  for i:=0 to length(snapshots)-1 do
  begin
    if snapshots[i].pic<>nil then
      FreeAndNil(snapshots[i].pic);
  end;
end;

procedure TfrmSaveSnapshots.lblDeselectAllClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to length(snapshots)-1 do
    snapshots[i].selected:=false;

  paintbox1.Repaint;
end;

procedure TfrmSaveSnapshots.btnCancelClick(Sender: TObject);
begin

end;

procedure TfrmSaveSnapshots.btnSaveClick(Sender: TObject);
var i: integer;
  v: string;
  j: integer;

  ext: string;
  f: string;


  fn: string;
begin



  if savedialog1.execute then
  begin
    //copy all selected snapshots to this folder

    ext:=ExtractFileExt(savedialog1.filename);
    f:=ExtractFileNameWithoutExt(savedialog1.filename);



    j:=1;
    for i:=0 to length(snapshots)-1 do
      if snapshots[i].selected then
      begin
        if j=1 then
          fn:=f+ext
        else
          fn:=f+inttostr(j)+ext;

        CopyFile(snapshots[i].filename, fn, true);
        fsaved.Add(fn);

      end;

    lblDeselectAll.OnClick(lblSelectAll);

  end;

end;

procedure TfrmSaveSnapshots.btnDoneClick(Sender: TObject);
var i,j: integer;
begin
  j:=0;
  for i:=0 to length(snapshots)-1 do
    if snapshots[i].selected then
      inc(j);

  if j>0 then //something is selected
    btnSave.click;

  close;
end;

procedure TfrmSaveSnapshots.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if (saved.count>0) or (MessageDlg('Are you sure you wish to throw away these snapshots?', mtConfirmation, [mbyes, mbno], 0)=mryes) then
    CanClose:=true
  else
    canclose:=false;
end;

procedure TfrmSaveSnapshots.lblSelectAllClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to length(snapshots)-1 do
    snapshots[i].selected:=true;

  paintbox1.Repaint;
end;

procedure TfrmSaveSnapshots.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  for i:=scrollbar1.Position to length(snapshots)-1 do
    if InRange(x, snapshots[i].xpos, snapshots[i].xpos+snapshots[i].width) then
    begin
      snapshots[i].selected:=not snapshots[i].selected;
      break;
    end;

  PaintBox1.repaint;
end;

procedure TfrmSaveSnapshots.PaintBox1Paint(Sender: TObject);
var i,j: integer;
  startpos: integer;
  xpos: integer;

  aspectratio: single;
  currentw: integer;

  h: integer;
begin
  //load and draw the pictures
  startpos:=scrollbar1.Position;

  xpos:=0;

  //
  if snapshots[startpos].pic=nil then
    loadSnapshot(startpos);


  aspectratio:=snapshots[startpos].pic.Width/snapshots[startpos].pic.Height;

  h:=paintbox1.Height;


  paintbox1.Canvas.Clear;

  for i:=0 to startpos-1 do //cleanup
    if snapshots[i].pic<>nil then
      FreeAndNil(snapshots[i].pic);


  for i:=startpos to length(snapshots)-1 do
  begin
    if snapshots[i].pic=nil then
      loadSnapshot(i);

    currentw:=ceil(h*aspectratio);

    paintbox1.Canvas.CopyRect(rect(xpos, 0, xpos+currentw, h), snapshots[i].pic.Canvas, rect(0,0,snapshots[i].pic.width, snapshots[i].pic.height));
    snapshots[i].xpos:=xpos;
    snapshots[i].width:=currentw;

    if snapshots[i].selected then
    begin
      paintbox1.Canvas.Pen.Width:=3;
      paintbox1.canvas.pen.Color:=clAqua;
      paintbox1.canvas.Brush.Style:=bsClear;
      paintbox1.Canvas.Rectangle(rect(xpos, 0, xpos+currentw, h));
      paintbox1.canvas.Brush.Style:=bsSolid;

    end;


    inc(xpos, currentw+1);
    if xpos>paintbox1.Width then
    begin
      for j:=i+1 to length(snapshots)-1 do
        if snapshots[j].pic<>nil then
          FreeAndNil(snapshots[j].pic);

      scrollbar1.LargeChange:=min(1, i);
      exit; //done
    end;
  end;

end;

procedure TfrmSaveSnapshots.PaintBox1Resize(Sender: TObject);
begin


end;

end.

