unit frmSaveSnapshotsUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, math, LuaCanvas, FPImage, FPCanvas, FPImgCanv, FPReadPNG, FPWritePNG;

resourcestring
  rsSSAreYouSureYouWishToThrowAwayTheseSnapshots = 'Are you sure you wish to throw away these snapshots?';

type

  { TfrmSaveSnapshots }

  TfrmSaveSnapshots = class(TForm)
    btnSave: TButton;
    btnDone: TButton;
    btnCombinedSelect: TButton;
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
    procedure btnCombinedSelectClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
      pic: TBitmap;
      selected: boolean;
      xpos: integer;
      width: integer;
    end;
    fsaved: tstringlist;

    loaded: integer;
    procedure combinedselect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
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
  picturesize: integer;
  error: string;
  i: integer;
  fpi: TFPMemoryImage;
  fpr: TFPReaderPNG;
  fpw: TFPWriterPNG;


  c: TFPCustomCanvas;

  format: integer;


begin
  try



    if loaded>64 then //time to cleanup
    begin
      for i:=index-16 downto 0 do  //allow a few in front
        if snapshots[i].pic<>nil then
        begin
          freeandnil(snapshots[i].pic);
          dec(loaded);
        end;

      for i:=index+48 to length(snapshots)-1 do
        if snapshots[i].pic<>nil then
        begin
          freeandnil(snapshots[i].pic);
          dec(loaded);
        end;

    end;

    if snapshots[index].pic=nil then
    begin
      s:=tfilestream.Create(snapshots[index].filename, fmOpenRead);
      try
        s.position:=4; //to the format type
        s.readbuffer(format, sizeof(format));
        s.ReadBuffer(picturesize, sizeof(picturesize));

        if format=0 then
        begin
          //bmp
          snapshots[index].pic:=tbitmap.Create;
          snapshots[index].pic.LoadFromStream(s, picturesize);
        end
        else
        if format=3 then //png
        begin

          fpi:=TFPMemoryImage.Create(0,0);
          fpr:=TFPReaderPNG.create;
          fpi.LoadFromStream(s, fpr);


          c:=TFPImageCanvas.create(fpi);

          snapshots[index].pic:=tbitmap.Create;
          snapshots[index].pic.Width:=fpi.Width;
          snapshots[index].pic.Height:=fpi.Height;
          TFPCustomCanvas(snapshots[index].pic.Canvas).CopyRect(0,0, c, rect(0,0,fpi.width, fpi.height));

          c.free;
          fpr.free;
          fpi.free;
        end;

      finally
        s.free;
      end;

      inc(loaded);
    end;

  except
    //this is called from onpaint. Exceptions in onpaint crash apps
{    on e: exception do
    begin
      error:=e.Message;
      ShowMessage(error);
    end;}
  end;
end;

procedure TfrmSaveSnapshots.initialize(path: string; max: integer);
var i: integer;
begin
  try
    scrollbar1.position:=0;
    scrollbar1.Max:=max-1;
    setlength(snapshots, max);

    for i:=0 to max-1 do
    begin
      snapshots[i].filename:=path+'snapshot'+inttostr(i+1)+'.ce3dsnapshot';
      snapshots[i].pic:=nil;
      snapshots[i].selected:=false;
    end;
  except
    setlength(snapshots, 0);
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

procedure TfrmSaveSnapshots.FormShow(Sender: TObject);
begin
  lblSelectAll.Font.height:=GetFontData(font.handle).height;
  lblDeselectAll.Font.height:=GetFontData(font.handle).height;
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
        inc(j);

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


procedure TfrmSaveSnapshots.combinedselect(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var img: timage;
  sx, sy: integer;
  i: integer;


begin
  img:=TImage(sender);
  sx:=trunc((img.Picture.Width/img.Width)*x);
  sy:=trunc((img.Picture.Height/img.Height)*y);

  //find snapshots that have a pixel not $ff00ff at sx,sy

  for i:=0 to length(snapshots)-1 do
  begin
    loadSnapshot(i);

    if snapshots[i].pic.canvas.Pixels[sx,sy]<>$ff00ff then
      snapshots[i].selected:=true;
  end;


  TCustomForm(img.Parent).close;

  PaintBox1.repaint;
end;

procedure TfrmSaveSnapshots.btnCombinedSelectClick(Sender: TObject);
var
  i: integer;
  b: TBitmap;

  b2: tbitmap;
  f: TCustomForm;

  img: timage;
begin
  //create a "combined" view the user can use to select which pixels to pick

  loadSnapshot(0);

  b:=tbitmap.create;
  b.Width:=snapshots[0].pic.Width;
  b.Height:=snapshots[0].pic.Height;




  for i:=0 to length(snapshots)-1 do
  begin
    loadSnapshot(i);
    //for some reason I first need to convert it to a BMP before transparancy take an effect

    b2:=tbitmap.create;
    b2.width:=snapshots[i].pic.width;
    b2.Height:=snapshots[i].pic.Height;

    b2.Canvas.Draw(0,0,snapshots[i].pic);


    b2.TransparentColor:=$ff00ff;
    b2.Transparent:=true;

    b.Canvas.Draw(0,0,b2);

    b2.free;
  end;


  f:=TCustomForm.create(self);
  img:=TImage.create(f);
  img.Picture.Bitmap:=b;
  img.parent:=f;
  img.align:=alClient;
  img.Stretch:=true;
  img.OnMouseDown:=combinedselect;

  f.ClientWidth:=b.width;
  f.ClientHeight:=b.Height;
  f.BorderIcons:=[biSystemMenu];
  f.position:=poScreenCenter;
  f.showmodal;

  img.free;
  f.free;
  b.free;


end;

procedure TfrmSaveSnapshots.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if (saved.count>0) or (MessageDlg(rsSSAreYouSureYouWishToThrowAwayTheseSnapshots, mtConfirmation, [mbyes, mbno], 0)=mryes) then
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
  paintbox1.Canvas.Clear;

  startpos:=scrollbar1.Position;

  if startpos>=length(snapshots) then
    exit;

  xpos:=0;

  //
  if snapshots[startpos].pic=nil then
    loadSnapshot(startpos);




  h:=paintbox1.Height;




  for i:=startpos to length(snapshots)-1 do
  begin
    if snapshots[i].pic=nil then
      loadSnapshot(i);

    if snapshots[i].pic=nil then continue;

    aspectratio:=snapshots[i].pic.Width/snapshots[i].pic.Height;



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
      exit; //done
  end;

end;

procedure TfrmSaveSnapshots.PaintBox1Resize(Sender: TObject);
begin


end;

end.

