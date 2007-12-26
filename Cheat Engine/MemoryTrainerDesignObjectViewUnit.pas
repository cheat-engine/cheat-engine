unit MemoryTrainerDesignObjectViewUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ValEdit,StdCtrls, ExtDlgs,ExtCtrls,ExtraTrainerComponents;

type
  TfrmTrainerDesignObjectView = class(TForm)
    ValueListEditor1: TValueListEditor;
    ColorDialog1: TColorDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure ValueListEditor1StringsChange(Sender: TObject);
    procedure ValueListEditor1EditButtonClick(Sender: TObject);
    procedure ValueListEditor1Validate(Sender: TObject; ACol,
      ARow: Integer; const KeyName, KeyValue: String);
    procedure ValueListEditor1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    
  end;

var
  frmTrainerDesignObjectView: TfrmTrainerDesignObjectView;

implementation

uses MemoryTrainerDesignUnit;

{$R *.dfm}

function strToCursor(x: string): tcursor;
begin
  if x='Handpoint' then result:=crhandpoint else result:=crDefault;
end;

function strtobevelkind(x:string): tbevelkind;
begin
  if x='None' then result:=bkNone else
  if x='Tile' then result:=bkTile else
  if x='Soft' then result:=bkSoft else
  if x='Flat' then result:=bkFlat;
end;

function bevelinout(x:string): tbevelcut;
begin
  if x='None' then result:=bvNone else
  if x='Lowered' then result:=bvLowered else
  if x='Raised' then result:=bvRaised else
  if x='Space' then result:=bvSpace else
  result:=bvNone;
end;

procedure TfrmTrainerDesignObjectView.FormCreate(Sender: TObject);
begin
//ValueListEditor1.InsertRow()
  left:=screen.WorkAreaLeft;
  top:=screen.WorkAreaHeight-height;
end;

procedure TfrmTrainerDesignObjectView.ValueListEditor1StringsChange(
  Sender: TObject);
var key,value: string;
    newcolor:tcolor;
    i,j:integer;
begin
  key:=Valuelisteditor1.Keys[Valuelisteditor1.row];
  value:=Valuelisteditor1.Values[key];




  if (frmtrainerdesigner.selectedobject<>nil) then
  begin
    if key='On Click' then
      twincontrol(frmtrainerdesigner.selectedobject).Tag:=Valuelisteditor1.ItemProps['On Click'].PickList.IndexOf(value);

    //tbutton
    if (frmtrainerdesigner.selectedobject is tbutton2) then
    begin
      if key='Text' then
        tbutton2(frmtrainerdesigner.selectedobject).Caption:=value;

      if key='Word Wrap' then
        tbutton2(frmtrainerdesigner.selectedobject).WordWrap:=strtobool(value);

      if key='Command' then
        tbutton2(frmtrainerdesigner.selectedobject).command:=value;


    end;

    if (frmtrainerdesigner.selectedobject is tcheatlist) then
    begin
      if key='Background color' then
      begin
        val('$'+value,newcolor,i);
        if i=0 then
          tcheatlist(frmtrainerdesigner.selectedobject).Color:=newcolor;
      end;

      if key='Text color' then
      begin
        val('$'+value,newcolor,i);
        if i=0 then
          tcheatlist(frmtrainerdesigner.selectedobject).TextColor:=newcolor;
      end;

      if key='Activation color' then
      begin
        val('$'+value,newcolor,i);
        if i=0 then
          tcheatlist(frmtrainerdesigner.selectedobject).activationcolor:=newcolor;
      end;

      if key='Show hotkeys' then
        tcheatlist(frmtrainerdesigner.selectedobject).Showhotkeys:=strtobool(value);


      if key='Show checkboxes' then
        tcheatlist(frmtrainerdesigner.selectedobject).HasCheckbox:=strtobool(value);

      //'Description left' =handled at validate
      //'Edit left' same

      if key='Bevel Inner' then tcheatlist(frmtrainerdesigner.selectedobject).BevelInner:=bevelinout(value);
      if key='Bevel Outer' then tcheatlist(frmtrainerdesigner.selectedobject).BevelOuter:=bevelinout(value);
      if key='Bevel Kind' then tcheatlist(frmtrainerdesigner.selectedobject).bevelkind:=strtobevelkind(value);


    end;

    //tcheat
    if (frmtrainerdesigner.selectedobject is tcheat) then
    begin
      if key='Cheat' then
      begin
        val(value,j,i);
        if i=0 then
          tcheat(frmtrainerdesigner.selectedobject).cheatnr:=j;

        frmTrainerDesigner.updatecheats;
      end;

      if key='Background color' then
      begin
        val('$'+value,newcolor,i);
        if i=0 then
          tcheat(frmtrainerdesigner.selectedobject).Color:=newcolor;
      end;

      if key='Text color' then
      begin
        val('$'+value,newcolor,i);
        if i=0 then
          tcheat(frmtrainerdesigner.selectedobject).TextColor:=newcolor;
      end;

      if key='Activation color' then
      begin
        val('$'+value,newcolor,i);
        if i=0 then
          tcheat(frmtrainerdesigner.selectedobject).activationcolor:=newcolor;
      end;

      if key='Show hotkey' then
        tcheat(frmtrainerdesigner.selectedobject).showhotkey:=strtobool(value);

      if key='Show checkbox' then
        tcheat(frmtrainerdesigner.selectedobject).HasCheckbox:=strtobool(value);
    end;

    if (frmtrainerdesigner.selectedobject is timage2) then
    begin
      if key='Stretch' then
        timage2(frmtrainerdesigner.selectedobject).stretch:=strtobool(value);

      if key='Transparent' then
        timage2(frmtrainerdesigner.selectedobject).Transparent:=strtobool(value);

      if key='Cursor' then
        timage2(frmtrainerdesigner.selectedobject).Cursor:=strtocursor(value);

      if key='Command' then
        timage2(frmtrainerdesigner.selectedobject).command:=value;

    end;


    //tlabel
    if (frmtrainerdesigner.selectedobject is tlabel2) then
    begin
      if key='Text' then
        tlabel2(frmtrainerdesigner.selectedobject).Caption:=value;

      if key='Word Wrap' then
        tlabel2(frmtrainerdesigner.selectedobject).WordWrap:=strtobool(value);

      if key='Underline' then
      begin
        if strtobool(value) then
          tlabel2(frmtrainerdesigner.selectedobject).Font.Style:=[fsUnderline]
        else
          tlabel2(frmtrainerdesigner.selectedobject).Font.Style:=[];
      end;


      if key='Color' then
      begin
        val('$'+value,newcolor,i);
        if i=0 then
          tlabel2(frmtrainerdesigner.selectedobject).font.color:=newcolor;
      end;

      if key='Cursor' then
        tlabel2(frmtrainerdesigner.selectedobject).Cursor:=strtocursor(value);

      if key='Command' then
        tlabel2(frmtrainerdesigner.selectedobject).command:=value;

    end;

  end;
end;

procedure TfrmTrainerDesignObjectView.ValueListEditor1EditButtonClick(
  Sender: TObject);
var key,value: string;
begin
  key:=Valuelisteditor1.Keys[Valuelisteditor1.row];
  value:=Valuelisteditor1.Values[key];

  if (frmtrainerdesigner.selectedobject<>nil) then
  begin
    //tcheatlist
    if (frmtrainerdesigner.selectedobject is tcheatlist) then
    begin
      if key='Activation color' then
      begin
        colordialog1.Color:=tcheatlist(frmtrainerdesigner.selectedobject).activationcolor;
        if colordialog1.Execute then
          Valuelisteditor1.Values[key]:=inttohex(colordialog1.Color,6);
      end;

      if key='Background color' then
      begin
        colordialog1.Color:=tcheatlist(frmtrainerdesigner.selectedobject).Color;
        if colordialog1.Execute then
          Valuelisteditor1.Values[key]:=inttohex(colordialog1.Color,6);
      end;

      if key='Text color' then
      begin
        colordialog1.Color:=tcheatlist(frmtrainerdesigner.selectedobject).TextColor;
        if colordialog1.Execute then
          Valuelisteditor1.Values[key]:=inttohex(colordialog1.Color,6);
      end;
    end;

    //tcheat
    if (frmtrainerdesigner.selectedobject is tcheat) then
    begin
      if key='Activation color' then
      begin
        colordialog1.Color:=tcheat(frmtrainerdesigner.selectedobject).activationcolor;
        if colordialog1.Execute then
          Valuelisteditor1.Values[key]:=inttohex(colordialog1.Color,6);
      end;

      if key='Background color' then
      begin
        colordialog1.Color:=tcheat(frmtrainerdesigner.selectedobject).Color;
        if colordialog1.Execute then
          Valuelisteditor1.Values[key]:=inttohex(colordialog1.Color,6);
      end;

      if key='Text color' then
      begin
        colordialog1.Color:=tcheat(frmtrainerdesigner.selectedobject).TextColor;
        if colordialog1.Execute then
          Valuelisteditor1.Values[key]:=inttohex(colordialog1.Color,6);
      end;
    end;

    if (frmtrainerdesigner.selectedobject is timage2) then
    begin
      if key='Picture' then
      begin
        if openpicturedialog1.Execute then
          timage(frmtrainerdesigner.selectedobject).Picture.LoadFromFile(openpicturedialog1.FileName);
      end;
    end;

    //tlabel
    if (frmtrainerdesigner.selectedobject is tlabel2) then
    begin
      if key='Color' then
      begin
        colordialog1.Color:=tlabel2(frmtrainerdesigner.selectedobject).Color;
        if colordialog1.Execute then
          Valuelisteditor1.Values[key]:=inttohex(colordialog1.Color,6);
      end;
    end;
  end;
end;

procedure TfrmTrainerDesignObjectView.ValueListEditor1Validate(
  Sender: TObject; ACol, ARow: Integer; const KeyName, KeyValue: String);
var i:integer;
    newvalue:integer;
begin
  if (frmtrainerdesigner.selectedobject is tcheatlist) then
  begin
    if keyname='Hotkey left' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheatlist(frmtrainerdesigner.selectedobject).hotkeyleft:=newvalue;
    end;

    if keyname='Description left' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheatlist(frmtrainerdesigner.selectedobject).descriptionleft:=newvalue;
    end;

    if keyname='Edit left' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheatlist(frmtrainerdesigner.selectedobject).editleft:=newvalue;
    end;

    if keyname='Edit width' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheatlist(frmtrainerdesigner.selectedobject).editwidth:=newvalue;
    end;

    if keyname='Bevel Width' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheatlist(frmtrainerdesigner.selectedobject).bevelwidth:=newvalue;
    end;
  end;

  if (frmtrainerdesigner.selectedobject is tcheat) then
  begin
    if keyname='Hotkey left' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheat(frmtrainerdesigner.selectedobject).hotkeyleft:=newvalue;
    end;

    if keyname='Description left' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheat(frmtrainerdesigner.selectedobject).descriptionleft:=newvalue;
    end;

    if keyname='Edit left' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheat(frmtrainerdesigner.selectedobject).editleft:=newvalue;
    end;

    if keyname='Edit width' then
    begin
      val(keyvalue,newvalue,i);
      if i=0 then
        tcheat(frmtrainerdesigner.selectedobject).editwidth:=newvalue;
    end;

  end;

end;

procedure TfrmTrainerDesignObjectView.ValueListEditor1KeyPress(
  Sender: TObject; var Key: Char);
var k,v:string;
begin
  k:=Valuelisteditor1.Keys[Valuelisteditor1.row];
  v:=Valuelisteditor1.Values[k];

  if key=#13 then
    valuelisteditor1.OnValidate(sender,0,0,k,v);
end;

procedure TfrmTrainerDesignObjectView.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  canclose:=false;
end;

end.
