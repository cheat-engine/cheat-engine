unit formPatchEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmPatchEdit = class(TForm)
    editOffset: TEdit;
    Label1: TLabel;
    editLength: TEdit;
    Label2: TLabel;
    editFilename: TEdit;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure editOffsetKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
    edit: boolean;
  end;

var
  frmPatchEdit: TfrmPatchEdit;

implementation

{$R *.dfm}

uses formPatcherMaker, formPatcherMaker3;

procedure TfrmPatchEdit.FormShow(Sender: TObject);
var i,j: integer;
begin
  if not edit then
  begin
    editFilename.Text:='';
    editOffset.Text:='00000000';
    editLength.Text:='0';
    messagedlg('This patch entry won''t be used to check if the file being patched is the right version!',mtInformation,[mbok],0);
  end
  else
  begin
    with frmPatcherMaker do
    begin
      i:=patchcodelist.ItemIndex;
      editfilename.Text:=patches[i].filename;
      editOffset.Text:=IntToHex(patches[i].address,1);
      editLength.Text:=IntToStr(length(patches[i].mem));

      for j:=0 to length(patches[i].mem)-1 do
        if patches[i].mem[j]<>$90 then
        begin
          messagedlg('If you change this entry it wont be used to check if the file being patched is the right version!',mtInformation,[mbok],0);
          break;
        end;
      end;
  end;

end;

procedure TfrmPatchEdit.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=caFree;
end;

procedure TfrmPatchEdit.Button1Click(Sender: TObject);
var i,j: integer;
begin
  with frmPatchermaker do
  begin
    if self.edit then
    begin
      //the record already exists
      i:=patchcodelist.ItemIndex;
    end
    else
    begin
      //there is not yet an entry, so make it
      i:=length(patches);
      setlength(patches,i+1);
    end;

    try
      patches[i].address:=StrToInt('$'+editOffset.Text);
    except
      raise exception.Create('The offset you specified is not valid');
    end;

    try
      j:=StrToInt(editlength.Text);
    except
      raise exception.Create('The length you specified is not valid');
    end;

    setlength(patches[i].mem,j);
    for j:=0 to length(patches[i].mem)-1 do patches[i].mem[j]:=$90;

    patches[i].filename:=editFilename.Text;

    frmpatchermaker3.FillList;
    modalresult:=mrok;
    close;
  end;
end;

procedure TfrmPatchEdit.editOffsetKeyPress(Sender: TObject; var Key: Char);
begin
  case key of
    chr(65)..chr(70) : ;
    chr(97)..chr(102) : ;
    chr(8)        : ;
    chr(16)       : ;
    chr(48)..chr(57) : ;
    else key:=chr(0);
  end;
end;

end.
