unit formPatcherMaker;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type TPatch=record
  address: Dword;
  mem: array of byte;
  filename: string;
end;

type
  TfrmPatcherMaker = class(TForm)
    PatchCodeList: TListBox;
    Label1: TLabel;
    Button1: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    current: Integer;

    patches: array of TPatch;

  end;

var
  frmPatcherMaker: TfrmPatcherMaker;

implementation

{$R *.dfm}

uses advancedoptionsunit, frmFindCodeInFileUnit, formPatcherMaker2,
  formPatcherMaker3;

procedure TfrmPatcherMaker.FormShow(Sender: TObject);
var i: integer;
begin
  patchcodelist.Items:=advancedoptions.codelist.items;
end;

procedure TfrmPatcherMaker.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmPatcherMaker.Button1Click(Sender: TObject);
var i,j,k,l: integer;
    temp,temp2,temp3: string;
    path: array of string;

begin
  modalresult:=mrok;
  hide;
  
  for i:=0 to patchcodelist.count-1 do
  begin
    if patchcodelist.Selected[i] then
    begin
      current:=i;
      frmPatcherMaker2:=TfrmPatcherMaker2.Create(self);
      if frmPatcherMaker2.ok then
      begin
        frmPatcherMaker2.Caption:=patchcodelist.items[i];
        frmPatcherMaker2.ShowModal;
      end;
      frmPatcherMaker2.free;
    end;
  end;

  //now try to change the filenames in the patches list to local filenames
  if length(patches)>0 then
  begin

    temp:=extractfilepath(patches[0].filename);
    //fill in the path array
    j:=1;
    for i:=1 to length(temp) do
      if IsPathDelimiter(temp,i) then
      begin
        setlength(path,length(path)+1);
        path[length(path)-1]:=copy(temp,j,i-j+1);
        j:=i+1;
      end;

    //the array is filled with path parts

    for k:=1 to length(patches)-1 do
    begin
      l:=0;
      temp2:=path[0];

      temp:=extractfilepath(patches[k].filename);
      j:=1;
      for i:=1 to length(temp)-1 do
        if IsPathDelimiter(temp,i) then
        begin
          temp3:=copy(temp,j,i-j+1);
          if uppercase(temp3)=uppercase(temp2) then
          begin
            inc(l);
            if l=length(path) then break;

            temp2:=path[l];
          end
          else
          begin
            setlength(path,l);
            break;
          end;


          j:=i+1;
        end;

    end;
  end;

  //the path array now consists out of a path that has something in common with all the files
  //now make a string out of it and make all the files a relative path
  if length(path)>0 then
  begin
    temp:='';
    for i:=0 to length(path)-1 do
      temp:=temp+path[i];

    for i:=0 to length(patches)-1 do
      patches[i].filename:=ExtractRelativePath(uppercase(temp),uppercase(patches[i].filename));
  end;

  frmPatcherMaker3:=TFrmPatcherMaker3.create(self);
  frmPatcherMaker3.showmodal;

  //now show the 3th and final patchmaker window
end;

end.
