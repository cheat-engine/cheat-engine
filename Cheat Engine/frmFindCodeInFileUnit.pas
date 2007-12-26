unit frmFindCodeInFileUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TformFindCodeInFile = class(TForm)
    FoundList: TListBox;
    SaveDialog1: TSaveDialog;
    Button2: TButton;
    btnReplace: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OpenDialog1: TOpenDialog;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FoundListClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    ok: boolean;
  end;

var
  formFindCodeInFile: TformFindCodeInFile;

implementation

uses AdvancedOptionsUnit;

{$R *.dfm}

procedure TformFindCodeInFile.FormCreate(Sender: TObject);

begin
  ok:=false;
  if not opendialog1.execute then
  begin
    modalresult:=mrOk;
    close;
  end else ok:=true;
end;

procedure TformFindCodeInFile.FoundListClick(Sender: TObject);
begin
  if foundlist.SelCount>0 then btnReplace.enabled:=true;

end;

procedure TformFindCodeInFile.btnReplaceClick(Sender: TObject);
var filename,temp: string;
    mem:tmemorystream;
    mempointer: ^Byte;
    a: dword;
    i,j: integer;
begin
  with advancedoptions do
  begin
    filename:='new'+extractfilename(formFindCodeInFile.OpenDialog1.filename);
    filename:=extractfilepath(formFindCodeInFile.OpenDialog1.FileName)+filename;
    formFindCodeInFile.savedialog1.filename:=filename;

    if formFindCodeInFile.savedialog1.execute then
    begin
      mem:=TMemorystream.create;

      try
        mem.LoadFromFile(formFindCodeInFile.OpenDialog1.filename);
      except
        mem.free;
        raise exception.create('Error while reading the source file! Please make sure that the file you want to read from is not in use by another application!');
      end;

      for i:=0 to foundlist.items.count-1 do
      begin
        if foundlist.Selected[i] then
        begin
          mempointer:=mem.memory;
          temp:='$'+copy(foundlist.Items[i],5,8);

          a:=StrToInt(temp);
          inc(mempointer,a);
          for j:=0 to length(code[codelist.itemindex].actualopcode)-1 do
          begin
            mempointer^:=$90;
            inc(mempointer);
          end;
        end;
      end;


      try
        mem.SaveToFile(formFindCodeInFile.savedialog1.filename);
      except
        mem.free;
        raise exception.create('Error while writing to the file! Please make sure that the path you specified is correct and that the file you want to write to is not in use by another application!');
      end;
      mem.free;
      showmessage('Replaced!');
    end;
  end;
end;

procedure TformFindCodeInFile.FormShow(Sender: TObject);
var mem: TMemorystream;
    pnt: ^byte;
    i: int64;
    j: integer;
    k: integer;

    searchstring: array of byte;
    certain: integer;

    bef,aft:boolean;
    pnt2,pnt3: ^byte;
    pnt2b,pnt3b: ^byte;

    indeX: integer;
begin
  mem:=TMemorystream.Create;
  mem.LoadFromFile(opendialog1.filename);

  with advancedoptions do
  begin


    indeX:=codelist.itemindex;
    setlength(searchstring,length(code[index].before)+length(code[index].actualopcode)+length(code[index].after));
    j:=0;
    for k:=0 to length(code[index].before)-1 do
    begin
      searchstring[j]:=code[index].before[k];
      inc(j);
    end;

    for k:=0 to length(code[index].actualopcode)-1 do
    begin
      searchstring[j]:=code[index].actualopcode[k];
      inc(j);
    end;

    for k:=0 to length(code[index].after)-1 do
    begin
      searchstring[j]:=code[index].after[k];
      inc(j);
    end;



    i:=0;
    j:=0;
    pnt:=mem.Memory;
    while i<mem.Size do
    begin
      if pnt^=code[index].actualopcode[j] then
      begin
        inc(j);
        if j=length(code[index].actualopcode) then
        begin
          //found one
          bef:=true;
          aft:=true;

          //check if the bytes before are what I need

          pnt2:=pointer(dword(pnt)-dword(length(code[index].actualopcode))-dword(length(code[index].before)-1));
          pnt3:=pointer(pnt);
          inc(pnt3);

          pnt2b:=pointer(pnt2);
          pnt3b:=pointer(pnt3);

          for k:=0 to length(code[index].before)-1 do
          begin
            if pnt2^<>code[index].before[k] then bef:=false;
            inc(pnt2);
          end;

          for k:=0 to length(code[index].after)-1 do
          begin
            if pnt3^<>code[index].after[k] then aft:=false;
            inc(pnt3);
          end;

          if bef and aft then certain:=1 else
          begin
            //it's not 100% sure
            if (bef or aft) then
            begin
              certain:=3;
              //check if it wasnt a nopped before
              pnt2:=pointer(pnt2b);
              pnt3:=pointer(pnt3b);

              if not bef then
              begin
                bef:=true;
                for k:=0 to length(code[index].before)-1 do
                begin
                  if (pnt2^<>code[index].before[k]) or (pnt2^<>$90) then bef:=false;
                  inc(pnt2);
                end;
              end;

              if not aft then
              begin
                aft:=true;
                for k:=0 to length(code[index].before)-1 do
                begin
                  if (pnt3^<>code[index].before[k]) or (pnt3^<>$90) then bef:=false;
                  inc(pnt3);
                end;
              end;

              if bef and aft then certain:=2;
            end else certain:=4;
          end;

          foundlist.Items.Add('('+inttostr(certain)+') '+inttohex(i-(length(code[index].actualopcode))+1,8));
          j:=0;
        end;
      end else j:=0;

      inc(i);
      inc(pnt);
    end;

    mem.free;
  end;
end;

procedure TformFindCodeInFile.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

end.
