unit formPatcherMaker2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmPatcherMaker2 = class(TForm)
    FoundList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FoundListClick(Sender: TObject);
  private
    { Private declarations }
    mem: TMemorystream;
  public
    { Public declarations }
    ok: boolean;
  end;

var
  frmPatcherMaker2: TfrmPatcherMaker2;

implementation

uses AdvancedOptionsUnit, formPatcherMaker;

{$R *.dfm}

procedure TfrmPatcherMaker2.FormCreate(Sender: TObject);
begin
  frmpatchermaker.opendialog1.Title:=frmPatcherMaker.PatchCodeList.Items[frmPatcherMaker.current];
  ok:=frmPatchermaker.opendialog1.execute;
  if not ok then close else
  begin
    mem:=TMemorystream.Create;
    mem.LoadFromFile(frmPatcherMaker.opendialog1.filename);
  end;
end;

procedure TfrmPatcherMaker2.FormShow(Sender: TObject);
var
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


  with advancedoptions do
  begin
    indeX:=frmPatchermaker.current;
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

procedure TfrmPatcherMaker2.Button1Click(Sender: TObject);
var i,j: integer;
    temp: string;
    a: dword;
    act: array of byte;
begin
  setlength(act, length(advancedoptions.code[frmPatcherMaker.current].actualopcode));
  for i:=0 to length(act)-1 do
    act[i]:=advancedoptions.code[frmPatcherMaker.current].actualopcode[i];


  for i:=0 to foundlist.count-1 do
  begin
    if foundlist.selected[i] then
    begin

      with frmpatchermaker do
      begin
        setlength(frmPatcherMaker.patches,length(patches)+1);
        frmPatcherMaker.patches[length(frmPatcherMaker.patches)-1].filename:=frmPatcherMaker.opendialog1.FileName;

        temp:='$'+copy(foundlist.Items[i],5,8);
        a:=StrToInt(temp);
        frmPatcherMaker.patches[length(frmPatcherMaker.patches)-1].address:=a;

        setlength(frmPatcherMaker.patches[length(frmPatcherMaker.patches)-1].mem,length(act));
        for j:=0 to length(act)-1 do
          frmPatcherMaker.patches[length(frmPatcherMaker.patches)-1].mem[j]:=act[j];


      end;
    end;
  end;

  modalresult:=mrok;
end;

procedure TfrmPatcherMaker2.FoundListClick(Sender: TObject);
begin
  if FoundList.itemindex>-1 then button1.enableD:=true;
end;

end.
