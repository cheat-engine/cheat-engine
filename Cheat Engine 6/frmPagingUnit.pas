unit frmPagingUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,newkernelhandler,cefuncproc;

type TPageData=record
  level: integer;
  value: qword;
  va,pa: qword;
end;
PPageData=^TPageData;

type

  { TfrmPaging }

  TfrmPaging = class(TForm)
    Button1: TButton;
    cb8byteentries: TCheckBox;
    cb64bit: TCheckBox;
    edtCR3: TEdit;
    Label1: TLabel;
    frmPaging: TPanel;
    tvPage: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure cb64bitChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvPageExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    { private declarations }
    procedure FillNodeLevel3(node: TTreenode);
    procedure FillNodeLevel2(node: TTreenode);
    procedure FillNodeLevel1(node: TTreenode);
  public
    { public declarations }
  end; 

var
  frmPaging: TfrmPaging;

implementation

{ TfrmPaging }

procedure TfrmPaging.cb64bitChange(Sender: TObject);
begin
  if cb64bit.checked then
  begin
    cb8byteentries.checked:=true;
    cb8byteentries.enabled:=false;
  end
  else
    cb8byteentries.enabled:=true;
end;

procedure TfrmPaging.FormCreate(Sender: TObject);
var cr3: ptrUint;
begin
  if getcr3(processhandle, cr3) then
    edtcr3.text:=inttohex(cr3,8);

  {$ifdef cpu64}
  cb64bit.checked:=true;
  {$endif}

end;

procedure TfrmPaging.FillNodeLevel1(node: TTreenode);
var
  pd: PPageData;
  buf: pointer;
  q: Puint64Array absolute buf;
  i: integer;
  a,b: qword;

  tn: ttreenode;
  x: dword;
begin
  pd:=node.data;
  buf:=getmem(4096);
  try
    if ReadPhysicalMemory(0, pointer(pd^.pa), buf, 4096, x) then
    begin
      if cb8byteentries.checked then
      begin
        for i:=0 to 511 do
        begin
          if odd(q[i]) then
          begin
            a:=pd^.va+qword(i*qword($1000));
            b:=a+qword($1fff);

            pd:=getmem(sizeof(TPageData));
            pd^.va:=a;
            pd^.level:=1;
            pd^.value:=q[i];
            pd^.pa:=q[i] and qword($FFFFFFF000);

            tn:=tvpage.Items.AddChild(node,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')');

            tn.HasChildren:=false;
          end;
        end;
      end
      else
      begin
        //4 byte entries
      end;
    end;

  finally
    freemem(buf);
  end;
end;

procedure TfrmPaging.FillNodeLevel2(node: TTreenode);
var
  pd: PPageData;
  buf: pointer;
  q: Puint64Array absolute buf;
  i: integer;
  a,b: qword;

  tn: ttreenode;
  x: dword;
  bigpage: boolean;
begin
  //fill in the pagedir table
  pd:=node.data;
  buf:=getmem(4096);
  try
    if ReadPhysicalMemory(0, pointer(pd^.pa), buf, 4096, x) then
    begin
      if cb8byteentries.checked then
      begin
        for i:=0 to 511 do
        begin
          if odd(q[i]) then
          begin
            a:=pd^.va+qword(i*qword($200000));
            b:=a+qword($1fffff);


            pd:=getmem(sizeof(TPageData));
            pd^.va:=a;

            bigpage:=((q[i] shr 7) and 1)=1;

            if bigpage then
              pd^.pa:=q[i] and qword($FFFFE00000)
            else
              pd^.pa:=q[i] and qword($FFFFFFF000);

            pd^.value:=q[i];
            pd^.level:=2;

            tn:=tvpage.Items.AddChild(node,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')');
            tn.data:=pd;


            tn.HasChildren:=not bigpage;
          end;
        end;

      end
      else
      begin
        //4 byte entries (1024 entries)
      end;
    end;
  finally
    freemem(buf);
  end;

end;

procedure TfrmPaging.FillNodeLevel3(node: TTreenode);
var pd: PPageData;
  buf: pointer;
  q: Puint64Array absolute buf;
  max: integer;
  i: integer;
  a,b: qword;

  tn: ttreenode;
  x: dword;
begin
  //fill in the pagedir pointer table
  pd:=node.data;
  buf:=getmem(4096);
  try

  if ReadPhysicalMemory(0, pointer(pd^.pa), buf, 4096, x) then
  begin


    if cb64bit.checked then
      max:=511
    else
      max:=3;

    for i:=0 to max do
    begin
      if (odd(q[i])) then
      begin

        a:=pd^.va+qword(i*qword($40000000));
        b:=a+qword($3fffffff);




        pd:=getmem(sizeof(TPageData));
        pd^.va:=a;
        pd^.pa:=q[i] and qword($FFFFFFF000);
        pd^.value:=q[i];
        pd^.level:=3;

        tn:=tvpage.Items.AddChild(node,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')');
        tn.data:=pd;

        tn.HasChildren:=true;
      end;
    end;

  end;

  finally
    freemem(buf);
  end;


end;

procedure TfrmPaging.tvPageExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);

var pd: PPageData;
begin
  if node.Count=0 then //try to expand
  begin
    pd:=node.data;
    if pd=nil then exit;

    case pd.level of
      4: FillNodeLevel3(node);
      3: FillNodeLevel2(node);
      2: FillNodeLevel1(node);
    end;
  end;

  allowExpansion:=true;
end;

procedure TfrmPaging.Button1Click(Sender: TObject);
var x: dword;
  base: ptrUint;
  buf: pointer;

  q: Puint64Array;
  i: integer;
  a,b: qword;

  tn: ttreenode;

  pd: PPageData;
begin
  base:=strtoint64('$'+edtcr3.text);
  tvpage.Items.Clear;
  buf:=getmem(4096);
  try
    base:=base and ulong_ptr((not ulong_ptr($ff)));
    if ReadPhysicalMemory(0, pointer(base), buf, 4096, x) then
    begin
      if cb64bit.checked then //this is the pml4 table
      begin
        q:=buf;
        for i:=0 to 511 do
          if (odd(q[i])) then
          begin
            a:=i*qword($8000000000);
            if a>=qword($0000800000000000) then
              a:=a or qword($ffff000000000000);

            b:=a+qword($7fffffffff);

            pd:=getmem(sizeof(TPageData));
            pd^.level:=4;
            pd^.value:=q[i];
            pd^.va:=a;
            pd^.pa:=q[i] and qword($FFFFFFF000);

            tn:=tvpage.Items.AddChild(nil,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')');
            tn.Data:=pd;
            tn.HasChildren:=true;

          end;
      end
      else
      begin
      end;
    end
    else
      raise exception.create('failure reading physical memory');
  finally
    freemem(buf);
  end;
end;

initialization
  {$I frmPagingUnit.lrs}

end.

