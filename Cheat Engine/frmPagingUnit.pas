unit frmPagingUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Menus,newkernelhandler,cefuncproc, commonTypeDefs;

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
    FindDialog1: TFindDialog;
    pImageList: TImageList;
    Label1: TLabel;
    frmPaging: TPanel;
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    tvPage: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure cb64bitChange(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure tvPageDblClick(Sender: TObject);
    procedure tvPageExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    { private declarations }
    base: ptrUint;
    procedure cleanup;
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

uses MemoryBrowserFormUnit, ProcessHandlerUnit, Parsers;

resourcestring
  rsNotFound = 'Not found';
  rsFailureReadingPhysicalMemory = 'failure reading physical memory';

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

procedure TfrmPaging.FindDialog1Find(Sender: TObject);
var n: ttreenode;
  i,s: integer;
  substring: string;
begin
  i:=0;
  if tvpage.items.count=0 then exit;

  tvpage.Items.BeginUpdate;
  try
    while i<tvpage.Items.count do
    begin
      tvpage.items[i].Expand(true);
      inc(i);

      if tvpage.items[i].Level>5 then raise exception.create('wtf');
    end;
  finally
    tvpage.Items.EndUpdate;
  end;

  n:=tvpage.Selected;

  if n=nil then
    n:=tvpage.items[0];

  s:=n.AbsoluteIndex;
  if frFindNext in FindDialog1.Options then
    inc(s);

  substring:=uppercase(finddialog1.findtext);

  for i:=s to tvpage.Items.Count-1 do
    if pos(substring,  tvpage.items[i].Text)>0 then
    begin
      tvpage.items[i].selected:=true;
      tvpage.items[i].MakeVisible;
      findDialog1.Options:=findDialog1.Options+[frFindNext];
      exit;
    end;

  showmessage(rsNotFound);



end;

procedure TfrmPaging.FormCreate(Sender: TObject);
var cr3: QWORD;
  cr4: DWORD;
begin
  if getcr3(processhandle, cr3) then
    edtcr3.text:=inttohex(cr3,8);

  {$ifdef cpu64}
  cb64bit.checked:=true;
  {$endif}


  //check if pae is enabled or not
  cr4:=GetCR4;
  cb8byteentries.checked:=((cr4 shr 5) and 1)=1;



end;

procedure TfrmPaging.cleanup;
var i: integer;
begin
  for i:=0 to tvpage.Items.Count-1 do
  begin
    freemem(tvpage.items[i].data);
    tvpage.items[i].data:=nil;
  end;

  tvpage.items.Clear;
end;

procedure TfrmPaging.FormDestroy(Sender: TObject);
begin
  cleanup;
end;

procedure TfrmPaging.MenuItem1Click(Sender: TObject);
begin
  finddialog1.Execute;
end;

procedure TfrmPaging.tvPageDblClick(Sender: TObject);
var pd: PPageData;
begin
  if (tvpage.Selected<>nil) and (tvpage.Selected.HasChildren=false) then
  begin
    pd:=tvpage.Selected.data;
    if pd<>nil then
    begin
      MemoryBrowser.hexview.address:=pd.va;
      if MemoryBrowser.visible=false then
        MemoryBrowser.show;
    end;
  end;
end;

procedure TfrmPaging.FillNodeLevel1(node: TTreenode);
var
  pd: PPageData;
  buf: pointer;
  q: Puint64Array absolute buf;
  d: PDwordArray absolute buf;
  i: integer;
  a,b: qword;

  tn: ttreenode;
  x: ptrUint;
  virtualbase: qword;
  physicalbase: qword;

begin
  //page table

  if node=nil then
  begin
    virtualbase:=0;
    physicalbase:=base;
  end
  else
  begin
    pd:=node.data;
    virtualbase:=pd^.va;
    physicalbase:=pd^.pa;
  end;

  buf:=getmem(4096);
  try
    if ReadPhysicalMemory(0, pointer(ptrUint(physicalbase)), buf, 4096, x) then
    begin
      if cb8byteentries.checked then
      begin
        for i:=0 to 511 do
        begin
          if odd(q[i]) then
          begin
            a:=virtualbase+qword(i*qword($1000));
            b:=a+qword($fff);

            pd:=getmem(sizeof(TPageData));
            pd^.va:=a;
            pd^.level:=1;
            pd^.value:=q[i];
            pd^.pa:=q[i] and qword($FFFFFFF000);

            if node=nil then
              tn:=tvpage.Items.AddChild(nil,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')')
            else
              tn:=tvpage.Items.AddChild(node,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')');

            tn.Data:=pd;
            tn.HasChildren:=false;
          end;
        end;
      end
      else
      begin
        //4 byte entries
        for i:=0 to 1023 do
        begin
          if odd(d[i]) then
          begin
            a:=pd^.va+dword(i*dword($1000));
            b:=a+dword($1fff);

            pd:=getmem(sizeof(TPageData));
            pd^.va:=a;
            pd^.level:=1;
            pd^.value:=d[i];
            pd^.pa:=d[i] and dword($FFFFF000);

            if node=nil then
              tn:=tvpage.Items.AddChild(nil,inttostr(i)+':'+inttohex(a,8)+'-'+inttohex(b,8)+'('+inttohex(pd^.pa,8)+')')
            else
              tn:=tvpage.Items.AddChild(node,inttostr(i)+':'+inttohex(a,8)+'-'+inttohex(b,8)+'('+inttohex(pd^.pa,8)+')');

            tn.HasChildren:=false;
            tn.data:=pd;
          end;
        end;
      end;
    end;

  finally
    freememandnil(buf);
  end;
end;

procedure TfrmPaging.FillNodeLevel2(node: TTreenode);
var
  pd: PPageData=nil;
  buf: pointer;
  q: Puint64Array absolute buf;
  d: PDwordArray absolute buf;
  i: integer;
  a,b: qword;

  tn: ttreenode;
  x: ptrUint;
  bigpage: boolean;

  virtualbase: qword;
  physicalbase: qword;

begin
  //fill in the pagedir table
  if node=nil then
  begin
    virtualbase:=0;
    physicalbase:=base;
  end
  else
  begin
    pd:=node.data;
    virtualbase:=pd^.va;
    physicalbase:=pd^.pa;
  end;

  buf:=getmem(4096);
  try
    if ReadPhysicalMemory(0, pointer(ptrUint(physicalbase)), buf, 4096, x) then
    begin
      if cb8byteentries.checked then
      begin
        for i:=0 to 511 do
        begin
          if odd(q[i]) then
          begin
            a:=virtualbase+qword(i*qword($200000));
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

            if node=nil then
              tn:=tvpage.Items.Add(nil,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')')
            else
              tn:=tvpage.Items.AddChild(node,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')');

            tn.data:=pd;


            tn.HasChildren:=not bigpage;
          end;
        end;

      end
      else
      begin
        //4 byte entries (1024 entries)
        for i:=0 to 1023 do
        begin
          if odd(d[i]) then
          begin
            a:=virtualbase+dword(i*qword($400000));
            b:=a+dword($3fffff);


            pd:=getmem(sizeof(TPageData));
            pd^.va:=a;

            bigpage:=((d[i] shr 7) and 1)=1;

            if bigpage then
              pd^.pa:=d[i] and dword($FFC00000)
            else
              pd^.pa:=d[i] and dword($FFFFF000);

            pd^.value:=d[i];
            pd^.level:=2;

            if node=nil then
              tn:=tvpage.Items.Add(nil,inttostr(i)+':'+inttohex(a,8)+'-'+inttohex(b,8)+'('+inttohex(pd^.pa,8)+')')
            else
              tn:=tvpage.Items.AddChild(node,inttostr(i)+':'+inttohex(a,8)+'-'+inttohex(b,8)+'('+inttohex(pd^.pa,8)+')');

            tn.data:=pd;


            tn.HasChildren:=not bigpage;
          end;
        end;
      end;
    end;
  finally
    freememandnil(buf);
  end;

  if pd<>nil then
    freememandnil(pd);

end;

procedure TfrmPaging.FillNodeLevel3(node: TTreenode);
var pd: PPageData=nil;
  buf: pointer;
  q: Puint64Array absolute buf;
  max: integer;
  i: integer;
  a,b: qword;

  tn: ttreenode;
  x: ptrUint;

  virtualbase: qword;
  physicalbase: qword;
begin
  //fill in the pagedir pointer table
  if node=nil then
  begin
    virtualbase:=0;
    physicalbase:=base;
  end
  else
  begin
    pd:=node.data;
    virtualbase:=pd^.va;
    physicalbase:=pd^.pa;
  end;


  buf:=getmem(4096);
  try

  if ReadPhysicalMemory(0, pointer(ptrUint(physicalbase)), buf, 4096, x) then
  begin


    if cb64bit.checked then
      max:=511 //all 8 byte entries
    else
      max:=3;

    for i:=0 to max do
    begin
      if (odd(q[i])) then
      begin

        a:=virtualbase+qword(i*qword($40000000));
        b:=a+qword($3fffffff);




        pd:=getmem(sizeof(TPageData));
        pd^.va:=a;
        pd^.pa:=q[i] and qword($FFFFFFF000);
        pd^.value:=q[i];
        pd^.level:=3;

        if node=nil then
          tn:=tvpage.Items.Add(nil,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')')
        else
          tn:=tvpage.Items.AddChild(node,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16)+'('+inttohex(pd^.pa,16)+')');

        tn.data:=pd;

        tn.HasChildren:=true;
      end;
    end;

  end;

  finally
    freememandnil(buf);
  end;

  if pd<>nil then
    freememandnil(pd);


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
var x: ptrUint;

  buf: pointer;

  q: Puint64Array;
  i: integer;
  a,b: qword;

  tn: ttreenode;

  pd: PPageData;
  cr4: ptruint;
begin
  base:=StrToQWordEx('$'+edtcr3.text);

  cleanup;

  buf:=getmem(4096);
  try
    if cb8byteentries.checked then
      base:=base and ulong_ptr((not ulong_ptr($1f)))
    else
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
        FillNodeLevel3(nil);
    end
    else
      raise exception.create(rsFailureReadingPhysicalMemory);
  finally
    freememandnil(buf);
  end;
end;

initialization
  {$I frmPagingUnit.lrs}

end.

