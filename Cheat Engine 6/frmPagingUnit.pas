unit frmPagingUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,newkernelhandler,cefuncproc;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    cb8byteentries: TCheckBox;
    cb64bit: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    frmPaging: TPanel;
    tvPage: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure cb64bitChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2: TForm2; 

implementation

{ TForm2 }

procedure TForm2.cb64bitChange(Sender: TObject);
begin
  if cb64bit.checked then
  begin
    cb8byteentries.checked:=true;
    cb8byteentries.enabled:=false;
  end
  else
    cb8byteentries.enabled:=true;
end;

procedure TForm2.Button1Click(Sender: TObject);
var x: dword;
  base: ptrUint;
  buf: pbyte;

  q: Puint64Array;
  i: integer;
  a,b: qword;

  tn: ttreenode;
begin
  tvpage.Items.Clear;
  buf:=getmem(4096);
  try
    base:=base and ulong_ptr((not ulong_ptr($ff)));
    if ReadPhysicalMemory(0, base, @buf, 4096, x) then
    begin
      if cb64bit then //this is the pml4 table
      begin
        q:=buf;
        for i:=0 to 511 do
          if (odd(q[i]) then
          begin
            a:=i*qword($8000000000);
            if a>=qword($0000800000000000) then
              a:=a and qword($ffff000000000000);

            b:=b+qword($7fffffffff);

            tn:=tvpage.Items.Add(nil,inttostr(i)+':'+inttohex(a,16)+'-'+inttohex(b,16));

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

