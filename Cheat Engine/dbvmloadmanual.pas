unit dbvmLoadManual;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmDBVMLoadManual }

  TfrmDBVMLoadManual = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    cpulabels: array of TLabel;
    procedure launchDBVMForCpuClick(Sender: TObject);
    procedure checkfordbvm;
  public
    { public declarations }

  end;

implementation

{$R *.lfm}

uses aboutunit, Parsers, DBK32functions, vmxfunctions;

{ TfrmDBVMLoadManual }

procedure TfrmDBVMLoadManual.launchDBVMForCpuClick(Sender: TObject);
var cpuid: integer;
begin
  //LoadDBK32;

  if (sender is TButton) then
  begin
    cpuid:=TButton(sender).tag;
    OutputDebugString(pchar('launchDBVMForCpuClick('+inttostr(cpuid)+')'));
    LaunchDBVM(cpuid);
  end;

  checkfordbvm;
end;

procedure TfrmDBVMLoadManual.FormCreate(Sender: TObject);
var
  process, sys: DWORD_PTR;
  i: integer;
  p, oldp: TPanel;
  b: TButton;
  l: TLabel;
begin
  oldp:=nil;
  GetProcessAffinityMask(GetCurrentProcess, process,sys);
  for i:=0 to {$ifdef cpu64}63{$else}31{$endif} do
  begin
    if getbit(i, sys)=1 then
    begin
      p:=TPanel.create(self);
      p.BevelOuter:=bvNone;
      p.autosize:=true;

      if oldp=nil then
      begin
        p.AnchorSideTop.Control:=self;
        p.AnchorSideTop.Side:=asrTop;
      end
      else
      begin
        p.AnchorSideTop.Control:=oldp;
        p.AnchorSideTop.Side:=asrBottom;
      end;

      p.AnchorSideLeft.control:=self;
      p.AnchorSideLeft.side:=asrLeft;

      p.BorderSpacing.top:=5;

      b:=Tbutton.create(p);
      b.caption:='CPU '+inttostr(i);
      b.parent:=p;

      l:=TLabel.create(p);
      l.caption:='<Checking>';
      l.parent:=p;

      b.AnchorSideTop.Control:=p;
      b.AnchorSideTop.Side:=asrTop;
      b.AnchorSideLeft.Control:=p;
      b.AnchorSideLeft.Side:=asrLeft;
      b.tag:=i;
      b.OnClick:=launchDBVMForCpuClick;

      l.AnchorSideTop.Control:=b;
      l.AnchorSideTop.Side:=asrCenter;
      l.AnchorSideLeft.Control:=b;
      l.AnchorSideLeft.Side:=asrRight;
      l.BorderSpacing.Left:=5;
      l.BorderSpacing.Right:=5;
      l.tag:=i;

      setlength(cpulabels, length(cpulabels)+1);
      cpulabels[length(cpulabels)-1]:=l;

      p.parent:=self;

      oldp:=p;
    end;
  end;

  checkfordbvm;
end;


procedure TfrmDBVMLoadManual.checkfordbvm;
var i: integer;
  proc, sys: DWORD_PTR;

  allactive: boolean;
begin
  GetProcessAffinityMask(GetCurrentProcess, proc, sys);
  allactive:=true;
  for i:=0 to length(cpulabels)-1 do
  begin

    SetProcessAffinityMask(GetCurrentProcess, 1 shl TLabel(cpulabels[i]).Tag);
    OutputDebugString(pchar('Testing '+inttostr(TLabel(cpulabels[i]).Tag)));

    sleep(10);

    if dbvm_version>0 then
    begin
      cpulabels[i].caption:='Loaded:'+inttostr(dbvm_version and $ffffff);
      cpulabels[i].font.color:=clGreen;
    end
    else
    begin
      allactive:=false;
      cpulabels[i].caption:='Not loaded';
      cpulabels[i].font.color:=clWindowText;
    end;
  end;

  SetProcessAffinityMask(GetCurrentProcess, proc);

  if allactive and (about<>nil) then
    about.UpdateDBVMStatus;
end;

end.

