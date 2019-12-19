unit dbvmLoadManual;

{$mode delphi}

interface
{$ifdef windows}

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  { TfrmDBVMLoadManual }

  TfrmDBVMLoadManual = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    cpulabels: array of TLabel;
    procedure launchDBVMForCpuClick(Sender: TObject);
    procedure checkfordbvm;
  public
    { public declarations }

  end;

var
  frmDBVMLoadManual: TfrmDBVMLoadManual;

{$endif}

implementation

{$R *.lfm}

uses aboutunit, Parsers, DBK32functions, vmxfunctions;

resourcestring
  rsCpuAlreadyRunningDBVM='This cpu is already running DBVM';
  rsCpu = 'CPU ';
  rsChecking = '<Checking>';
  rsLoaded = 'Loaded:';
  rsNotLoaded = 'Not loaded';
  
{ TfrmDBVMLoadManual }

{$ifdef windows}

procedure TfrmDBVMLoadManual.launchDBVMForCpuClick(Sender: TObject);
var
  cpuid: integer;
  proc, sys: DWORD_PTR;
begin
  //LoadDBK32;
  GetProcessAffinityMask(GetCurrentProcess, proc, sys);

  try
    if (sender is TButton) then
    begin
      cpuid:=TButton(sender).tag;
      SetProcessAffinityMask(GetCurrentProcess, 1 shl cpuid);
      sleep(10);

      if dbvm_version=0 then
      begin
        OutputDebugString(pchar('launchDBVMForCpuClick('+inttostr(cpuid)+')'));
        LaunchDBVM(cpuid);
      end
      else
        raise exception.create(rsCpuAlreadyRunningDBVM);
    end;



  finally
    checkfordbvm;
    SetProcessAffinityMask(GetCurrentProcess, proc);
  end;

end;

procedure TfrmDBVMLoadManual.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  frmDBVMLoadManual:=nil;
  closeaction:=caFree;
end;

procedure TfrmDBVMLoadManual.FormCreate(Sender: TObject);
var
  process, sys: DWORD_PTR;
  i: integer;
  p, oldp: TPanel;
  b: TButton;
  l: TLabel;
begin
  frmDBVMLoadManual:=self;
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
      b.caption:=rsCpu+inttostr(i);
      b.parent:=p;

      l:=TLabel.create(p);
      l.caption:=rsChecking;
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
  id: integer;
begin
  GetProcessAffinityMask(GetCurrentProcess, proc, sys);
  allactive:=true;
  for i:=0 to length(cpulabels)-1 do
  begin
    id:=TLabel(cpulabels[i]).Tag;

    if id=-1 then continue; //already on loaded

    SetProcessAffinityMask(GetCurrentProcess, 1 shl id);
    OutputDebugString(pchar('Testing '+inttostr(id)));
    sleep(10);

    if dbvm_version>0 then
    begin
      cpulabels[i].caption:=rsLoaded+inttostr(dbvm_version and $ffffff);
      cpulabels[i].font.color:=clGreen;
      cpulabels[i].tag:=-1;
    end
    else
    begin
      allactive:=false;
      cpulabels[i].caption:=rsNotLoaded;
      cpulabels[i].font.color:=clWindowText;
    end;
  end;

  SetProcessAffinityMask(GetCurrentProcess, proc);

  if allactive and (about<>nil) then
    about.UpdateDBVMStatus;
end;

{$endif}

end.

