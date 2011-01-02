unit aboutunit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources,shellapi, NewKernelHandler;

type

  { TAbout }

  TAbout = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Image1: TImage;
    Label6: TLabel;
    Button1: TButton;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Button2: TButton;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lblDBVM: TLabel;
    Label11: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lblDBVMClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  About: TAbout;

implementation

uses tlgUnit,MainUnit2,
{$ifdef net}
unit2;
{$else}
MainUnit;
{$endif}

procedure TAbout.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=caFree;
end;

procedure TAbout.Button2Click(Sender: TObject);
begin

  shellexecute(0,'open','https://www.paypal.com/xclick/business=dark_byte%40hotmail.com&no_note=1&tax=0&lc=US',nil,nil,sw_maximize);
end;

procedure TAbout.FormShow(Sender: TObject);
var supportsdbvm: boolean;
    a,b,c,d: dword;
begin
  {$ifdef net}
    groupbox1.Caption:=unit2.CEnorm;
  {$else}
    groupbox1.Caption:=mainunit2.CEnorm;
  {$endif}



  if not isDBVMCapable then
  begin
    lblDBVM.Font.Color:=clRed;
    lbldbvm.caption:='Your system DOES NOT support DBVM';
    lbldbvm.Hint:='This means that you will need a new cpu (intel) to be able to use the advanced dbvm options';
    lbldbvm.ShowHint:=true;
  end
  else
  begin
{$ifdef cpu32}
    Loaddbk32;
{$endif}
    if (assigned(dbvm_version)) and (dbvm_version>0) then
    begin
      lblDBVM.Font.Color:=clLime;
      lbldbvm.caption:='Your system is running DBVM version '+inttostr(dbvm_version and $00ffffff);
      lbldbvm.Hint:='This means that your system is running dbvm. This means ce will make use of some advanced tools that are otherwhise unavailable';
      lbldbvm.ShowHint:=true;
    end
    else
    begin
      lblDBVM.Font.Color:=clGreen;
      lbldbvm.caption:='Your system supports DBVM';
      lbldbvm.Hint:='This means that you''re currently not running dbvm, but that your system is capable of running it';
      lbldbvm.ShowHint:=true;
    end;
  end;


  if (not assigned(dbvm_version)) or (dbvm_version=0) then
  begin
    supportsdbvm:=false;

    begin
      {$ifdef cpu64}
      asm
        push rax
        push rbx
        push rcx
        push rdx
        mov eax,0
        cpuid
        mov a,eax
        mov b,ebx
        mov c,ecx
        mov d,edx
        pop rdx
        pop rcx
        pop rbx
        pop rax
      end;
      {$else}
      asm
        push eax
        push ebx
        push ecx
        push edx
        mov eax,0
        cpuid
        mov a,eax
        mov b,ebx
        mov c,ecx
        mov d,edx
        pop edx
        pop ecx
        pop ebx
        pop eax
      end;
      {$endif}

      //GenuineIntel check
      if (b=$756e6547) and (d=$49656e69) and (c=$6c65746e) then
      begin
        //it's an intel

        asm
          {$ifdef cpu64}
          push rax
          push rbx
          push rcx
          push rdx
          {$else}
          push eax
          push ebx
          push ecx
          push edx
          {$endif}
          mov eax,1
          cpuid
          mov a,eax
          mov b,ebx
          mov c,ecx
          mov d,edx
          {$ifdef cpu64}
          pop rdx
          pop rcx
          pop rbx
          pop rax
          {$else}
          pop edx
          pop ecx
          pop ebx
          pop eax
          {$endif}
        end;


        if ((c shr 5) and 1)=1 then //check for the intel-vt flag
          supportsdbvm:=true;
      end;
    end;

    if supportsdbvm then
    begin
      lblDBVM.Font.Color:=clGreen;
      lbldbvm.caption:='Your system supports DBVM';
      lbldbvm.Hint:='This means that you''re currently not running dbvm, but that your system is cable of running it (click this to launch dbvm)';
      lbldbvm.ShowHint:=true;
      lbldbvm.Cursor:=crHandPoint;
    end
    else
    begin
      lblDBVM.Font.Color:=clRed;
      lbldbvm.caption:='Your system DOES NOT support DBVM';
      lbldbvm.Hint:='This means that you will need a new cpu (intel) to be abnle to use the advanced dbvm options';
      lbldbvm.ShowHint:=true;
      lbldbvm.Cursor:=crNo;
    end;
  end
  else
  begin
    lblDBVM.Font.Color:=clLime;
    lbldbvm.caption:='Your system is running DBVM version '+inttostr(dbvm_version and $00ffffff);
    lbldbvm.Hint:='This means that your system is running dbvm. This means ce will make use of some advanced tools that are otherwhise unavailable';
    lbldbvm.ShowHint:=true;
    lbldbvm.Cursor:=crDefault;
  end;
end;

procedure TAbout.Label8Click(Sender: TObject);
begin
  ShellExecute(0, pchar('open'),pchar('http://cheatengine.org/'), pchar(''),pchar(''), SW_MAXIMIZE	);
end;

procedure TAbout.Label9Click(Sender: TObject);
begin
  ShellExecute(0, pchar('open'),pchar('http://forum.cheatengine.org/'), pchar(''),pchar(''), SW_MAXIMIZE	);
end;

procedure TAbout.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) and (ssAlt in Shift) and (ssShift in Shift) then
  begin
    ShowMessage('Did you really think you''d find an easter egg by doing this? Well, you know what? You where right!');
    with TTlg.create(self) do show;
  end;
end;

procedure TAbout.lblDBVMClick(Sender: TObject);
begin
  if not isRunningDBVM then
  begin
    if not isDBVMCapable then exit;
    
    if not Is64bitOS then
    begin
      if messagedlg('Are you sure you want to launch DBVM? You seem to be running in 32-bit, so don''t really need it that badly',mtWarning,[mbno,mbyes],0)=mryes then
      begin
        launchdbvm;
        formshow(self);
      end;
    end
    else
    begin
      if loaddbvmifneeded then
      begin
        formshow(self);
      end;
    end;
  end;
end;

initialization
  {$i aboutunit.lrs}

end.
