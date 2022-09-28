unit formFoundcodeListExtraUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$else}
  windows,
  {$endif}
  LResources, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Menus,Clipbrd, ExtCtrls, Buttons,
  frmFloatingPointPanelUnit, NewKernelHandler,cefuncproc, frmStackViewUnit, betterControls;

type

  { TFormFoundCodeListExtra }

  TFormFoundCodeListExtra = class(TForm)
    eiImageList: TImageList;
    lblGSBaseKernel: TLabel;
    lblGSBase: TLabel;
    lblCR3: TLabel;
    Label18: TLabel;
    lblPhysicalAddress: TLabel;
    lblVirtualAddress: TLabel;
    lblFSBase: TLabel;
    lblRAX: TLabel;
    lblRBP: TLabel;
    lblRBX: TLabel;
    lblRCX: TLabel;
    lblRDI: TLabel;
    lblRDX: TLabel;
    lblRIP: TLabel;
    lblRSI: TLabel;
    lblRSP: TLabel;
    pnlEPTWatch: TPanel;
    pnlRegisters: TPanel;
    Panel8: TPanel;
    pmCopy: TPopupMenu;
    Copyaddresstoclipboard1: TMenuItem;
    Panel1: TPanel;
    Label10: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label6: TLabel;
    Label17: TLabel;
    Panel5: TPanel;
    Button1: TButton;
    Panel6: TPanel;
    pmCopy2: TPopupMenu;
    Copyguesstoclipboard1: TMenuItem;
    pmEmpty: TPopupMenu;
    sbShowFloats: TSpeedButton;
    sbShowStack: TSpeedButton;
    sbShowIPT: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Copyaddresstoclipboard1Click(Sender: TObject);
    procedure Copyguesstoclipboard1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label1DblClick(Sender: TObject);
    procedure RegisterDblClick(Sender: TObject);
    procedure RegisterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel6Resize(Sender: TObject);
    procedure sbShowIPTClick(Sender: TObject);
    procedure sbShowStackClick(Sender: TObject);
    procedure sbShowFloatsClick(Sender: TObject);
  private
    { Private declarations }
    fIs64bit: boolean;
    fprobably: ptrUint;
    fpp:       TfrmFloatingPointPanel;
    stackview: TfrmStackView;
    procedure setprobably(address:ptrUint);
  public
    { Public declarations }
    context: PContext;  //needs to free this on destroy
    stack: record
      savedsize: dword;
      stack: pbyte;
    end;

    ipt: record
      log: pointer;
      size: integer;
    end;

    lblR8: tlabel;
    lblR9: tlabel;
    lblR10: tlabel;
    lblR11: tlabel;
    lblR12: tlabel;
    lblR13: tlabel;
    lblR14: tlabel;
    lblR15: tlabel;
    lblR16: tlabel; //arm
    lblR17: tlabel;
    property probably: ptrUint read fprobably write setprobably;
  end;



implementation

uses MemoryBrowserFormUnit, ProcessHandlerUnit, globals, debughelper,
  DebuggerInterfaceAPIWrapper, IPTLogDisplay, disassembler;

resourcestring
  rsTheValueOfThePointerNeededToFindThisAddressIsProba = 'The value of the pointer needed to find this address is probably %s';
  rsProbableBasePointer = 'Probable base pointer =%s';
  rsNeedsIPTFindWhat = 'The debugger did not collect any IPT data. This may be due to the Intel PT feature not being enabled in settings/malfunctioning, or that the option to also log the trace in "find what..." results was disabled. Do you wish to enable this for this debugging session? You will need to recollect this information again though.'#13#10'(This is for this session only. If you wish to always turn it on from the start, go to settings->debugger options, and enable "Use Intel-PT feature" and the suboption for recording elements in "find what ..." routines)';

procedure TFormFoundCodeListExtra.setprobably(address: ptrUint);
begin
  fprobably:=address;
  Label17.Caption:=Format(rsTheValueOfThePointerNeededToFindThisAddressIsProba, [IntToHex(address, 8)]);
end;


procedure TFormFoundCodeListExtra.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Stackview<>nil then
    freeandnil(Stackview);

  if fpp<>nil then
    freeandnil(fpp);

  if stack.stack<>nil then
    freememandnil(stack.stack);
    
  action:=cafree;
end;

procedure TFormFoundCodeListExtra.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TFormFoundCodeListExtra.Copyaddresstoclipboard1Click(
  Sender: TObject);
begin
end;

procedure TFormFoundCodeListExtra.Copyguesstoclipboard1Click(
  Sender: TObject);
var
s: string;
begin
  s:=inttohex(probably,8);
  clipboard.SetTextBuf(pchar(s));
end;

procedure setFontColor(control: TWinControl; color: TColor);
var i: integer;
begin
  for i:=0 to control.ControlCount-1 do
  begin
    control.controls[i].Font.color:=color;
    if control.Controls[i] is twincontrol then setfontcolor(twincontrol(control.controls[i]),color);
  end;
end;

procedure TFormFoundCodeListExtra.FormCreate(Sender: TObject);
var x: array of integer;
begin
  setlength(x,0);
  loadformposition(self,x);

  Font.Color:=clWindowtext;
  setFontColor(self, clWindowtext);

  sbShowIPT.visible:=systemSupportsIntelPT and not hideiptcapability and (CurrentDebuggerInterface<>nil) and CurrentDebuggerInterface.canUseIPT;
end;

procedure TFormFoundCodeListExtra.FormDestroy(Sender: TObject);
begin
  if stackview<>nil then
    stackview.free;

  if stack.stack<>nil then
    freememandnil(stack.stack);

  if fpp<>nil then
    fpp.Free;

  if context<>nil then
    freememandnil(context);

  if ipt.log<>nil then
    freememandnil(ipt.log);

  saveformposition(self);
end;

procedure TFormFoundCodeListExtra.FormShow(Sender: TObject);
begin

  panel1.Font.Height:=GetFontData(font.reference.Handle).Height-5;
  pnlRegisters.Font.Height:=panel1.Font.Height;

  label3.parentfont:=false;
  label10.parentfont:=false;
  label3.font.assign(pnlRegisters.font);
  label10.font.assign(pnlRegisters.font);

  label3.font.color:=clRed;
  label10.font.color:=clRed;

  Constraints.MaxHeight:=panel5.Top+panel5.height+10;
  Constraints.MinHeight:=Constraints.MaxHeight;

  autosize:=false;
end;

procedure TFormFoundCodeListExtra.Label1DblClick(Sender: TObject);
begin
  if (sender is tlabel) then
  begin
    memorybrowser.disassemblerview.SelectedAddress:=tlabel(sender).tag;
    memorybrowser.show;
  end;
end;

procedure TFormFoundCodeListExtra.RegisterDblClick(Sender: TObject);
var s: string;
i: integer;
begin
  if (sender is TLabel) then
  begin
    s:=tlabel(sender).Caption;
    i:=pos('=',s);
    if i>0 then //should always be true
    begin
      s:=copy(s,i+1,length(s));

      memorybrowser.hexview.address:=StrToInt64('$'+s);
      memorybrowser.show;
    end;
  end;

end;


procedure TFormFoundCodeListExtra.RegisterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s: string;
i: integer;
begin
  if button = mbright then
  begin
    if (sender is TLabel) then
    begin
      s:=tlabel(sender).Caption;
      i:=pos('=',s);
      if i>0 then //should always be true
      begin
        s:=copy(s,i+1,length(s));

        clipboard.AsText:=s;
      end;
    end;
  end;
end;

procedure TFormFoundCodeListExtra.Panel6Resize(Sender: TObject);
begin

end;

procedure TFormFoundCodeListExtra.sbShowIPTClick(Sender: TObject);
var
  f: TfrmIPTLogDisplay;
begin
  {$IFDEF WINDOWS}
  if (ipt.log=nil) then
  begin
    if debuggerthread<>nil then
    begin
      if (useintelptfordebug=false) or (inteliptlogfindwhatroutines=false) then
      begin
        if messagedlg(rsNeedsIPTFindWhat, mtConfirmation, [mbyes,mbno],0)=mryes then
        begin
          useintelptfordebug:=true;
          inteliptlogfindwhatroutines:=true;
          debuggerthread.initIntelPTTracing;
        end;
      end;
    end;
  end
  else
  begin
    f:=TfrmIPTLogDisplay.create(application);
    f.show;
    f.loadlog('log'+GetTickCount64.ToHexString, ipt.log, ipt.size, context^.{$ifdef cpu64}Rip{$else}Eip{$endif});
  end;
  {$ENDIF}
end;

procedure TFormFoundCodeListExtra.sbShowStackClick(Sender: TObject);
begin
  if stack.stack=nil then exit;

  if Stackview=nil then
    stackview:=TfrmStackView.create(self);

  stackview.SetContextPointer(@context, stack.stack, stack.savedsize);
  stackview.show;
end;

procedure TFormFoundCodeListExtra.sbShowFloatsClick(Sender: TObject);
begin
  if fpp=nil then
    fpp:=TfrmFloatingPointPanel.create(self);

  fpp.Left:=self.left+self.Width;
  fpp.Top:=self.top;
  fpp.SetContextPointer(context);
  fpp.show;//pop to foreground
end;

initialization
  {$i formFoundcodeListExtraUnit.lrs}

end.

