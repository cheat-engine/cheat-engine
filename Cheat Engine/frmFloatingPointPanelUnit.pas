unit frmFloatingPointPanelUnit;

{$MODE Delphi}

{
This window will be used to display the floating point values of a context structure
}

interface

uses
  {$ifdef darwin}
  macport, math,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, cefuncproc, ComCtrls, LResources, NewKernelHandler,
  commonTypeDefs, betterControls, contexthandler;

resourcestring
  rsFPPExtended = 'Extended (default)';

type

  { TfrmFloatingPointPanel }

  TfrmFloatingPointPanel = class(TForm)
    cbClassicView: TCheckBox;
    PageControl1: TPageControl;
    sbData: TScrollBox;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    cbContextSection: TComboBox;
    cbDisplayType: TComboBox;
    mData: TMemo;
    procedure cbClassicViewChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label6DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    context: PContext;
    contexthandler: TContextInfo;

    loadedFormPosition: boolean;
    procedure ValueDoubleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    { Public declarations }
    procedure UpdatedContext;
    procedure SetContextPointer(context: pointer);
  end;


{$ifdef cpux86_64}
procedure doubletoextended(float64:pointer; outextended:pointer); assembler;
procedure extendedtodouble(float80:pointer;var outdouble:double); assembler;
{$endif}


var frmFloatingPointPanel:TfrmFloatingPointPanel;

implementation

uses MemoryBrowserFormUnit, processhandlerunit, debughelper, DPIHelper, networkInterfaceApi;

{$ifdef cpux86_64}
//coded by mgr.inz.player
procedure extendedtodouble(float80:pointer;var outdouble:double); assembler;
  var
    oldcw,newcw: word;
    _rcx: PtrUInt;
  asm
    mov _rcx,rcx
    fnstcw oldcw
    fwait
    mov cx,oldcw
    or  cx,$0c3f
    mov newcw,cx
    mov rcx,_rcx
    fldcw newcw
    fld tbyte [float80]
    fstp qword [outdouble]
    fwait
    fldcw oldcw
  end;

procedure doubletoextended(float64:pointer; outextended:pointer); assembler;
  var
    oldcw,newcw: word;
    _rcx: PtrUInt;
  asm
    mov _rcx,rcx
    fnstcw oldcw
    fwait
    mov cx,oldcw
    or  cx,$0c3f
    mov newcw,cx
    mov rcx,_rcx
    fldcw newcw
    fld qword [float64]
    fstp tbyte [outextended]
    fwait
    fldcw oldcw
  end;
{$endif}



procedure TfrmFloatingPointPanel.SetContextPointer(context: pointer);
var
  oldscrollpos: integer;
  oldcontexthandler: TContextInfo;
begin
  oldcontexthandler:=contexthandler;
  contexthandler:=getBestContextHandler;
  self.context:=context;

  oldscrollpos:=mData.VertScrollBar.Position;
  UpdatedContext;
  if mData.VertScrollBar.Range>mData.VertScrollBar.Position then
    mData.VertScrollBar.Position:=oldscrollpos;
end;

procedure TfrmFloatingPointPanel.ValueDoubleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  offset: integer;
  p: pointer;
  pba: pbytearray absolute p;
  pb: pbyte absolute p;
  pw: pword absolute p;
  pd: pdword absolute p;
  pq: Puint64 absolute p;
  ps: PSingle absolute p;
  pss: Pdouble absolute p;
  pe: PExtended absolute p;
  v: string;

  vd: double;

  e: PContextElement_register;
begin

  p:=nil;
  if self<>frmFloatingPointPanel then exit; //readonly for all other panels
  if button<>mbLeft then exit;

  if (debuggerthread=nil) or (debuggerthread.CurrentThread=nil) then exit;

  p:=pointer(tlabel(Sender).tag);

  if p=nil then exit;

  v:=tlabel(sender).caption;
  if inputquery('FPU Edit', 'Enter the new value', v) then
  begin
    case cbDisplayType.itemindex of
      0: pb^:=strtoint('$'+v);
      1: pw^:=strtoint('$'+v);
      2: pd^:=strtoint('$'+v);
      3: pq^:=StrToInt64('$'+v);
      4: ps^:=StrToFloat(v);
      5: pss^:=StrToFloat(v);
      6:
      begin
        {$ifdef cpux86_64}
        vd:=StrToFloat(v);
        doubleToExtended(@vd, p);
        {$else}
        pe^:=StrToFloat(v);
        {$endif}
      end;

    end;

    UpdatedContext;
  end;
end;

procedure TfrmFloatingPointPanel.UpdatedContext;
{
Called by the debugger and initial display
Will fetch the debuggerthread's context and show the floating point values
}
var i,j: integer;
    lastentry: integer;
    line, row: integer;
    lbl: tlabel;
    s: single;
    d: double;
    center: integer;
    temp: integer;
    e: extended;
    str: string;
    tempstr: string;
    regname: string;

    p: pointer;
    pba: pbytearray absolute p;
    pwa: pwordarray absolute p;
    pda: pdwordarray absolute p;
    pqa: Puint64Array absolute p;
    psa: PSingleArray absolute p;
    pssa: PdoubleArray absolute p;
    pea: PextendedArray absolute p;

    lw: longword;
    max: integer;
    bytelength: integer;

    oldscrollpos: integer;

    classic: boolean;

    blocksize: integer;

    oldcontexthandler: TContextInfo;

    FPUList: PContextElementRegisterList;
    altFPUList: PContextElementRegisterList;


    procedure newLabel(text: string; contextpointer: pointer);
    begin
      lbl:=tlabel.create(sbData);
      lbl.caption:=text;
      lbl.tag:=ptruint(contextpointer);
      lbl.parent:=sbData;
      if contextpointer<>nil then
      begin
        //lbl.OnDblClick:=ValueDoubleClick;
        lbl.OnMouseDown:=ValueDoubleClick;
      end;
    end;


begin
  if context=nil then exit;

  fpulist:=contexthandler.getFloatingPointRegisters;
  altFPUList:=contexthandler.getAlternateFloatingPointRegisters;
  if altFPUList=nil then
  begin
    //no alternate, only use the main one
    cbContextSection.enabled:=false;
    cbContextSection.visible:=false;
    cbContextSection.ItemIndex:=1;
  end
  else
  begin
    if not cbContextSection.enabled then
      cbContextSection.enabled:=true;

    if not cbContextSection.visible then
      cbContextSection.visible:=true;
  end;

  mData.lines.BeginUpdate;
  sbdata.BeginUpdateBounds;
  try
    mData.Clear;
    while sbdata.ComponentCount>0 do
      sbdata.Components[0].Free;

    case cbContextSection.ItemIndex of
      0: //fpu (old)
      begin
        if altFPUList=nil then exit;

        if altFPUList^[0].size=10 then //extended type
        begin
          if cbDisplayType.Items.Count=6 then
          begin
            cbDisplayType.Items.Add(rsFPPExtended);    //make it the default selection
            cbDisplayType.OnSelect:=nil;
            cbDisplayType.itemindex:=6;
            cbDisplayType.OnSelect:=ComboBox1Select;
          end;
        end;


        //if extended the other types are really useless, but it's there just in case...
        case cbDisplayType.ItemIndex of
          0: sbData.ChildSizing.ControlsPerLine:=1+altFPUList^[0].size; //byte
          1: sbData.ChildSizing.ControlsPerLine:=1+altFPUList^[0].size div 2; //word
          2: sbData.ChildSizing.ControlsPerLine:=1+altFPUList^[0].size div 4; //dword
          3: sbData.ChildSizing.ControlsPerLine:=1+altFPUList^[0].size div 8; //8 byte
          4: sbData.ChildSizing.ControlsPerLine:=1+altFPUList^[0].size div 4; //single
          5: sbData.ChildSizing.ControlsPerLine:=1+altFPUList^[0].size div 8; //double
          6: sbData.ChildSizing.ControlsPerLine:=1+1; //extended
        end;

        for i:=0 to length(altFPUList^)-1 do
        begin
          newlabel(altFPUList^[i].name+':',nil);
          p:=altFPUList^[i].getPointer(context);

          str:=altFPUList^[i].name+':';
          case cbDisplayType.ItemIndex of
            0: //byte
            begin
              for j:=0 to altFPUList^[i].size-1 do
              begin
                str:=str+inttohex(pba[j],2);
                if j<altFPUList^[i].size-1 then
                  str:=str+'  _  ';

                newLabel(inttohex(pba[j],2), pointer(ptruint(p)+j));
              end;

              mData.Lines.Add(str);
            end;

            1: //word
            begin
              lastentry:=(altFPUList^[i].size div 2)-1;

              for j:=0 to lastentry do
              begin
                str:=str+inttohex(pwa[j],2);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(inttohex(pwa[j],4), pointer(ptruint(p)+j*2));
              end;

              mData.Lines.Add(str);
            end;

            2:  //dword
            begin
              lastentry:=(altFPUList^[i].size div 4)-1;

              for j:=0 to lastentry do
              begin
                str:=str+inttohex(pda[j],2);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(inttohex(pda[j],8), pointer(ptruint(p)+j*4));
              end;

              mData.Lines.Add(str);
            end;

            3:  //qword
            begin
              lastentry:=(altFPUList^[i].size div 8)-1;

              for j:=0 to lastentry do
              begin
                str:=str+inttohex(pqa[j],2);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(inttohex(pqa[j],16), pointer(ptruint(p)+j*8));
              end;

              mData.Lines.Add(str);
            end;

            4: //single
            begin
              lastentry:=(altFPUList^[i].size div 4)-1;

              for j:=0 to lastentry do
              begin
                str:=str+format('%f',[psa[j]]);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(format('%f',[psa[j]]), pointer(ptruint(p)+j*4));
              end;

              mData.Lines.Add(str);
            end;

            5: //double
            begin
              lastentry:=(altFPUList^[i].size div 8)-1;

              for j:=0 to lastentry do
              begin
                str:=str+format('%f',[pssa[j]]);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(format('%f',[pssa[j]]), pointer(ptruint(p)+j*8));
              end;

              mData.Lines.Add(str);
            end;

            6:
            begin
              {$ifdef cpux86_64}
              extendedtodouble(p, d);
              {$else}
              d:=pea[0];
              {$endif}
              mData.Lines.Add(str+format('%f', [d])); //extended

              newLabel(format('%f',[d]), p);
            end;
          end;
        end;
      end;

      1: //xmm (main)
      begin
        if fpulist=nil then exit;

        if cbDisplayType.Items.Count>6 then //no extended type
        begin
          if cbDisplayType.ItemIndex=6 then
          begin
            cbDisplayType.OnSelect:=nil;
            cbDisplayType.ItemIndex:=5;
            cbDisplayType.OnSelect:=combobox1select;
          end;

          cbDisplayType.Items.Delete(6);
        end;

        bytelength:=fpulist^[0].size;
        max:=length(fpulist^);


        case cbDisplayType.ItemIndex of
          0: sbData.ChildSizing.ControlsPerLine:=1+bytelength; //byte
          1: sbData.ChildSizing.ControlsPerLine:=1+bytelength div 2; //word
          2: sbData.ChildSizing.ControlsPerLine:=1+bytelength div 4; //dword
          3: sbData.ChildSizing.ControlsPerLine:=1+bytelength div 8; //8 byte
          4: sbData.ChildSizing.ControlsPerLine:=1+bytelength div 4; //single
          5: sbData.ChildSizing.ControlsPerLine:=1+bytelength div 8; //double
          else
            sbData.ChildSizing.ControlsPerLine:=1+bytelength div 8;
        end;

        for i:=0 to max-1 do
        begin
          regname:=fpulist^[i].name;
          p:=fpulist^[i].getPointer(context);
          newLabel(regname+':', nil);

          str:=regname+':';

          case cbDisplayType.ItemIndex of
            0: //byte
            begin
              lastentry:=bytelength-1;
              for j:=0 to lastentry do
              begin
                str:=str+inttohex(pba[j],2);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(inttohex(pba[j],2), pointer(ptruint(p)+j));
              end;

              mData.Lines.Add(str);
            end;

            1: //word
            begin
              lastentry:=(bytelength div 2)-1;
              for j:=0 to lastentry do
              begin
                str:=str+inttohex(pwa[j],4);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(inttohex(pwa[j],4), pointer(ptruint(p)+j*2));
              end;

              mData.Lines.Add(str);
            end;

            2: //dword
            begin
              lastentry:=(bytelength div 4)-1;
              for j:=0 to lastentry do
              begin
                str:=str+inttohex(pda[j],8);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(inttohex(pda[j],8), pointer(ptruint(p)+j*4));
              end;

              mData.Lines.Add(str);
            end;

            3:   //8 byte
            begin
              lastentry:=(bytelength div 8)-1;
              for j:=0 to lastentry do
              begin
                str:=str+inttohex(pqa[j],16);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(inttohex(pqa[j],16), pointer(ptruint(p)+j*8));
              end;

              mData.Lines.Add(str);
            end;

            4: //float
            begin
              lastentry:=(bytelength div 4)-1;
              for j:=0 to lastentry do
              begin
                str:=str+format('%f',[psa[j]]);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(format('%f',[psa[j]]), pointer(ptruint(p)+j*4));
              end;

              mData.Lines.Add(str);
            end;

            5: //double
            begin
              lastentry:=(bytelength div 8)-1;
              for j:=0 to lastentry do
              begin
                str:=str+format('%f',[pssa[j]]);
                if j<lastentry then
                  str:=str+'  _  ';

                newLabel(format('%f',[pssa[j]]), pointer(ptruint(p)+j*8));
              end;

              mData.Lines.Add(str);
            end;
          end;
        end;
      end;
    end;
  finally
    mData.lines.endupdate;
    sbdata.EndUpdateBounds;
  end;
end;

procedure TfrmFloatingPointPanel.FormShow(Sender: TObject);
var w: integer;
begin
  if self<>frmFloatingPointPanel then //only show the new one on the memview version
  begin
    cbClassicView.checked:=true;
    //cbClassicView.visible:=false;
  end;

  mData.Font.Height:=GetFontData(font.Handle).Height;
  UpdatedContext;

  AdjustComboboxSize(cbContextSection, canvas);
  AdjustComboboxSize(cbDisplayType, canvas);

  w:=max(cbContextSection.Width, cbDisplayType.width)+16;
  cbContextSection.width:=w;
  cbDisplayType.width:=w;

  if loadedFormPosition=false then
    width:=max(width, canvas.TextWidth('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'));
end;

procedure TfrmFloatingPointPanel.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmFloatingPointPanel.cbClassicViewChange(Sender: TObject);
begin
  sbData.Visible:=not cbClassicView.checked;
end;

procedure TfrmFloatingPointPanel.ComboBox1Select(Sender: TObject);
begin
  UpdatedContext;
end;

procedure TfrmFloatingPointPanel.Label4Click(Sender: TObject);
begin

end;

procedure TfrmFloatingPointPanel.Label6DblClick(Sender: TObject);
begin

end;

procedure TfrmFloatingPointPanel.FormCreate(Sender: TObject);
begin
  cbDisplayType.ItemIndex:=4;
  loadedFormPosition:=LoadFormPosition(self);
  sbdata.font.color:=clWindowtext;

  mdata.font.Name:='Courier New';
//  mdata.font.size:=;
end;

initialization
  {$i frmFloatingPointPanelUnit.lrs}

end.
