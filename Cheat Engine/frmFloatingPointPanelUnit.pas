unit frmFloatingPointPanelUnit;

{$MODE Delphi}

{
This window will be used to display the floating point values of a context structure
}

interface

uses
  {jwawindows,} windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, cefuncproc, ComCtrls, LResources, NewKernelHandler, commonTypeDefs;

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
    contextCopy: TContext;
    procedure ValueDoubleClick(sender: TObject);
  public
    { Public declarations }
    procedure UpdatedContext;
    procedure SetContextPointer(context: PContext);
  end;


{$ifdef cpu64}
procedure doubletoextended(float64:pointer; outextended:pointer); assembler;
procedure extendedtodouble(float80:pointer;var outdouble:double); assembler;
{$endif}


var frmFloatingPointPanel:TfrmFloatingPointPanel;

implementation

uses MemoryBrowserFormUnit, processhandlerunit, debughelper;

{$ifdef cpu64}
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



procedure TfrmFloatingPointPanel.SetContextPointer(context: PContext);
var oldscrollpos: integer;
begin
  self.context:=context;
  self.contextCopy:=context^;

  oldscrollpos:=mData.VertScrollBar.Position;
  UpdatedContext;
  if mData.VertScrollBar.Range>mData.VertScrollBar.Position then
    mData.VertScrollBar.Position:=oldscrollpos;
end;

procedure TfrmFloatingPointPanel.ValueDoubleClick(sender: TObject);
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
begin
  p:=nil;
  if self<>frmFloatingPointPanel then exit; //readonly for all other panels

  if (debuggerthread=nil) or (debuggerthread.CurrentThread=nil) then exit;



  offset:=tlabel(Sender).tag;

  v:=tlabel(sender).caption;
  if inputquery('FPU Edit', 'Enter the new value', v) then
  begin
    case cbContextSection.itemindex of
      0:
      begin
        //fpu
        {$ifdef cpu64}
        p:=@debuggerthread.CurrentThread.context.FltSave.FloatRegisters[0];
        {$else}
        p:=@debuggerthread.CurrentThread.context.FloatSave.RegisterArea[0];
        {$endif}


      end;

      1:
      begin
        //xmm
        {$ifdef cpu64}
        p:=@debuggerthread.CurrentThread.context.FltSave.XmmRegisters[0];
        {$else}
        p:=@debuggerthread.CurrentThread.context.ext.XMMRegisters.LegacyXMM[0];
        {$endif}
      end;
    end;

    p:=@pba[offset];

    case cbDisplayType.itemindex of
      0: pb^:=strtoint('$'+v);
      1: pw^:=strtoint('$'+v);
      2: pd^:=strtoint('$'+v);
      3: pq^:=StrToInt64('$'+v);
      4: ps^:=StrToFloat(v);
      5: pss^:=StrToFloat(v);
      6:
      begin
        {$ifdef cpu64}
        vd:=StrToFloat(v);
        doubleToExtended(@vd, p);
        {$else}
        pe^:=StrToFloat(v);
        {$endif}
      end;

    end;


    context^:=debuggerthread.CurrentThread.context^;
    UpdatedContext;
  end;


end;

procedure TfrmFloatingPointPanel.UpdatedContext;
{
Called by the debugger and initial display
Will fetch the debuggerthread's context and show the floating point values
}
var i,j: integer;
    line, row: integer;
    lbl: tlabel;
    s: single;
    d: double;
    center: integer;
    temp: integer;
    e: extended;
    str: string;
    tempstr: string;

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

    oldscrollpos: integer;

    classic: boolean;

    procedure newLabel(text: string; id: integer);
    begin
      lbl:=tlabel.create(sbData);
      lbl.caption:=text;
      lbl.tag:=id;
      lbl.parent:=sbData;
      if id<>-1 then
        lbl.OnDblClick:=ValueDoubleClick;
    end;
begin
  if context=nil then exit;

  mData.lines.BeginUpdate;
  sbdata.BeginUpdateBounds;
  try
    mData.Clear;



    while sbdata.ComponentCount>0 do
      sbdata.Components[0].Free;




    case cbContextSection.ItemIndex of
      0: //fpu
      begin
        if cbDisplayType.Items.Count=6 then
        begin
          cbDisplayType.Items.Add(rsFPPExtended);    //make it the default selection
          cbDisplayType.OnSelect:=nil;
          cbDisplayType.itemindex:=6;
          cbDisplayType.OnSelect:=ComboBox1Select;
        end;

        case cbDisplayType.ItemIndex of
          0: sbData.ChildSizing.ControlsPerLine:=1+16; //byte
          1: sbData.ChildSizing.ControlsPerLine:=1+8; //word
          2: sbData.ChildSizing.ControlsPerLine:=1+4; //dword
          3: sbData.ChildSizing.ControlsPerLine:=1+2; //8 byte
          4: sbData.ChildSizing.ControlsPerLine:=1+4; //single
          5: sbData.ChildSizing.ControlsPerLine:=1+2; //double
          6: sbData.ChildSizing.ControlsPerLine:=1+1; //extended
        end;



        for i:=0 to 7 do
        begin
          newlabel('ST('+inttostr(i)+'):',-1);

          {$ifdef cpu64}
          p:=@context.FltSave.FloatRegisters[i];
          {$else}
          p:=@context.FloatSave.RegisterArea[i*10];
          {$endif}

          case cbDisplayType.ItemIndex of
            0: //byte
            begin
              str:='';


              for j:=0 to {$ifdef cpu64}15{$else}9{$endif} do
              begin
                str:=str+inttohex(pba[j],2);
                if j<15 then
                  str:=str+' _ ';

                newLabel(inttohex(pba[j],2), (i*{$ifdef cpu64}16{$else}10{$endif})+j);
              end;

              mData.Lines.Add(str);
            end;

            1:
            begin
              mData.Lines.Add(inttohex(pwa[0],4)+' _ '+inttohex(pwa[1],4)+' _ '+inttohex(pwa[2],4)+' _ '+inttohex(pwa[3],4)+' _ '+inttohex(pwa[4],4){$ifdef cpu64}+' _ '+inttohex(pwa[5],4)+' _ '+inttohex(pwa[6],4)+' _ '+inttohex(pwa[7],4){$endif}); //2byte
              for j:=0 to {$ifdef cpu64}7{$else}3{$endif} do
                newLabel(inttohex(pwa[j],4), (i*{$ifdef cpu64}16{$else}10{$endif})+(j*2));
            end;

            2:
            begin
              mData.Lines.Add(inttohex(pda[0],8)+' _ '+inttohex(pda[1],8){$ifdef cpu64}+' _ '+inttohex(pda[2],8)+' _ '+inttohex(pda[3],8){$endif}); //4byte
              for j:=0 to {$ifdef cpu64}3{$else}1{$endif} do
                newLabel(inttohex(pda[j],8), (i*{$ifdef cpu64}16{$else}10{$endif})+(j*4));
            end;

            3:
            begin
              mData.Lines.Add(inttohex(pqa[0],16){$ifdef cpu64}+' _ '+inttohex(pqa[1],16){$endif}); //8 byte
              for j:=0 to {$ifdef cpu64}1{$else}0{$endif} do
                newLabel(inttohex(pqa[j],17), (i*{$ifdef cpu64}16{$else}10{$endif})+(j*8));
            end;

            4:
            begin
              mData.Lines.Add(format('%f - %f'{$ifdef cpu64}+' - %f - %f'{$endif}, [psa[0], psa[1]{$ifdef cpu64}, psa[2], psa[3]{$endif}])); //single
              for j:=0 to {$ifdef cpu64}3{$else}1{$endif} do
                newLabel(format('%f',[psa[j]]),(i*{$ifdef cpu64}16{$else}10{$endif})+(j*4));
            end;

            5:
            begin
              mData.Lines.Add(format('%f'{$ifdef cpu64}+' - %f'{$endif}, [pssa[0]{$ifdef cpu64}, pssa[1]{$endif}]));  //double
              for j:=0 to {$ifdef cpu64}1{$else}0{$endif} do
                newLabel(format('%f',[pssa[j]]), (i*{$ifdef cpu64}16{$else}10{$endif})+(j*8));
            end;

            6:
            begin
              {$ifdef cpu64}
              extendedtodouble(p, d);
              {$else}
              d:=pea[0];
              {$endif}
              mData.Lines.Add(format('%f', [d])); //extended

              newLabel(format('%f',[d]), (i*{$ifdef cpu64}16{$else}10{$endif})); //(i*{$ifdef cpu64}16{$else}10{$endif}));

            end;
          end;
        end;
      end;

      1: //xmm
      begin

        if cbDisplayType.Items.Count>6 then
        begin
          if cbDisplayType.ItemIndex=6 then
          begin
            cbDisplayType.OnSelect:=nil;
            cbDisplayType.ItemIndex:=5;
            cbDisplayType.OnSelect:=combobox1select;
          end;

          cbDisplayType.Items.Delete(6);
        end;

        {$ifdef cpu64}
        if processhandler.is64bit then
          max:=15
        else
        {$endif}
          max:=7;

        case cbDisplayType.ItemIndex of
          0: sbData.ChildSizing.ControlsPerLine:=1+16; //byte
          1: sbData.ChildSizing.ControlsPerLine:=1+8; //word
          2: sbData.ChildSizing.ControlsPerLine:=1+4; //dword
          3: sbData.ChildSizing.ControlsPerLine:=1+2; //8 byte
          4: sbData.ChildSizing.ControlsPerLine:=1+4; //single
          5: sbData.ChildSizing.ControlsPerLine:=1+2; //double
          6: sbData.ChildSizing.ControlsPerLine:=1;
        end;

        for i:=0 to max do
        begin
          {$ifdef cpu64}
          p:=@context.FltSave.XmmRegisters[i];
          {$else}
          p:=@context.ext.XMMRegisters.LegacyXMM[i];
          {$endif}

          newLabel('XMM'+inttostr(i)+':', -1);

          case cbDisplayType.ItemIndex of
            0: //byte
            begin
              str:='';
              for j:=0 to 15 do
              begin
                str:=str+inttohex(pba[j],2);
                if j<15 then
                  str:=str+' _ ';

                newLabel(inttohex(pba[j],2), i*16+j);
              end;

              mData.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            1: //word
            begin
              str:='';
              for j:=0 to 7 do
              begin
                str:=str+inttohex(pwa[j],4);
                if j<7 then
                  str:=str+' _ ';

                newLabel(inttohex(pwa[j],4), i*16+j*2);
              end;

              mData.Lines.Add('xmm'+inttostr(i)+':'+str);


            end;

            2: //dword
            begin
              str:='';
              for j:=0 to 3 do
              begin
                str:=str+inttohex(pda[j],8);
                if j<3 then
                  str:=str+' _ ';

                newLabel(inttohex(pda[j],8), i*16+j*4);
              end;

              mData.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            3:   //8 byte
            begin
              str:='';
              for j:=0 to 1 do
              begin
                str:=str+inttohex(pqa[j],16);
                if j<1 then
                  str:=str+' _ ';

                newLabel(inttohex(pqa[j],16), i*16+j*8);
              end;

              mData.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            4:
            begin
              str:='';
              for j:=0 to 3 do
              begin
                str:=str+format('%f',[psa[j]]);
                if j<3 then
                  str:=str+' _ ';

                newLabel(format('%f',[psa[j]]), i*16+j*4);
              end;

              mData.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            5:
            begin
              str:='';
              for j:=0 to 1 do
              begin
                str:=str+format('%f',[pssa[j]]);
                if j<1 then
                  str:=str+' _ ';

                newLabel(format('%f',[pssa[j]]), i*16+j*8);
              end;

              mData.Lines.Add('xmm'+inttostr(i)+':'+str);
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
begin
  if self<>frmFloatingPointPanel then //only show the new one on the memview version
  begin
    cbClassicView.checked:=true;
    cbClassicView.visible:=false;
  end;

  mData.Font.Height:=GetFontData(font.Handle).Height;
  UpdatedContext;
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
  LoadFormPosition(self);
end;

initialization
  {$i frmFloatingPointPanelUnit.lrs}

end.
