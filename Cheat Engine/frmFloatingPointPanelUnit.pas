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
  rsFPPExtended = 'Extended';

type
  TfrmFloatingPointPanel = class(TForm)
    PageControl1: TPageControl;
    TabSheet2: TTabSheet;
    Panel1: TPanel;
    ComboBox3: TComboBox;
    ComboBox2: TComboBox;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label6DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    context: PContext;
    contextCopy: TContext;
  public
    { Public declarations }
    procedure UpdatedContext;
    procedure SetContextPointer(context: PContext);
  end;

var frmFloatingPointPanel:TfrmFloatingPointPanel;

implementation

uses MemoryBrowserFormUnit, processhandlerunit;

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
{$endif}



procedure TfrmFloatingPointPanel.SetContextPointer(context: PContext);
begin
  self.context:=context;
  self.contextCopy:=context^;
  UpdatedContext;
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

begin
  if context=nil then exit;

    memo1.Clear;
    case combobox3.ItemIndex of
      0: //fpu
      begin


        if combobox2.Items.Count=6 then
          combobox2.Items.Add(rsFPPExtended);

        for i:=0 to 7 do
        begin
          {$ifdef cpu64}
          p:=@context.FltSave.FloatRegisters[i];

          case combobox2.ItemIndex of
            0: //byte
            begin
              str:='';
              for j:=0 to 15 do
              begin
                str:=str+inttohex(pba[j],2);
                if j<15 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add(str);

            end;

            1:  memo1.Lines.Add(inttohex(pwa[0],4)+' - '+inttohex(pwa[1],4)+' - '+inttohex(pwa[2],4)+' - '+inttohex(pwa[3],4)+' - '+inttohex(pwa[4],4)+' - '+inttohex(pwa[5],4)+' - '+inttohex(pwa[6],4)+' - '+inttohex(pwa[7],4)); //2byte
            2:  memo1.Lines.Add(inttohex(pda[0],8)+' - '+inttohex(pda[1],8)+' - '+inttohex(pda[2],8)+' - '+inttohex(pda[3],8)); //4byte
            3:  memo1.Lines.Add(inttohex(context.FltSave.FloatRegisters[i].Low,16)+' - '+inttohex(context.FltSave.FloatRegisters[i].High,16)); //8 byte
            4:  memo1.Lines.Add(format('%f - %f - %f - %f', [psa[0], psa[1], psa[2], psa[3]])); //single
            5:  memo1.Lines.Add(format('%f - %f', [pssa[0], pssa[1]]));  //double
            6:
            begin
              extendedtodouble(p, d);
              memo1.Lines.Add(format('%f', [d])); //extended
            end;
          end;
          {$else}
          p:=@context.FloatSave.RegisterArea[i*10];

          case combobox2.ItemIndex of
            0: //byte
            begin
              str:='';
              for j:=0 to 9 do
              begin
                str:=str+inttohex(pba[j],2);
                if j<9 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add(str);

            end;

            1:  memo1.Lines.Add(inttohex(pwa[0],4)+' - '+inttohex(pwa[1],4)+' - '+inttohex(pwa[2],4)+' - '+inttohex(pwa[3],4)+' - '+inttohex(pwa[4],4)); //2byte
            2:  memo1.Lines.Add(inttohex(pda[0],8)+' - '+inttohex(pda[1],8)); //4byte
            3:  memo1.Lines.Add(inttohex(pqa[0],16)); //8 byte
            4:  memo1.Lines.Add(format('%f - %f', [psa[0], psa[1]])); //single
            5:  memo1.Lines.Add(format('%f', [pssa[0]]));  //double
            6:  memo1.Lines.Add(format('%f', [pea[0]]));  //extended
          end;

          {$endif}
        end;
      end;

      1: //xmm
      begin

        if combobox2.Items.Count>6 then
        begin
          if combobox2.ItemIndex=6 then
            combobox2.ItemIndex:=5;

          combobox2.Items.Delete(6);
        end;

        {$ifdef cpu64}
        if processhandler.is64bit then
          max:=15
        else
          max:=7;

        for i:=0 to max do
        begin

          p:=@context.FltSave.XmmRegisters[i];


          case combobox2.ItemIndex of
            0: //byte
            begin
              str:='';
              for j:=0 to 15 do
              begin
                str:=str+inttohex(pba[j],2);
                if j<15 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            1: //word
            begin
              str:='';
              for j:=0 to 7 do
              begin
                str:=str+inttohex(pwa[j],4);
                if j<7 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            2: //dword
            begin
              str:='';
              for j:=0 to 3 do
              begin
                str:=str+inttohex(pda[j],8);
                if j<3 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            3:   //8 byte
            begin
              str:='';
              for j:=0 to 1 do
              begin
                str:=str+inttohex(pqa[j],16);
                if j<1 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            4:
            begin
              str:='';
              for j:=0 to 3 do
              begin
                str:=str+format('%f',[psa[j]]);
                if j<3 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            5:
            begin
              str:='';
              for j:=0 to 1 do
              begin
                str:=str+format('%f',[pssa[j]]);
                if j<1 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;
          end;


        end;
        {$else}
        //memo1.lines.add('Not implemented in the 32-bit version yet');

        for i:=0 to 7 do
        begin
          case combobox2.ItemIndex of
            0: //byte
            begin
              str:='';
              for j:=0 to 15 do
              begin
                str:=str+inttohex(context.ext.XMMRegisters.LegacyXMM[i].Bytes[j],2);
                if j<15 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            1: //word
            begin
              str:='';
              for j:=0 to 7 do
              begin
                str:=str+inttohex(context.ext.XMMRegisters.LegacyXMM[i].words[j],4);
                if j<7 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            2: //dword
            begin
              str:='';
              for j:=0 to 3 do
              begin
                str:=str+inttohex(context.ext.XMMRegisters.LegacyXMM[i].dwords[j],8);
                if j<3 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            3:   //8 byte
            begin
              str:='';
              for j:=0 to 1 do
              begin
                str:=str+inttohex(context.ext.XMMRegisters.LegacyXMM[i].qwords[j],16);
                if j<1 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            4:
            begin
              str:='';
              for j:=0 to 3 do
              begin
                str:=str+format('%f',[context.ext.XMMRegisters.LegacyXMM[i].singles[j]]);
                if j<3 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;

            5:
            begin
              str:='';
              for j:=0 to 1 do
              begin
                str:=str+format('%f',[context.ext.XMMRegisters.LegacyXMM[i].doubles[j]]);
                if j<1 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add('xmm'+inttostr(i)+':'+str);
            end;
          end;
         end;



        {$endif}


      end;
    end;


end;

procedure TfrmFloatingPointPanel.FormShow(Sender: TObject);
begin
  memo1.Font.Height:=GetFontData(font.Handle).Height;
  UpdatedContext;
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
  combobox2.ItemIndex:=4;
end;

initialization
  {$i frmFloatingPointPanelUnit.lrs}

end.
