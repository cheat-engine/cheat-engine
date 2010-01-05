unit frmFloatingPointPanelUnit;
{
This window will be used to display the floating point values of a context structure
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, debugger, ComCtrls, newkernelhandler;

type
  TfrmFloatingPointPanel = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    pnlFloatdata: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ComboBox1: TComboBox;
    Panel1: TPanel;
    ComboBox3: TComboBox;
    ComboBox2: TComboBox;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label6DblClick(Sender: TObject);
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

uses MemoryBrowserFormUnit;

{$R *.dfm}

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
begin
  if context=nil then exit;

  center:=pnlFloatdata.width div 2;
  label1.caption:='ControlWord:'+inttohex(context.FloatSave.ControlWord and $ffff,4);
  label3.caption:='StatusWord:'+inttohex(context.FloatSave.StatusWord and $ffff,4);
  label2.caption:='TagWord:'+inttohex(context.FloatSave.TagWord and $ffff,4);
  label8.caption:='Cr0NpxState:'+inttohex(context.FloatSave.Cr0NpxState,4);
  label4.caption:='ErrorOffset:'+inttohex(context.FloatSave.ErrorOffset,4);
  label5.caption:='ErrorSelector:'+inttohex(context.FloatSave.ErrorSelector and $ffff,4);
  label6.caption:='DataOffset:'+inttohex(context.FloatSave.DataOffset,4);
  label7.caption:='DataSelector:'+inttohex(context.FloatSave.DataSelector and $ffff,4);

  pnlFloatdata.Visible:=false;
  try
    while pnlFloatdata.ControlCount>0 do
    begin
      lbl:=tlabel(pnlFloatdata.Controls[0]);
      lbl.Free;
    end;

    memo1.Clear;
    case combobox3.ItemIndex of
      0: //fpu
      begin
        if combobox2.Items.Count=6 then
          combobox2.Items.Add('Extended');

        for i:=0 to 7 do
        begin
          case combobox2.ItemIndex of
            0: //byte
            begin
              str:='';
              for j:=0 to 7 do
              begin
                str:=str+inttohex(context.ext.FPURegisters[i].Data.MMRegister.bytes[j],2);
                if j<7 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add(str);
            end;
            1:  memo1.Lines.Add(inttohex(context.ext.FPURegisters[i].Data.MMRegister.words[0],4)+' - '+inttohex(context.ext.FPURegisters[i].Data.MMRegister.words[1],4)+' - '+inttohex(context.ext.FPURegisters[i].Data.MMRegister.words[2],4)+' - '+inttohex(context.ext.FPURegisters[i].Data.MMRegister.words[3],4)); //2byte
            2:  memo1.Lines.Add(inttohex(context.ext.FPURegisters[i].Data.MMRegister.dwords[0],8)+' - '+inttohex(context.ext.FPURegisters[i].Data.MMRegister.dwords[1],8)); //4byte
            3:  memo1.Lines.Add(inttohex(context.ext.FPURegisters[i].Data.MMRegister.qwords,16)); //8 byte
            4:  memo1.Lines.Add(floattostr(context.ext.FPURegisters[i].Data.MMRegister.Singles[0])+' - '+floattostr(context.ext.FPURegisters[i].Data.MMRegister.Singles[1])); //2 singles
            5:  memo1.Lines.Add(floattostr(context.ext.FPURegisters[i].Data.MMRegister.Doubles)); //double
            6:  memo1.Lines.Add(floattostr(context.ext.FPURegisters[i].Data.FloatValue)); //extended
          end;

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

              memo1.Lines.Add(str);
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

              memo1.Lines.Add(str);
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

              memo1.Lines.Add(str);
            end;

            3:   //8 byte
            begin
              str:='';
              for j:=0 to 3 do
              begin
                str:=str+inttohex(context.ext.XMMRegisters.LegacyXMM[i].qwords[j],16);
                if j<3 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add(str);
            end;

            4:
            begin
              str:='';
              for j:=0 to 3 do
              begin
                str:=str+floattostr(context.ext.XMMRegisters.LegacyXMM[i].singles[j]);
                if j<3 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add(str);
            end;

            5:
            begin
              str:='';
              for j:=0 to 1 do
              begin
                str:=str+floattostr(context.ext.XMMRegisters.LegacyXMM[i].doubles[j]);
                if j<1 then
                  str:=str+' - ';
              end;

              memo1.Lines.Add(str);
            end;
          end;

        end;
      end;
    end;


    case combobox1.ItemIndex of
      0: //byte
      begin
        line:=0;
        row:=0;
        for i:=0 to SIZE_OF_80387_REGISTERS-1 do
        begin
          if (i mod 10)=0 then
          begin
            //new line
            lbl:=tlabel.Create(self);
            lbl.Parent:=pnlFloatdata;
            lbl.font.Name:='Courier';
            lbl.top:=line*lbl.Height;
            lbl.caption:=format('%.2d-',[line*10]);

            inc(line);
            row:=0;
          end;

          lbl:=tlabel.Create(self);
          lbl.Parent:=pnlFloatdata;
          lbl.font.Name:='Courier';
          lbl.Top:=(line-1)*lbl.Height;
          lbl.left:=4+lbl.Canvas.TextWidth(' ')*3*(row+1);
          lbl.Caption:=inttohex(context.FloatSave.RegisterArea[i], 2);

          inc(row);
        end;
      end;

      1: //floating point value
      begin
        line:=0;
        i:=0;
        while i<SIZE_OF_80387_REGISTERS do
        begin

          lbl:=tlabel.Create(self);
          lbl.Parent:=pnlFloatdata;
          lbl.font.Name:='Courier';
          lbl.Top:=line*lbl.Height;
          lbl.left:=0;

          try
            e:=pextended(@(context.FloatSave.RegisterArea[i]))^;
            lbl.Caption:=format('%f', [e]);
          except
            lbl.Caption:='...';
          end;

          inc(i,10);
          inc(line);
        end;
      end;


   {   1: //dword
      begin
        line:=0;
        row:=0;
        i:=0;
        while i<SIZE_OF_80387_REGISTERS do
        begin

          lbl:=tlabel.Create(self);
          lbl.Parent:=pnlFloatdata;
          lbl.font.Name:='Courier';
          lbl.Top:=line*lbl.Height;
          if row=0 then //only 2 rows...
            lbl.left:=0
          else
            lbl.left:=center;
              
          lbl.Caption:=inttohex(line*8+row,2)+'-'+inttohex(pdword(@(context.FloatSave.RegisterArea[i]))^, 8);

          inc(row,4);
          row:=row mod 8;
          if row=0 then
            inc(line);
          inc(i,4);
        end;
      end; }

   {   2: //float
      begin
        line:=0;
        row:=0;
        i:=0;
        while i<SIZE_OF_80387_REGISTERS do
        begin

          lbl:=tlabel.Create(self);
          lbl.Parent:=pnlFloatdata;
          lbl.font.Name:='Courier';
          lbl.Top:=line*lbl.Height;
          if row=0 then //only 2 rows...
            lbl.left:=0
          else
            lbl.left:=center;

          try
            s:=psingle(@(context.FloatSave.RegisterArea[i]))^;
            lbl.Caption:=inttohex(line*8+row,2)+'-'+format('%5.5f', [s]);
          except
            lbl.Caption:='...';
          end;

          if row=0 then
          begin
            if lbl.Width>center then
            begin
              //not enough room. Truncate
              temp:=center div lbl.Canvas.TextWidth(' ');
              lbl.caption:=copy(lbl.Caption,1,temp-4)+'...';
            end;
          end;

          //lbl.Width:=lbl.Canvas.TextWidth(' ')*12;

          inc(row,4);
          row:=row mod 8;
          if row=0 then
            inc(line);
          inc(i,4);
        end;
      end; }

     { 3: //double
      begin
        line:=0;
        i:=0;
        while i<SIZE_OF_80387_REGISTERS do
        begin

          lbl:=tlabel.Create(self);
          lbl.Parent:=pnlFloatdata;
          lbl.font.Name:='Courier';
          lbl.Top:=line*lbl.Height;
          lbl.left:=0;

          try
            d:=pdouble(@(context.FloatSave.RegisterArea[i]))^;
            lbl.Caption:=inttohex(line*8,2)+'-'+format('%5.5f', [d]);
          except
            lbl.Caption:='...';
          end;

          inc(i,8);
          inc(line);
        end;
      end; }


    end;
  finally
    pnlFloatdata.Visible:=true;
  end;
end;

procedure TfrmFloatingPointPanel.FormShow(Sender: TObject);
begin
  UpdatedContext;
end;

procedure TfrmFloatingPointPanel.ComboBox1Select(Sender: TObject);
begin
  UpdatedContext;
end;

procedure TfrmFloatingPointPanel.Label4Click(Sender: TObject);
begin
  memorybrowser.disassemblerview.SelectedAddress:=self.contextCopy.FloatSave.ErrorOffset;
end;

procedure TfrmFloatingPointPanel.Label6DblClick(Sender: TObject);
begin
  memorybrowser.memoryaddress:=self.contextCopy.FloatSave.DataOffset;
  memorybrowser.refreshmb;
end;

end.
