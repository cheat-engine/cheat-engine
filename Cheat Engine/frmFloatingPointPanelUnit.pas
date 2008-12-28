unit frmFloatingPointPanelUnit;
{
This window will be used to display the floating point values of a context structure
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, debugger;

type
  TfrmFloatingPointPanel = class(TForm)
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
    procedure FormShow(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
  private
    { Private declarations }
    context: PContext;
  public
    { Public declarations }
    procedure UpdatedContext;
    procedure SetContextPointer(context: PContext);
  end;

var frmFloatingPointPanel:TfrmFloatingPointPanel;

implementation

{$R *.dfm}

procedure TfrmFloatingPointPanel.SetContextPointer(context: PContext);
begin
  self.context:=context;
end;

procedure TfrmFloatingPointPanel.UpdatedContext;
{
Called by the debugger and initial display
Will fetch the debuggerthread's context and show the floating point values
}
var i: integer;
    line, row: integer;
    lbl: tlabel;
    s: single;
    d: double;
    center: integer;
    temp: integer;
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


    case combobox1.ItemIndex of
      0: //byte
      begin
        line:=0;
        row:=0;
        for i:=0 to SIZE_OF_80387_REGISTERS-1 do
        begin
          if (i mod 8)=0 then
          begin
            //new line
            lbl:=tlabel.Create(self);
            lbl.Parent:=pnlFloatdata;
            lbl.font.Name:='Courier';
            lbl.top:=line*lbl.Height;
            lbl.caption:=inttohex(line*8,2)+'-';

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

      1: //dword
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
      end;

      2: //float
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
      end;

      3: //double
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
      end;


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

end.
