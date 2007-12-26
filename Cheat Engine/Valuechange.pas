unit Valuechange;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls
  {$ifdef net}
  ,netapis,CEClient;
  {$else}
  ,newkernelhandler, CEFuncProc;
  {$endif}
                                       
type
  TValueChangeForm = class(TForm)
    VarType: TComboBox;
    Button1: TButton;
    Button2: TButton;
    ValueText: TEdit;
    cbunicode: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure VarTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    faddress: Dword;
    fvartype: byte;
    procedure Updatevalue;
    procedure setaddress(x: dword);
    procedure setvartype(x: byte);

    procedure setunicode(x:boolean);
    function getunicode:boolean;
  public
    { Public declarations }

    slength: integer;
    property Address: dword read faddress write setaddress;
    property vtype: byte read fvartype write setvartype;
    property unicode: boolean read getunicode write setunicode;
  end;

var
  ValueChangeForm: TValueChangeForm;

implementation

{$R *.DFM}

function TValueChangeForm.getunicode:boolean;
begin
  result:=cbunicode.checked;
end;


procedure TValueChangeForm.setunicode(x: boolean);
begin
  cbunicode.checked:=x;
end;

procedure TValueChangeForm.setaddress(x: dword);
begin
  faddress:=x;
  caption:='Change offset '+inttohex(x,8);  
  updatevalue;
end;

procedure TValuechangeForm.setvartype(x: byte);
begin
  fvartype:=x;

  case x of
    0: vartype.itemindex:=0;  //byte
    1: vartype.itemindex:=1;  //word
    2: vartype.itemindex:=2;  //dword
    6: vartype.itemindex:=3;  //int64
    4: vartype.itemindex:=4;  //float
    5: vartype.itemindex:=5;  //double
    7: vartype.itemindex:=6;  //text
  end;

  updatevalue;
end;

procedure TValueChangeForm.UpdateValue;
var value1: Byte;
    value2: word;
    value3: dword;
    value4: single;
    value5: double;
    value6: Int64;

    read: dword;

    s: pchar;
    ws: pwchar;

begin
  value4:=0;
  value5:=0;

  case vartype.itemindex of
      0 : begin //byte
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value1),1,read);            
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value1),1,read);
            {$endif}
            valuetext.text:=IntToStr(value1);

            cbunicode.visible:=true;
          end;

      1 : begin //word
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value2),2,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value2),2,read);
            {$endif}

            valuetext.text:=IntToStr(value2);

            cbunicode.visible:=true;
          end;

      2 : begin  //dword
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value3),4,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value3),4,read);
            {$endif}
            valuetext.text:=IntToStr(value3);

            cbunicode.visible:=true;
          end;

      3 : begin //int64
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value6),8,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value6),8,read);
            {$endif}
            valuetext.text:=IntToStr(value6);

            cbunicode.visible:=true;
          end;

      4 : begin //float
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value4),4,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value4),4,read);
            {$endif}
            valuetext.text:=FloatToStr(value4);

            cbunicode.visible:=true;
          end;

      5 : begin //double
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value5),8,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value5),8,read);
            {$endif}
            valuetext.text:=floatToStr(value5);

            cbunicode.visible:=true;
          end;

      6 : begin //text
            if unicode then
            begin
              getmem(ws,slength*2+2);
              {$ifdef net}
              readprocessmemorynet(0,pointer(address),ws,slength*2,read);
              {$else}
              readprocessmemory(processhandle,pointer(address),ws,slength*2,read);
              {$endif}

              ws[slength]:=#0;
              ValueText.text:=ws;

              freemem(ws);

            end
            else
            begin
              getmem(s,slength+1);
              {$ifdef net}
              readprocessmemorynet(0,pointer(address),s,slength,read);
              {$else}
              readprocessmemory(processhandle,pointer(address),s,slength,read);
              {$endif}

              s[slength]:=#0;
              ValueText.text:=s;

              freemem(s);
            end;

            cbunicode.visible:=true;
          end;

      7 : begin
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value1),1,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value1),1,read);
            {$endif}
            ValueText.text:=inttohex(value1,2);

            cbunicode.visible:=false;
          end;
  end;
end;

procedure TValueChangeForm.Button2Click(Sender: TObject);
begin
  modalresult:=mrcancel;
end;

procedure TValueChangeForm.FormShow(Sender: TObject);
begin
  ValuechangeForm.caption:='Change Offset: '+IntToHex(address,8);
//  vartype.ItemIndex:=0;
  updatevalue;

  valuetext.SetFocus;
  valuetext.SelectAll;
end;

procedure TValueChangeForm.Button1Click(Sender: TObject);
var write: dword;
    newvalue1: byte;
    newvalue2: word;
    newvalue3: dword;
    newvalue4: single;
    newvalue5: double;
    newvalue6: int64;

    {$ifndef net}
    newvalue7: Tbytes;
    {$endif}
    newstring: array of byte;
    i: Integer;

    newvaluest:string;
begin
  val(valuetext.text,newvalue1,write);
  val(valuetext.text,newvalue2,write);
  val(valuetext.text,newvalue3,write);

  val(valuetext.text,newvalue4,write);
  val(valuetext.text,newvalue5,write);
  val(valuetext.text,newvalue6,write);

  newvaluest:=valuetext.text;


  case vartype.itemindex of
  {$ifdef net}
{byte}  0       : writeprocessmemorynet(0,pointer(address),addr(newvalue1),1,write);
{word}  1       : writeprocessmemorynet(0,pointer(address),addr(newvalue2),2,write);
{dword} 2       : writeprocessmemorynet(0,pointer(address),addr(newvalue3),4,write);
{int64} 3       : writeprocessmemorynet(0,pointer(address),addr(newvalue6),8,write);
{float} 4       : writeprocessmemorynet(0,pointer(address),addr(newvalue4),4,write);
{double}5       : writeprocessmemorynet(0,pointer(address),addr(newvalue5),8,write);
  {$else}
{byte}  0       : writeprocessmemory(processhandle,pointer(address),addr(newvalue1),1,write);
{word}  1       : writeprocessmemory(processhandle,pointer(address),addr(newvalue2),2,write);
{dword} 2       : writeprocessmemory(processhandle,pointer(address),addr(newvalue3),4,write);
{int64} 3       : writeprocessmemory(processhandle,pointer(address),addr(newvalue6),8,write);
{float} 4       : begin

                    if write<>0 then
                    begin

                      if newvaluest[write]=',' then newvaluest[write]:='.'
                      else
                      if newvaluest[write]='.' then newvaluest[write]:=',';

                      write:=0;
                      val(newvaluest,newvalue4,write);
                    end;

                    writeprocessmemory(processhandle,pointer(address),addr(newvalue4),4,write);
                  end;

{double}5       : begin
                    if write<>0 then
                    begin
                      if newvaluest[write]=',' then newvaluest[write]:='.'
                      else
                      if newvaluest[write]='.' then newvaluest[write]:=',';

                      write:=0;
                      val(newvaluest,newvalue5,write);
                    end;

                    writeprocessmemory(processhandle,pointer(address),addr(newvalue5),8,write);
                  end;
  {$endif}
{byte}  6       : begin
                    setlength(newstring,length(ValueText.text));
                    for i:=1 to length(ValueText.text) do
                      newstring[i-1]:=ord(ValueText.text[i]);

                    {$ifdef net}
                    writeprocessmemorynet(0,pointer(address),newstring,length(ValueText.text),write);
                    {$else}
                    writeprocessmemory(processhandle,pointer(address),newstring,length(ValueText.text),write);
                    {$endif}

                  end;

{$ifndef net}

{bytes} 7       : begin
//convert the string to bytes
                    ConvertStringToBytes(Valuetext.Text,true,newvalue7);
                    setlength(newstring,length(newvalue7));
                    for i:=0 to length(newvalue7)-1 do
                    begin
                      //if the bytesvalue is -1 then read that byte from the memory and leave it untouched
                      //if it is unreadable popup a message noptifying the user
                      if newvalue7[i]=-1 then
                      begin
                        write:=0;
                        readprocessmemory(processhandle,pointer(address+i),addr(newstring[i]),1,write);
                        if write<>1 then raise exception.Create('Part of the string is unreadable!');
                      end
                      else
                        newstring[i]:=newvalue7[i];

                      //newstring now contains all the bytes we need so write them to the memory

                    end;
                    writeprocessmemory(processhandle,pointer(address),newstring,length(newstring),write);
                  end;
{$endif}
  end;

  modalresult:=mrok;
end;

procedure TValueChangeForm.VarTypeChange(Sender: TObject);
begin
  updatevalue;
end;

procedure TValueChangeForm.FormCreate(Sender: TObject);
begin
  vartype.Items.Delete(vartype.Items.Count-1);
  vartype.itemindex:=0;
end;

procedure TValueChangeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caFree;
end;

end.
