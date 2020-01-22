unit Valuechange;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, LResources, ExtCtrls, commonTypeDefs,NewKernelHandler, CEFuncProc;
                                       
type

  { TValueChangeForm }

  TValueChangeForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbVarType: TComboBox;
    Panel1: TPanel;
    ValueText: TEdit;
    cbunicode: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure cbunicodeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure cbVarTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    faddress: ptrUint;
    fvartype: byte;
    fvartype2: Tvariabletype;
    procedure Updatevalue;
    procedure setaddress(x: ptrUint);
    procedure setvartype(x: byte);
    procedure setvartype2(vt: TVariableType);
    function getVartype: TVariabletype;

    procedure setunicode(x:boolean);
    function getunicode:boolean;
  public
    { Public declarations }

    slength: integer;
    property Address: ptrUint read faddress write setaddress;
    property vtype: byte read fvartype write setvartype;
    property VarType: TVariableType read getVartype write setVartype2;
    property unicode: boolean read getunicode write setunicode;
  end;

var
  ValueChangeForm: TValueChangeForm;

implementation

uses ProcessHandlerUnit, parsers;

resourcestring
  rsChangeOffset = 'Change offset %s';
  rsPartOfTheStringIsUnreadable = 'Part of the string is unreadable!';
  rsUnicode = 'Unicode';


function TValueChangeForm.getunicode:boolean;
begin
  result:=cbunicode.checked;
end;


procedure TValueChangeForm.setunicode(x: boolean);
begin
  cbunicode.checked:=x;
end;

procedure TValueChangeForm.setaddress(x: ptrUint);
begin
  faddress:=x;
  caption:=Format(rsChangeOffset, [inttohex(x, 8)]);
  updatevalue;
end;

procedure TValuechangeForm.setvartype2(vt: TVariableType);
//9/23/2011: adding support for the 'new' type... (really old code here)
begin

  case vt of
    vtByte: cbVarType.itemindex:=0;
    vtWord: cbVarType.itemindex:=1;
    vtDword: cbVarType.itemindex:=2;
    vtQword: cbVarType.itemindex:=3;
    vtSingle: cbVarType.itemindex:=4;
    vtDouble: cbVarType.itemindex:=5;
    vtString,vtUnicodeString:
    begin
      cbVarType.itemindex:=6;
      cbunicode.checked:=vt=vtUnicodeString;
    end;

    vtByteArray: cbVarType.itemindex:=7;
  end;

  updatevalue;
end;

function TValuechangeForm.getVartype: TVariabletype;
begin
  result:=vtbyte;
  case cbVarType.itemindex of
    0: result:=vtByte;
    1: result:=vtWord;
    2: result:=vtDword;
    3: result:=vtQword;
    4: result:=vtSingle;
    5: result:=vtDouble;
    6: if cbunicode.checked then result:=vtUnicodeString else result:=vtString;
    7: result:=vtByteArray;
  end;
end;

procedure TValuechangeForm.setvartype(x: byte);
begin
  fvartype:=x;


  case x of
    0: cbVarType.itemindex:=0;  //byte
    1: cbVarType.itemindex:=1;  //word
    2: cbVarType.itemindex:=2;  //dword
    6: cbVarType.itemindex:=3;  //int64
    4: cbVarType.itemindex:=4;  //float
    5: cbVarType.itemindex:=5;  //double
    7: cbVarType.itemindex:=6;  //text
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

    read: ptruint;

    s: pchar;
    ws: pwchar;

begin
  value4:=0;
  value5:=0;

  case cbVarType.itemindex of
      0 : begin //byte
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value1),1,read);            
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value1),1,read);
            {$endif}
            if unicode then
              valuetext.text:=IntToHex(value1,2)
            else
              valuetext.text:=IntToStr(value1);

            cbunicode.visible:=true;
            cbunicode.caption:='Hexadecimal';
          end;

      1 : begin //word
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value2),2,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value2),2,read);
            {$endif}

            if unicode then
              valuetext.text:=IntToHex(value2,4)
            else
              valuetext.text:=IntToStr(value2);

            cbunicode.visible:=true;
            cbunicode.caption:='Hexadecimal';
          end;

      2 : begin  //dword
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value3),4,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value3),4,read);
            {$endif}
            if unicode then
              valuetext.text:=IntToHex(value3,8)
            else
              valuetext.text:=IntToStr(value3);

            cbunicode.visible:=true;
            cbunicode.caption:='Hexadecimal';
          end;

      3 : begin //int64
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value6),8,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value6),8,read);
            {$endif}
            if unicode then
              valuetext.text:=IntToHex(value6,16)
            else
              valuetext.text:=IntToStr(value6);

            cbunicode.visible:=true;
            cbunicode.caption:='Hexadecimal';
          end;

      4 : begin //float
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value4),4,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value4),4,read);
            {$endif}
            valuetext.text:=FloatToStr(value4);

            cbunicode.visible:=false;
          end;

      5 : begin //double
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value5),8,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value5),8,read);
            {$endif}
            valuetext.text:=floatToStr(value5);

            cbunicode.visible:=false;
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

              freememandnil(ws);


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

              freememandnil(s);

            end;

            cbunicode.visible:=true;
            cbunicode.caption:=rsUnicode;
          end;

      7 : begin
            {$ifdef net}
            readprocessmemorynet(0,pointer(address),addr(value1),1,read);
            {$else}
            readprocessmemory(processhandle,pointer(address),addr(value1),1,read);
            {$endif}
            ValueText.text:=inttohex(value1,2);

            cbunicode.visible:=false;
            cbunicode.caption:=rsUnicode;
          end;
  end;
end;

procedure TValueChangeForm.Button2Click(Sender: TObject);
begin
  modalresult:=mrcancel;
end;

procedure TValueChangeForm.cbunicodeChange(Sender: TObject);
var
  v: qword;
  digits: integer;
begin
  try
    if cbunicode.checked then
    begin
      v:=StrToInt64(valuetext.text);

      case VarType of
        vtByte: digits:=2;
        vtWord: digits:=4;
        vtDword: digits:=8;
        vtQword: digits:=16;
      end;

      valuetext.text:=IntToHex(v,digits);
    end
    else
    begin
      v:=strtoint('$'+valuetext.text);
      valuetext.text:=inttostr(v);
    end;
  except
  end;
end;

procedure TValueChangeForm.FormShow(Sender: TObject);
begin
  //cbVarType.ItemIndex:=0;

  valuetext.SetFocus;
  valuetext.SelectAll;
end;

procedure TValueChangeForm.Button1Click(Sender: TObject);
var write: ptruint;
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
    fs: TFormatSettings;
begin
  val(valuetext.text,newvalue1,i);
  val(valuetext.text,newvalue2,i);
  val(valuetext.text,newvalue3,i);

  val(valuetext.text,newvalue4,i);
  val(valuetext.text,newvalue5,i);
  val(valuetext.text,newvalue6,i);
  write:=i;

  newvaluest:=valuetext.text;

  fs:=DefaultFormatSettings;

  case cbVarType.itemindex of
{byte}  0       : writeprocessmemory(processhandle,pointer(address),addr(newvalue1),1,write);
{word}  1       : writeprocessmemory(processhandle,pointer(address),addr(newvalue2),2,write);
{dword} 2       : writeprocessmemory(processhandle,pointer(address),addr(newvalue3),4,write);
{int64} 3       : writeprocessmemory(processhandle,pointer(address),addr(newvalue6),8,write);
{float} 4       : begin
                    try
                      newvalue4:=StrToFloat(valuetext.text, fs);
                    except
                      if fs.DecimalSeparator='.' then
                        fs.DecimalSeparator:=','
                      else
                        fs.DecimalSeparator:='.';

                      try
                        newvalue4:=StrToFloat(valuetext.text, fs);

                      except
                        exit; //quit
                      end;
                    end;

                    writeprocessmemory(processhandle,pointer(address),addr(newvalue4),4,write);
                  end;

{double}5       : begin
                    try
                      newvalue5:=StrToFloat(valuetext.text, fs);
                    except
                      if fs.DecimalSeparator='.' then
                        fs.DecimalSeparator:=','
                      else
                        fs.DecimalSeparator:='.';

                      try
                        newvalue5:=StrToFloat(valuetext.text, fs);

                      except
                        exit; //quit
                      end;
                    end;

                    writeprocessmemory(processhandle,pointer(address),addr(newvalue5),8,write);
                  end;

{byte}  6       : begin
                    setlength(newstring,length(ValueText.text));
                    for i:=1 to length(ValueText.text) do
                      newstring[i-1]:=ord(ValueText.text[i]);

                    writeprocessmemory(processhandle,pointer(address),newstring,length(ValueText.text),write);

                  end;

{bytes} 7       : begin
//convert the string to bytes
                    ConvertStringToBytes(Valuetext.Text,true,newvalue7, true);
                    setlength(newstring,length(newvalue7));
                    for i:=0 to length(newvalue7)-1 do
                    begin
                      //if the bytesvalue is -1 then read that byte from the memory and leave it untouched
                      //if it is unreadable popup a message noptifying the user
                      if newvalue7[i]<0 then
                      begin
                        write:=0;
                        readprocessmemory(processhandle,pointer(address+i),addr(newstring[i]),1,write);
                        if write<>1 then raise exception.Create(rsPartOfTheStringIsUnreadable);

                        if newvalue7[i]<>-1 then
                        begin
                          //apply the nibble part
                          //example: 9*  and newstring[i]=7c
                          //the wanted result will be 9c

                          //newvalue7[i]=$8000f090
                          //not (f0) = 0f
                          //0f and 9c = 0c
                          //0c or 90 = 9c

                          newstring[i]:=(((not (newvalue7[i] shr 8)) and $ff) and newstring[i]) or (newvalue7[i] and $ff);
                        end;
                      end
                      else
                        newstring[i]:=newvalue7[i];

                      //newstring now contains all the bytes we need so write them to the memory

                    end;
                    writeprocessmemory(processhandle,pointer(address),newstring,length(newstring),write);
                  end;
  end;

  modalresult:=mrok;
end;

procedure TValueChangeForm.cbVarTypeChange(Sender: TObject);
begin
  updatevalue;
end;

procedure TValueChangeForm.FormCreate(Sender: TObject);
begin
  //cbVarType.Items.Delete(cbVarType.Items.Count-1);
  cbVarType.itemindex:=0;
end;

procedure TValueChangeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caFree;
end;

initialization
  {$i Valuechange.lrs}

end.
