unit groupscancommandparser;

{
This unit contains the class that reads a groupscan command and parses it. The results can be looked at afterwards
}

{$mode delphi}

interface

uses
  Classes, SysUtils, cefuncproc, CustomTypeHandler;

type
  TGroupscanCommandParser=class
  private
    calculatedBlocksize: integer;
    procedure parseToken(s: string);
  public
    FloatSettings: TFormatSettings;

    elements: array of record
      wildcard: boolean;
      offset: integer;
      vartype: TVariabletype;
      uservalue: string;
      valueint: qword;
      valuefloat: double;
      customtype: TCustomType;
      bytesize: integer;
    end;

    blocksize: integer;
    blockalignment: integer;
    outOfOrder: boolean;
    typeAligned: boolean;


    procedure parse(command: string);
    constructor create(command: string='');
  end;

implementation

procedure TGroupscanCommandParser.parseToken(s: string); //todo: add support for strings
var i: integer;
  j: integer;
  command,value: string;
  ctn: string;
begin
  i:=pos(':', s);
  if i=-1 then exit;

  command:=copy(s,1,i-1);
  value:=copy(s,i+1, length(s));



  if command='BA' then
    blockalignment:=strtoint(value)
  else
  if command='BS' then
    blocksize:=strtoint(value)
  else
  if command='OOO' then
  begin
    outOfOrder:=true;
    typeAligned:=value='A';
  end;

  if (length(command)>=1) and (command[1] in ['1','2','4','8','F','D','C','S']) then
  begin
    j:=length(elements);
    setlength(elements, j+1);

    elements[j].offset:=calculatedBlocksize;


    case command[1] of
      '1':
      begin
        elements[j].vartype:=vtByte;
        elements[j].bytesize:=1;

      end;

      '2':
      begin
        elements[j].vartype:=vtWord;
        elements[j].bytesize:=2;
      end;

      '4':
      begin
        elements[j].vartype:=vtDWord;
        elements[j].bytesize:=4;
      end;

      '8':
      begin
        elements[j].vartype:=vtQword;
        elements[j].bytesize:=8;
      end;

      'F':
      begin
        elements[j].vartype:=vtSingle;
        elements[j].bytesize:=4;
      end;

      'D':
      begin
        elements[j].vartype:=vtDouble;
        elements[j].bytesize:=8;
      end;

      'C':
      begin
        //custom type
        elements[j].vartype:=vtCustom;
        i:=pos('(', command);

        for j:=length(command) downto i do
        begin
          if command[j]=')' then break;
        end;

        ctn:=copy(command, i+1, j-i-1);
        elements[j].customtype:=GetCustomTypeFromName(ctn);
        if elements[j].customtype<>nil then
          elements[j].bytesize:=elements[j].customtype.bytesize
        else
          raise exception.create('Custom type not recognized: '+ctn);
      end;

      'S':
      begin
        if (length(command)>=2) and (command[2]='U') then
        begin
          elements[j].vartype:=vtUnicodeString;
          elements[j].bytesize:=length(value)*2;
        end
        else
        begin
          elements[j].vartype:=vtString;
          elements[j].bytesize:=length(value);
        end;
      end;
    end;
    elements[j].uservalue:=value;

    inc(calculatedBlocksize, elements[j].bytesize);

    elements[j].wildcard:=(value='') or ((not (elements[j].vartype in [vtString, vtUnicodeString])) and (value = '*'));

    if not elements[j].wildcard then
    begin
      case elements[j].vartype of
        vtByte..vtQword: elements[j].valueint:=StrToQWordEx(value);


        vtSingle, vtDouble:
        begin
          try
            elements[j].valuefloat:=StrToFloat(value, FloatSettings);
          except
            if FloatSettings.DecimalSeparator='.' then
              FloatSettings.DecimalSeparator:=',' else
              FloatSettings.DecimalSeparator:='.';
          end;

        end;
      end;
    end;

  end;
end;

procedure TGroupscanCommandParser.parse(command: string);
var start, i: integer;
  s: string;
  inquote: boolean;
  inbraces: boolean;
begin
  //reset/init
  blockalignment:=4; //default
  blocksize:=-1;

  calculatedBlocksize:=0;
  setlength(elements,0);

  FloatSettings:=DefaultFormatSettings;

  command:=uppercase(command);

  start:=1;
  inquote:=false;
  inbraces:=false;

  for i:=1 to length(command) do
    if (command[i] in [' ','''','(',')']) or (i=length(command)) then
    begin
      if command[i]='''' then
        inquote:=not inquote;

      if inquote then continue;

      //not inside a quote
      if command[i]='(' then //check if custom type name
        inbraces:=true
      else
      if command[i]=')' then
        inbraces:=false;

      if inbraces then continue;
      //not inside braces or a quote, handle the token

      s:=trim(copy(command, start, i+1-start));
      start:=i;

      parseToken(s);
    end;

  if blocksize=-1 then  //check if set by the user, if not (or the user is a complete retard that sets size to -1)
    blocksize:=calculatedBlocksize;

  if outOfOrder then
    for i:=0 to length(elements)-1 do
      if elements[i].wildcard then
        raise exception.create('Wildcards/Empty are not allowed for Out of Order scans');

end;

constructor TGroupscanCommandParser.create(command: string='');
begin
  if command<>'' then
    parse(command);
end;


end.

