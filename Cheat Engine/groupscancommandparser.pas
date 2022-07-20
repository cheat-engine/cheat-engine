unit groupscancommandparser;

{
This unit contains the class that reads a groupscan command and parses it. The results can be looked at afterwards
}

{$mode delphi}

interface




{$ifdef jni}
uses Classes, SysUtils, strutils, CustomTypeHandler, commonTypeDefs;
{$else}
uses Classes, SysUtils, cefuncproc, CustomTypeHandler, strutils, commonTypeDefs;
{$endif}

resourcestring
  rsGSCPCustomTypeNotRecognized = 'Custom type not recognized: ';
  rsGSCPInvalidGroupscanCommand = 'Invalid groupscan command';
  rsGSCPWildcardsEmptyAreNotAllowedForOutOfOrderScans = 'Wildcards/Empty are not allowed for Out of Order scans';
  rsARangeTypeNeedsARangeForValue = 'A range type needs a range for value';
  rsARangeCantBeAWildcard = 'A range can''t be a wildcard';

type
  TGroupscanCommandParser=class
  private
    calculatedBlocksize: integer;
    function parseFloat(const s: string): double;
    function splitRange(const s: string): TStringArray;
    procedure parseToken(s: string);
  public
    FloatSettings: TFormatSettings;

    elements: array of record
      wildcard: boolean; //for vtPointer means it has to be a valid pointer, so not nil
      offset: integer;
      vartype: TVariabletype;
      uservalue, uservalue2: string;
      valueint, valueint2: qword;
      valuefloat, valuefloat2: double;
      customtype: TCustomType;
      bytesize: integer;
      command: string;
      picked: boolean;
      range: boolean;
      signed: boolean;
      pointertypes: TPointerTypes; // for vtPointer this restricts whether the pointer is static, dynamic, executable
    end;

    blocksize: integer;
    blockalignment: integer;
    outOfOrder: boolean;
    typeAligned: boolean;


    procedure parse(command: string);
    constructor create(command: string='');
  end;

implementation

uses Parsers, ProcessHandlerUnit;

function TGroupscanCommandParser.parseFloat(const s: string): double;
begin
  try
    result:=StrToFloat(s, FloatSettings);
  except
    if FloatSettings.DecimalSeparator='.' then
      FloatSettings.DecimalSeparator:=',' else
      FloatSettings.DecimalSeparator:='.';

    result:=StrToFloat(s, FloatSettings);
  end;
end;

function TGroupscanCommandParser.SplitRange(const s: string): TStringArray;
var
  i: integer;
  sa: TStringArray;
  temps: string;
begin
  sa:=s.Split('-');
  temps:='';
  result:=[];
  for i:=0 to length(sa)-1 do
  begin
    sa[i]:=trim(sa[i]);
    if sa[i]='' then
      temps:=temps+'-'
    else
    begin
      temps:=temps+sa[i];
      setlength(result, length(result)+1);
      result[length(result)-1]:=temps;
      temps:='';
    end;
  end;
end;

procedure TGroupscanCommandParser.parseToken(s: string);
var i,j,k: integer;
  command,value: string;
  values: array of string;
  ctn: string;
  bracketcount: integer;
  nextchar: integer;
  tempq: dword;
  tempd: double;
begin

  //deal with custom types with a ':' and don't mess up strings
  bracketcount:=0;
  j:=0;
  for i:=1 to length(s) do
  begin
    case s[i] of
      ':': if bracketcount=0 then
      begin
        j:=i;
        break; //found it
      end;
      '(': inc(bracketcount);
      ')': dec(bracketcount);
    end;
  end;

  if j=0 then exit;

  command:=uppercase(copy(s,1,j-1));
  value:=copy(s,j+1, length(s));



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

  if (length(command)>=1) and (command[1] in ['1','2','4','8','F','D','C','S','W','P']) then
  begin
    j:=length(elements);
    setlength(elements, j+1);

    //init the element to 0
    elements[j].wildcard:=false;
    elements[j].vartype:=vtByte;
    elements[j].uservalue:='';
    elements[j].uservalue2:='';
    elements[j].valueint:=0;
    elements[j].valueint2:=0;
    elements[j].valuefloat:=0;
    elements[j].valuefloat2:=0;
    elements[j].customtype:=nil;
    elements[j].bytesize:=1;
    elements[j].pointertypes:=[ptStatic, ptDynamic];
    elements[j].offset:=calculatedBlocksize;
    elements[j].command:=command;

    elements[j].picked:=false;
    elements[j].range:=false;
    elements[j].signed:=false;

    nextchar:=2;

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

      'P':   //pointer
      begin
        elements[j].vartype:=vtPointer;
        elements[j].bytesize:=processhandler.pointersize;
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

        for k:=length(command) downto i do
        begin
          if command[k]=')' then break;
        end;

        ctn:=copy(command, i+1, k-i-1);
        elements[j].customtype:=GetCustomTypeFromName(ctn);
        if elements[j].customtype<>nil then
          elements[j].bytesize:=elements[j].customtype.bytesize
        else
          raise exception.create(rsGSCPCustomTypeNotRecognized+ctn);

        nextchar:=k+1;
      end;

      'S':
      begin
        //remove quote part from value
        if (value<>'') and (value[1]='''') and (value[length(value)]='''') then
          value:=copy(value, 2, length(value)-2);

        if (length(command)>=2) and (command[2]='U') then
        begin
          elements[j].vartype:=vtUnicodeString;
          elements[j].bytesize:=length(value)*2;
          nextchar:=3;
        end
        else
        begin
          elements[j].vartype:=vtString;
          elements[j].bytesize:=length(value);
        end;


      end;

      'W': //wildcard of random size (implemented as a string of specific size with wildcard value)
      begin
        elements[j].vartype:=vtString;
        elements[j].bytesize:=strtoint(value);
        value:=''; //set as a wildcard
      end;

      else
        raise exception.create(rsGSCPInvalidGroupscanCommand);
    end;

    elements[j].uservalue:=value;

    if length(command)>=nextchar then
    begin
      case command[nextchar] of
        'P': elements[j].picked:=true; //elements marked picked will be added when doubleclicked in the addresslist
        'R':
        begin
          if elements[j].wildcard then raise exception.create(rsARangeCantBeAWildcard+'');
          elements[j].range:=true;
          values:=splitRange(value);
          if length(values)<>2 then
            raise exception.create(rsARangeTypeNeedsARangeForValue);

          if (values[0][1]='-') or (values[1][1]='-') then elements[j].signed:=true;

          elements[j].uservalue:=values[0];
          elements[j].uservalue2:=values[1];
        end
        else
          raise exception.create(rsGSCPInvalidGroupscanCommand);
      end;
    end;




    inc(calculatedBlocksize, elements[j].bytesize);

    elements[j].wildcard:=(value='') or ((not (elements[j].vartype in [vtString, vtUnicodeString])) and (value = '*'));

    if not elements[j].wildcard then
    begin
      case elements[j].vartype of
        vtByte..vtQword, vtCustom:
        begin
          if elements[j].range then
          begin
            elements[j].valueint:=StrToQwordEx(values[0]);
            elements[j].valueint2:=StrToQwordEx(values[1]);

            if (elements[j].signed and (int64(elements[j].valueint2)<int64(elements[j].valueint))) or
               (not elements[j].signed and (elements[j].valueint2<elements[j].valueint)) //swap
            then
            begin
              tempq:=elements[j].valueint;
              elements[j].valueint:=elements[j].valueint2;
              elements[j].valueint2:=tempq;
            end;
          end
          else
            elements[j].valueint:=StrToQWordEx(value);
        end;

        vtPointer:
        begin
          if value = 'S' then
          begin
            elements[j].pointertypes:=[ptStatic];
            elements[j].wildcard:=true;
          end
          else if value = 'D' then
          begin
            elements[j].pointertypes:=[ptDynamic];
            elements[j].wildcard:=true;
          end
          else if value = 'E' then
          begin
            elements[j].pointertypes:=[ptExecutable];
            elements[j].wildcard:=true;
          end
          else
          begin
            //convert it to a normal type
            if processhandler.is64Bit then
              elements[j].vartype:=vtQword
            else
              elements[j].vartype:=vtDword;

            elements[j].valueint:=StrToQWordEx(value);
          end;
        end;

        vtSingle, vtDouble:
        begin
          if elements[j].range then
          begin
            elements[j].valuefloat:=parsefloat(values[0]);
            elements[j].valuefloat2:=parsefloat(values[1]);

            if (elements[j].valuefloat2<elements[j].valuefloat) //swap
            then
            begin
              tempd:=elements[j].valuefloat;
              elements[j].valuefloat:=elements[j].valuefloat2;
              elements[j].valuefloat2:=tempd;
            end;

          end
          else
            elements[j].valuefloat:=parseFloat(value);
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
  haspick: boolean;
begin
  //reset/init
  blockalignment:=4; //default
  blocksize:=-1;

  calculatedBlocksize:=0;
  setlength(elements,0);

  FloatSettings:=DefaultFormatSettings;

  start:=1;
  inquote:=false;
  inbraces:=false;

  for i:=1 to length(command) do
    if (command[i] in [' ','''','(',')']) or (i=length(command)) then
    begin
      if command[i]='''' then
      begin
        inquote:=not inquote;

        if i<length(command) then //there's more, first check that (it's wrong, whatever comes after here if it's not a space though)
          continue;
      end;

      if inquote then continue;

      //not inside a quote
      if command[i]='(' then //check if custom type name
      begin
        inbraces:=true;
        continue;
      end
      else
      if command[i]=')' then
      begin
        inbraces:=false;
        continue;
      end;

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
        raise exception.create(rsGSCPWildcardsEmptyAreNotAllowedForOutOfOrderScans);


  haspick:=false;
  for i:=0 to length(elements)-1 do
    if elements[i].picked then
    begin
      haspick:=true;
      break;
    end;

  if haspick=false then
  begin
    //mark ALL elements as picked
    for i:=0 to length(elements)-1 do
      elements[i].picked:=true;
  end;
end;

constructor TGroupscanCommandParser.create(command: string='');
begin
  parse(command);
end;


end.

