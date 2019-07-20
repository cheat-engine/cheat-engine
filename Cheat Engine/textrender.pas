unit textrender;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphics, math, LazUTF8;

function renderFormattedText(canvas: TCanvas; rect: Trect; x,y: integer; const formattedtext: string): trect;

implementation

uses windows;

const altfonts: array [0..8] of string=('Consolas','Courier','Courier New','Fixedsys','Terminal','Arial','MS Sans Serif','Comic Sans MS','Wingdings');

var _8bitcolors:array[0..255] of dword;

type TTextState=record
  bgcolor: tcolor;
  font: tfont;
end;

type
  TParsedValueInformation=record
    defined: boolean;
    value:integer;
  end;

  PParsedValueInformation=^TParsedValueInformation;




procedure restoreOriginalState(canvas: TCanvas; originalState: TTextState);
begin
  canvas.font.Assign(originalState.font);
  canvas.brush.color:=originalState.bgcolor;
end;

function readIntegerString(const formattedtext: string; var index: integer): string;
begin
  result:='';
  while index<length(formattedtext) do
  begin
    if formattedtext[index] in ['0'..'9'] then
    begin
      result:=result+formattedtext[index];
      inc(index);
    end
    else
      exit;
  end;
end;

function getAnsiColor(c: integer): tcolor;
begin
  result:=0;

  case c of
    0: result:=RGBToColor(0,0,0);
    1: result:=RGBToColor(128,0,0);
    2: result:=RGBToColor(0,128,0);
    3: result:=RGBToColor(128,128,0);
    4: result:=RGBToColor(0,0,128);
    5: result:=RGBToColor(128,0,128);
    6: result:=RGBToColor(0,128,128);
    7: result:=RGBToColor(192,192,192);
    //bright
    8: result:=RGBToColor(128,128,128);
    9: result:=RGBToColor(255,0,0);
    10: result:=RGBToColor(0,255,0);
    11: result:=RGBToColor(255,255,0);
    12: result:=RGBToColor(0,0,255);
    13: result:=RGBToColor(255,0,255);
    14: result:=RGBToColor(0,255,255);
    15: result:=RGBToColor(255,255,255);
  end;
end;

var colorlistInitialize: boolean=false;
procedure initColorList;
var
  i: integer;
  r,g,b: byte;
begin
  if colorlistInitialize then exit;
  for i:=0 to 15 do
    _8bitcolors[i]:=getAnsiColor(i);

  r:=0;
  g:=0;
  b:=0;

  for i:=16 to 231 do
  begin
    _8bitcolors[i]:=RGBToColor(r,g,b);

    case b of
      0: b:=$5f;
      $5f..$d7: b:=b+40;
      $ff:
      begin
        b:=0;

        case g of
          0: g:=$5f;
          $5f..$d7: g:=g+40;
          $ff:
          begin
            case r of
              0: r:=$5f;
              $5f..$d7: r:=r+40;
              $ff: r:=0; //and break
            end;
          end;
        end;
      end;
    end

    {
    if i<=52 then r:=0 else r:=$5f+((i-52) div 36)*$28;

    b:=(i-16) mod 6;
    if b>0 then b:=$5f+(b-1)*$28;

    g:=(i-16) mod 36;
    g:=g div 6;

    if g>0 then g:=$5f+(g-1)*$28;

    }

  end;

  for i:=232 to 255 do
  begin
    r:=8+10*(i-232);
    _8bitcolors[i]:=RGBToColor(r,r,r);
  end;

end;


procedure handleSGR(canvas: TCanvas; const originalState: TTextState; ParsedValueList: PParsedValueInformation; ParsedValueCount: integer);
var
  operation: integer;
  t: Tcolor;

  r,g,b: byte;
  i: integer;
begin
  i:=0;
  while i<parsedvaluecount do
  begin
    if parsedvaluelist[i].defined then
      operation:=ParsedValueList[i].value
    else
      operation:=0;

    case operation of
      0: //reset
      begin
        canvas.Brush.color:=originalstate.bgcolor;
        canvas.font.Assign(originalstate.font);
      end;

      1: canvas.font.Style:=canvas.font.style+[fsBold];
      2: canvas.font.Style:=canvas.font.style-[fsBold];
      3: canvas.font.style:=canvas.font.style+[fsItalic];
      4: canvas.font.style:=canvas.font.style+[fsUnderline];
      5,6: ; //no blinking
      7,27: //reverse
      begin
        t:=canvas.brush.color;
        canvas.brush.color:=canvas.font.color;
        canvas.font.color:=t;
      end;

      8: canvas.font.color:=canvas.brush.color; //conceil
      9: canvas.font.style:=canvas.font.style+[fsStrikeOut];
      10: canvas.font.Name:=originalstate.Font.Name;
      11..19: canvas.font.name:=altfonts[operation-11];
      20: ; //fraktur
      21,22: canvas.font.style:=canvas.font.style-[fsBold];
      23: canvas.font.style:=canvas.font.style-[fsItalic];
      24: canvas.font.style:=canvas.font.style-[fsUnderline];
      25: ; //still no blinking
      28: canvas.font.color:=originalstate.font.color;
      29: canvas.font.style:=canvas.font.style-[fsStrikeOut];
      30..37:
      begin
        //Set foreground color
        canvas.font.color:=getAnsiColor(operation-30);
      end;

      38:
      begin
        //set Foreground Color Ex
        //canvas.font.color:=get
        inc(i);

        if (parsedvaluecount>i) and ParsedValueList[i].defined then
        begin
          case ParsedValueList[i].value of
            5:  //8 bit
            begin
              inc(i);
              if (parsedvaluecount>i) and parsedValueList[i].defined then
              begin
                if inrange(parsedValueList[i].value,0,255) then
                begin
                  initColorList;
                  canvas.font.color:=_8bitcolors[parsedValueList[i].value];
                end;
              end;
            end;

            2: //24 bit
            begin
              inc(i);
              if (ParsedValueCount>i) and parsedValueList[i].defined then r:=ParsedValueList[i].Value else r:=0;

              inc(i);
              if (ParsedValueCount>i) and parsedValueList[i].defined then g:=ParsedValueList[i].Value else g:=0;

              inc(i);
              if (ParsedValueCount>i) and parsedValueList[i].defined then b:=ParsedValueList[i].Value else b:=0;

              canvas.font.color:=RGBToColor(r,g,b);
            end;
          end;
        end;

      end;

      39: canvas.font.color:=originalstate.Font.color;

      40..47:
      begin
        //set background color
        canvas.brush.color:=getAnsiColor(operation-40);
      end;

      48:
      begin
        //set background color ex
        inc(i);
        if (parsedvaluecount>=2) and ParsedValueList[1].defined then
        begin
          case ParsedValueList[1].value of
            5:  //8 bit
            begin
              inc(i);
              if (parsedvaluecount>i) and parsedValueList[i].defined then
              begin
                if inrange(parsedValueList[i].value,0,255) then
                begin
                  initColorList;
                  canvas.brush.color:=_8bitcolors[parsedValueList[i].value];
                end;
              end;
            end;

            2: //24 bit
            begin
              inc(i);
              if (ParsedValueCount>i) and parsedValueList[i].defined then r:=ParsedValueList[i].Value else r:=0;

              inc(i);
              if (ParsedValueCount>i) and parsedValueList[i].defined then g:=ParsedValueList[i].Value else g:=0;

              inc(i);
              if (ParsedValueCount>i) and parsedValueList[i].defined then b:=ParsedValueList[i].Value else b:=0;

              canvas.brush.color:=RGBToColor(r,g,b);
            end;
          end;
        end;

      end;

      49: canvas.brush.color:=originalstate.bgcolor;

      90..97:
      begin
        canvas.font.color:=getAnsiColor(operation-90+8);
      end;

      100..107:
      begin
        canvas.brush.color:=getAnsiColor(operation-100+8);
      end;

    end;

    inc(i);
  end;
end;

procedure handleCSISequence(canvas: TCanvas; rect: Trect; const formattedtext: string; const originalState: TTextState; var index: integer; var _x: integer; var _y: integer);
  function LineHeight: integer;
  begin
    result:=canvas.TextHeight('AFgGjJ');
  end;

  function SpaceWidth: integer;
  begin
    result:=canvas.TextHeight(' ');
  end;

var
  lastvalue: integer=0;
  valuelist: array of TParsedValueInformation;

  s: string;
begin
  setlength(valuelist,0);
  while index<length(formattedtext) do
  begin
    case formattedtext[index] of
      '0'..'9': //ESC[n
      begin
        setlength(valuelist, length(valuelist)+1);

        s:=readIntegerString(formattedText, index);
        valuelist[length(valuelist)-1].defined:=true;
        valuelist[length(valuelist)-1].value:=strtoint(s);

        lastvalue:=index;
      end;

      'A':
      begin
        inc(index);
        if _y<rect.Top then exit;
        _y:=_y-LineHeight*valuelist[0].value;
        if _y<rect.top then _y:=rect.top;

        exit;
      end;

      'B':
      begin
        inc(index);
        if _y>=rect.Bottom-LineHeight then exit;
        _y:=_y+LineHeight*valuelist[0].value;
        if _y>=rect.Bottom-LineHeight then _y:=rect.Bottom-LineHeight;

        exit;
      end;

      'C':
      begin
        inc(index);
        if _x>=rect.right-spacewidth then exit;
        _x:=_x+SpaceWidth*valuelist[0].value;
        if _x>=rect.right-SpaceWidth then _x:=rect.right-SpaceWidth;

        exit;
      end;

      'D':
      begin
        inc(index);
        if _x<rect.left then exit;
        _x:=_x-SpaceWidth*valuelist[0].Value;
        if _x<rect.left then _x:=rect.left;

        exit;
      end;

      'H','f':
      begin
        inc(index);
        _x:=rect.Left;
        _y:=rect.Top;

        if length(valuelist)=0 then
          exit;

        if (length(valuelist)>=1) and valuelist[0].defined then
          _x:=rect.left+SpaceWidth*valuelist[0].value;

        if (length(valuelist)>=2) and valuelist[1].defined then
          _y:=rect.top+LineHeight*valuelist[1].value;
      end;

      'm': //the actual use for this
      begin
        inc(index);
        handleSGR(canvas,originalState, @valuelist[0],length(valuelist));
        exit;
      end;

      ';':
      begin
        inc(index);

        if index<>lastvalue+1 then //missing a value
        begin
          setlength(valuelist,length(valuelist)+1);
          valuelist[length(valuelist)-1].defined:=false;
        end;

      end;
      else
      begin
        inc(index);
        exit; //undefined
      end;
    end;
  end;

end;

procedure handleEscapeSequence(canvas: TCanvas; rect: Trect; const formattedtext: string; const originalState: TTextState; var index: integer; var _x: integer; var _y: integer);
begin
  if index>=length(formattedtext) then exit;

  case formattedtext[index] of
    '[' :
    begin
      inc(index);
      handleCSISequence(canvas, rect, formattedtext, originalstate, index, _x, _y);
    end;

    'c':
    begin
      inc(index);
      restoreOriginalState(canvas, originalState);
    end;
  end;
end;

function isChar(const s: string; index: integer; c: char): boolean;
begin
  result:=false;
  if index>=length(s) then exit;

  result:=s[index]=c;
end;

function handleStyleSequence(canvas: TCanvas; const formattedtext: string; var index: integer): boolean;
var i: integer;
begin
  result:=false;
  if index>=length(formattedtext) then exit;

  i:=index;
  inc(i);

  case formattedtext[i] of
    'b','B':
    begin
      if isChar(formattedtext, i+1,']') then
      begin
        canvas.Font.Style:=canvas.Font.Style+[fsBold];
        index:=i+2;
        result:=true;
      end;
    end;

    'u','U':
    begin
      if isChar(formattedtext, i+1,']') then
      begin
        canvas.Font.Style:=canvas.Font.Style+[fsUnderline];
        index:=i+2;
        result:=true;
      end;
    end;

    'i','I':
    begin
      if isChar(formattedtext, i+1,']') then
      begin
        canvas.Font.Style:=canvas.Font.Style+[fsItalic];
        index:=i+2;
        result:=true;
      end;
    end;

    's','S':
    begin
      if isChar(formattedtext, i+1,']') then
      begin
        canvas.Font.Style:=canvas.Font.Style+[fsStrikeOut];
        index:=i+2;
        result:=true;
      end;
    end;

    '/':
    begin
      if (length(formattedtext)>=i+2) and (formattedtext[i+2]=']') then
      begin
        inc(i);
        case formattedtext[i] of
          'b','B':
          begin
            canvas.Font.Style:=canvas.Font.Style-[fsBold];
            index:=i+2;
            result:=true;
          end;

          'u','U':
          begin
            canvas.Font.Style:=canvas.Font.Style-[fsUnderline];
            index:=i+2;
            result:=true;
          end;

          'i','I':
          begin
            canvas.Font.Style:=canvas.Font.Style-[fsItalic];
            index:=i+2;
            result:=true;
          end;

          's','S':
          begin
            canvas.Font.Style:=canvas.Font.Style-[fsStrikeOut];
            index:=i+2;
            result:=true;
          end;

        end;
      end;
    end;

  end;
end;

function renderFormattedText(canvas: TCanvas; rect: Trect; x,y: integer; const formattedtext: string): trect;
var
  i: integer;
  _x: integer;
  _y: integer;
  original: TTextState;

  lineheight: integer;
  w: integer;

  temprect: trect;

  maxx, maxy: integer;

  charlength: integer;
  c: string;

  procedure renderChar;
  begin
    //renderable character
    charlength:=UTF8CharacterLength(pchar(@formattedtext[i]));
    if charlength>1 then
    begin
      setlength(c,charlength);
      copymemory(@c[1], @formattedtext[i],charlength);
      w:=canvas.TextWidth(c);
    end
    else
    begin
      canvas.TextRect(rect,_x,_y,formattedtext[i]);
      w:=canvas.TextWidth(formattedtext[i]);
    end;

    if canvas.brush.Color<>original.bgcolor then
    begin
      temprect:=classes.rect(_x,_y,_x+w,_y+lineheight);
      if temprect.left>rect.right then
      begin
        inc(_x,w);

        maxx:=max(maxx, _x);
        inc(i);
        exit;
      end;
      if temprect.Right>rect.Right then temprect.right:=rect.right;
      if temprect.Bottom>rect.bottom then temprect.bottom:=rect.bottom;
      canvas.FillRect(temprect);
    end;

    if charlength>1 then
      canvas.TextRect(rect,_x,_y,c)
    else
      canvas.TextRect(rect,_x,_y,formattedtext[i]);



    inc(_x,w);
    maxx:=max(maxx, _x);
    inc(i,charlength);
  end;

begin
  i:=1;
  original.font:=tfont.create;
  original.font.Assign(canvas.font);
  original.bgcolor:=canvas.brush.color;

  _x:=x;
  _y:=y;

  maxx:=_x;
  maxy:=_y;

  lineheight:=canvas.GetTextHeight('AFgGjJ');

  while i<=length(formattedtext) do
  begin
    case formattedtext[i] of
      '[':
      begin
        //could be a secondary style thingy, check
        if handleStyleSequence(canvas,formattedtext, i)=false then
          renderchar;
      end;

      #27:   //escape character
      begin
        inc(i);
        handleEscapeSequence(canvas, rect, formattedtext, original, i,_x,_y);

        maxx:=max(maxx, _x);
        maxy:=max(maxy, _y);
      end;

      #13: //return
      begin
        _x:=x;
        inc(_y, lineheight);
        maxy:=max(maxy, _y);
        inc(i);
      end;

      #10: inc(i); //ignore (linefeed)
      else
        renderChar;

    end;
  end;

  restoreOriginalState(canvas, original);
  original.font.free;

  result.left:=x;
  result.top:=y;
  result.right:=maxx;
  result.bottom:=maxy+lineheight;
end;



end.

