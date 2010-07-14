unit disassemblerviewlinesunit;

{$MODE Delphi}

interface

uses LCLIntf,sysutils, classes,ComCtrls, graphics, CEFuncProc, disassembler,
     CEDebugger, debughelper, KernelDebugger, symbolhandler, plugin;

type TDisassemblerLine=class
  private
    fbitmap: tbitmap;
    fCanvas: TCanvas;
    fHeaders: THeaderSections;
    top: integer;
    height: integer; //height of the line
    fInstructionCenter: integer; //y position of the center of the disassembled line (so no header)
    isselected: boolean;

    faddress: ptrUint;
    fdescription: string;
    fdisassembled: string;

    fisJump: boolean;
    fJumpsTo: ptrUint;


    addressstring: string;
    bytestring: string;
    opcodestring: string;
    specialstring: string;
    parameterstring: string;
    referencedbylineheight: integer;
    boldheight: integer;
    textheight: integer;
    function truncatestring(s: string; maxwidth: integer): string;
    function buildReferencedByString: string;
    procedure DrawTextRectWithColor(const ARect: TRect; X, Y: integer; const Text: string);
  public
    property address: ptrUint read faddress;
    property instructionCenter: integer read fInstructionCenter;
    function isJumpOrCall(var addressitjumpsto: ptrUint): boolean;
    function getHeight: integer;
    function getTop: integer;
    property description:string read fdescription;
    property disassembled:string read fdisassembled;
    procedure renderLine(var address: ptrUint; linestart: integer; selected: boolean=false; focused: boolean=false);
    procedure drawJumplineTo(yposition: integer; offset: integer; showendtriangle: boolean=true);
    procedure handledisassemblerplugins(addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor);
    constructor create(bitmap: TBitmap; headersections: THeaderSections);
end;

implementation

uses MemoryBrowserFormUnit, dissectCodeThread,debuggertypedefinitions, dissectcodeunit;


procedure TDisassemblerLine.drawJumplineTo(yposition: integer; offset: integer; showendtriangle: boolean=true);
var
  oldpenstyle: Tpenstyle;
  oldpencolor, oldbrushcolor: TColor;
begin
  oldpenstyle:=fCanvas.Pen.Style;
  oldpencolor:=fCanvas.Pen.color;
  oldbrushcolor:=fCanvas.Brush.color;

  fCanvas.Pen.Color:=clBlack;
  fCanvas.Pen.Style:=psDot;


  fCanvas.PenPos:=point(fHeaders.items[2].Left,instructioncenter);
  fCanvas.LineTo(fHeaders.items[2].Left-offset,instructioncenter);
  fCanvas.LineTo(fHeaders.items[2].Left-offset,yposition);
  fCanvas.LineTo(fHeaders.items[2].Left,yposition);

  fCanvas.Pen.Style:=oldpenstyle;
  if showendtriangle then
  begin
    fCanvas.Brush.Style:=bsSolid; //should be the default, but in case something fucked with it (not in the planning, never intended, so even if someone did do it, I'll undo it)
    fCanvas.Brush.Color:=clblack;
    fCanvas.Polygon([point(fheaders.items[2].Left-4,yposition-4),point(fheaders.items[2].Left,yposition),point(fheaders.items[2].Left-4,yposition+4)]);
  end;
  fCanvas.Brush.Color:=oldbrushcolor;
  fCanvas.Pen.Color:=oldpencolor;

end;

function TDisassemblerLine.isJumpOrCall(var addressitjumpsto: ptrUint): boolean;
begin
  result:=fisJump;
  if result then
    addressitjumpsto:=fJumpsTo;
end;

function TDisassemblerLine.truncatestring(s: string; maxwidth: integer): string;
var dotsize: integer;
begin
  if fCanvas.TextWidth(s)>maxwidth then
  begin
    dotsize:=fCanvas.TextWidth('...');
    maxwidth:=maxwidth-dotsize;
    if maxwidth<=0 then
    begin
      result:=''; //it's too small for '...'
      exit;
    end;

    while fCanvas.TextWidth(s)>maxwidth do
      s:=copy(s,1,length(s)-1);

    result:=s+'...';
  end else result:=s; //it fits
end;


function TdisassemblerLine.buildReferencedByString: string;
var addresses: tdissectarray;
    i: integer;
begin

  result:='';
  setlength(addresses,0);

  if (frmDissectCode<>nil) and (frmDissectCode.dissectcode<>nil) and (frmDissectCode.dissectcode.done) then
  begin
    if frmDissectCode.dissectcode.CheckAddress(address, addresses) then
    begin
      for i:=0 to length(addresses)-1 do
      begin
        case addresses[i].jumptype of
          jtUnconditional:
            result:=result+' '+inttohex(addresses[i].address,8)+'(Un)';

          jtConditional:
            result:=result+' '+inttohex(addresses[i].address,8)+'(Con)';

          jtCall:
            result:=result+' '+inttohex(addresses[i].address,8)+'(Call)';
        end;
      end;
    end;
  end;
end;

procedure TDisassemblerLine.renderLine(var address: ptrUint; linestart: integer; selected: boolean=false; focused: boolean=false);
var isbp: boolean;
    baseofsymbol: ptrUint;
    symbolname: string;
    refferencedby: string;
    refferencedbylinecount: integer;
    refferencedbyheight: integer;
    refferencedbystrings: array of string;
    i,j: integer;

    paddressstring: pchar;
    pbytestring: pchar;
    popcodestring: pchar;
    pspecialstring: pchar;

    textcolor: TColor;

    bp: PBreakpoint;
begin
  top:=linestart;
  faddress:=address;
  isselected:=selected;

  fisJump:=cefuncproc.isjumporcall(faddress, fJumpsTo);


  height:=0;
  baseofsymbol:=0;
  symbolname:=symhandler.getNameFromAddress(address,symhandler.showsymbols,symhandler.showmodules,@baseofsymbol);

  if (baseofsymbol>0) and (faddress=baseofsymbol) then
  begin
    if textheight=-1 then
      textheight:=fcanvas.TextHeight(symbolname);

    height:=height+textheight+1+10;
  end;

  refferencedbylinecount:=0;


  if (frmDissectCode<>nil) and (frmDissectCode.dissectcode<>nil) and (frmDissectCode.dissectcode.done) then
  begin
    refferencedby:=buildReferencedByString;
    if refferencedby<>'' then
    begin
      fcanvas.Font.Style:=[fsBold, fsItalic];
      if referencedbylineheight=-1 then
        referencedbylineheight:=fcanvas.textheight('xxx');

      refferencedbylinecount:=1+(fcanvas.TextWidth(refferencedby) div (fbitmap.width - 10 ));

      setlength(refferencedbystrings, refferencedbylinecount);

      j:=1;
      i:=0;
      refferencedbyheight:=0;
      while (j<=length(refferencedby)) do
      begin
        refferencedbystrings[i]:='';
        while (fcanvas.TextWidth(refferencedbystrings[i])<(fbitmap.width-10)) and (j<=length(refferencedby)) do
        begin
          refferencedbystrings[i]:=refferencedbystrings[i]+refferencedby[j];
          inc(j);
        end;

        refferencedbyheight:=refferencedbyheight+referencedbylineheight;

        inc(i);
      end;

      height:=height+refferencedbyheight;
      fcanvas.Font.Style:=[];
    end;
  end;

  fdisassembled:=visibleDisassembler.disassemble(address,fdescription);


  if boldheight=-1 then
  begin
    fcanvas.Font.Style:=[fsbold];
    boldheight:=fcanvas.TextHeight(fdisassembled)+1;
    fcanvas.Font.Style:=[];
  end;

  height:=height+boldheight+1;

  if debuggerthread<>nil then
    bp:=debuggerthread.isBreakpoint(faddress)
  else
    bp:=nil;


  isbp:=((kdebugger.isactive) and (kdebugger.isExecutableBreakpoint(faddress))) or
        ((bp<>nil) and (bp.breakpointTrigger=bptExecute));
  
  if selected then
  begin
    if not isbp then
    begin
      //default
      if not focused then
      begin
        fcanvas.Brush.Color:=clGradientActiveCaption;
        fcanvas.Font.Color:=clHighlightText;
      end
      else
      begin
        fcanvas.Brush.Color:=clHighlight;
        fcanvas.Font.Color:=clHighlightText;
      end;
    end
    else
    begin
      //it's a breakpoint
      fCanvas.Brush.Color:=clGreen;
      fCanvas.font.Color:=clWhite;
    end;
    fcanvas.Refresh;



  end
  else
  begin
    //not selected
    if isbp then
    begin
      fCanvas.Brush.Color:=clRed;
      fCanvas.font.Color:=clBlack;
      fcanvas.Refresh
    end else
    begin
      fcanvas.Brush.Color:=clBtnFace;
      fcanvas.Font.Color:=clWindowText;
      fcanvas.Refresh
    end;
  end;
  fcanvas.FillRect(rect(0,top,fbitmap.width,top+height));

  if (baseofsymbol>0) and (faddress=baseofsymbol) then
  begin
    fcanvas.Font.Style:=[fsbold];
    fcanvas.TextOut(fHeaders.Items[0].Left+5,linestart+5,symbolname);
    linestart:=linestart+fcanvas.TextHeight(symbolname)+1+10;
    fcanvas.Font.Style:=[];
  end;

  if (refferencedbylinecount>0) then
  begin
    fcanvas.Font.Style:=[fsBold,fsItalic];
    for i:=0 to refferencedbylinecount-1 do
    begin
      fcanvas.TextOut(fHeaders.Items[0].Left+5,linestart,refferencedbystrings[i]);
      linestart:=linestart+fcanvas.TextHeight(refferencedbystrings[i]);
    end;
    fcanvas.Font.Style:=[];

  end;

  addressstring:=inttohex(visibleDisassembler.LastDisassembleData.address,8);
  bytestring:=visibleDisassembler.getLastBytestring;
  opcodestring:=visibleDisassembler.LastDisassembleData.prefix+visibleDisassembler.LastDisassembleData.opcode;

  parameterstring:=visibleDisassembler.LastDisassembleData.parameters+' ';
  specialstring:=visibleDisassembler.DecodeLastParametersToString;





 // splitDisassembledString(fdisassembled, true, addressstring, bytestring, opcodestring, specialstring, @MemoryBrowser.lastdebugcontext);
  if symhandler.showmodules then
    addressString:=symbolname
  else
    addressString:=truncatestring(addressString, fHeaders.Items[0].Width-2);

  bytestring:=truncatestring(bytestring, fHeaders.Items[1].Width-2);
  opcodestring:=truncatestring(opcodestring, fHeaders.Items[2].Width-2);
  specialstring:=truncatestring(specialstring, fHeaders.Items[3].Width-2);


  if MemoryBrowser.lastdebugcontext.{$ifdef cpu64}rip{$else}EIP{$endif}=faddress then
    addressString:='>>'+addressString;


  //set pointers to strings
  paddressstring:=nil;
  pbytestring:=nil;
  popcodestring:=nil;
  pspecialstring:=nil;

  if length(addressstring)>0 then
    paddressstring:=@addressstring[1];

  if length(bytestring)>0 then
    pbytestring:=@bytestring[1];

  if length(opcodestring)>0 then
    popcodestring:=@opcodestring[1];

  if length(specialstring)>0 then
    pspecialstring:=@specialstring[1];


  textcolor:=fcanvas.Font.Color;
  handledisassemblerplugins(@paddressString, @pbytestring, @popcodestring, @pspecialstring, @textcolor);
  fcanvas.font.color:=textcolor;


  fcanvas.TextRect(rect(fHeaders.Items[0].Left, linestart, fHeaders.Items[0].Right, linestart+height), fHeaders.Items[0].Left+1,linestart, paddressString);
  fcanvas.TextRect(rect(fHeaders.Items[1].Left, linestart, fHeaders.Items[1].Right, linestart+height),fHeaders.Items[1].Left+1,linestart, pbytestring);

  fcanvas.font.Style:=fcanvas.font.Style+[fsBold];
  fcanvas.TextRect(rect(fHeaders.Items[2].Left, linestart, fHeaders.Items[2].Right, linestart+height),fHeaders.Items[2].Left+1,linestart, popcodestring);
  fcanvas.font.Style:=fcanvas.font.Style-[fsBold];

  i:=fcanvas.TextWidth(popcodestring+'  ');
  j:=fcanvas.textwidth('XXXXXX');

  if i>j then
    i:=fHeaders.Items[2].Left+1+i+fcanvas.textwidth(' ')
  else
    i:=fHeaders.Items[2].Left+1+j;


  DrawTextRectWithColor(rect(fHeaders.Items[2].Left, linestart, fHeaders.Items[2].Right, linestart+height),i,linestart, parameterstring);

  fcanvas.TextRect(rect(fHeaders.Items[3].Left, linestart, fHeaders.Items[3].Right, linestart+height),fHeaders.Items[3].Left+1,linestart, pspecialstring);

  fInstructionCenter:=linestart+(fcanvas.TextHeight(opcodestring) div 2);

  if focused then
      fcanvas.DrawFocusRect(rect(0,top,fbitmap.width,top+height));

  if selected then //restore
  begin
    fcanvas.Brush.Color:=clBtnFace;
    fcanvas.Font.Color:=clWindowText;
    fcanvas.Refresh;
  end;
end;

procedure TDisassemblerLine.DrawTextRectWithColor(const ARect: TRect; X, Y: integer; const Text: string);
var defaultfontcolor: TColor;
    i: integer;
    start: integer;

    s: string;
begin
  defaultfontcolor:=fcanvas.Font.color;
  start:=1;
  s:='';
  i:=1;
  while i<length(text) do
  begin
    case text[i] of
      '{':
      begin
        s:=copy(text, start,i-start);
        fcanvas.TextRect(ARect,x,y,s);
        x:=x+fcanvas.TextWidth(s);

        inc(i);
        while i<length(text) do
        begin
          case text[i] of
            'N': fcanvas.Font.color:=defaultfontcolor;
            'H': if isselected then fcanvas.Font.color:=clYellow else fcanvas.Font.color:=clBlue;
            'R': fcanvas.font.color:=clRed;
            'S': if isselected then fcanvas.Font.color:=clLime else fcanvas.font.color:=clLime;
            '}':
            begin
              inc(i);
              break;
            end;

            else raise exception.create('Invalid disassembly');
          end;
          inc(i);
        end;

        start:=i;
      end;

      else inc(i);
    end;


  end;

  s:=copy(text, start,i-start);
  fcanvas.TextRect(ARect,x,y,s);

  fcanvas.Font.color:=defaultfontcolor;
end;

procedure TDisassemblerLine.handledisassemblerplugins(addressStringPointer: pointer; bytestringpointer: pointer; opcodestringpointer: pointer; specialstringpointer: pointer; textcolor: PColor);
begin
  pluginhandler.handledisassemblerplugins(faddress, addressStringPointer, bytestringpointer, opcodestringpointer, specialstringpointer, textcolor);
end;

function TDisassemblerLine.getHeight: integer;
begin
  result:=height;
end;

function TDisassemblerLine.getTop: integer;
begin
  result:=top;
end;

constructor TDisassemblerLine.create(bitmap: TBitmap; headersections: THeaderSections);
begin
  fCanvas:=bitmap.canvas;
  fBitmap:=bitmap;
  fheaders:=headersections;
  boldheight:=-1; //bypass for memory leak
  textheight:=-1;
  referencedbylineheight:=-1;

  height:=fCanvas.TextHeight('X');
end;

end.
