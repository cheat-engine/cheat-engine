unit disassemblerviewlinesunit;

interface

uses windows,sysutils, classes,ComCtrls, graphics, cefuncproc, disassembler,
     debugger, kerneldebugger, symbolhandler, dissectCodeThread;

type TDisassemblerLine=class
  private
    fbitmap: tbitmap;
    fCanvas: TCanvas;
    fHeaders: THeaderSections;
    top: integer;
    height: integer; //height of the line
    fInstructionCenter: integer; //y position of the center of the disassembled line (so no header)
    isselected: boolean;

    faddress: dword;
    fdescription: string;
    fdisassembled: string;

    fisJump: boolean;
    fJumpsTo: dword;
    fdissectcode: TDissectCodethread;

    addressstring: string;
    bytestring: string;
    opcodestring: string;
    specialstring: string;
    function truncatestring(s: string; maxwidth: integer): string;
    function buildReferencedByString: string;
  public
    property address: dword read faddress;
    property instructionCenter: integer read fInstructionCenter;
    function isJumpOrCall(var addressitjumpsto: dword): boolean;
    function getHeight: integer;
    function getTop: integer;
    property description:string read fdescription;
    property disassembled:string read fdisassembled;
    property dissectcode: TDissectCodeThread read fdissectcode write fdissectcode;
    procedure renderLine(var address: dword; linestart: integer; selected: boolean=false; focused: boolean=false);
    procedure drawJumplineTo(yposition: integer; offset: integer; showendtriangle: boolean=true);
    constructor create(bitmap: TBitmap; headersections: THeaderSections; dc: TdissectCodeThread);
end;

implementation

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

function TDisassemblerLine.isJumpOrCall(var addressitjumpsto: dword): boolean;
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
  if fdissectcode.CheckAddress(address, addresses) then
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

procedure TDisassemblerLine.renderLine(var address: dword; linestart: integer; selected: boolean=false; focused: boolean=false);
var isbp: boolean;
    baseofsymbol: dword;
    symbolname: string;
    refferencedby: string;
    refferencedbylinecount: integer;
    refferencedbyheight: integer;
    refferencedbystrings: array of string;
    i,j: integer;
begin
  top:=linestart;
  faddress:=address;

  fisJump:=cefuncproc.isjumporcall(faddress, fJumpsTo);


  height:=0;
  baseofsymbol:=0;
  symbolname:=symhandler.getNameFromAddress(address,symhandler.showsymbols,symhandler.showmodules,@baseofsymbol);

  if (baseofsymbol>0) and (faddress=baseofsymbol) then
  begin
    height:=height+fcanvas.TextHeight(symbolname)+1+10;
  end;

  refferencedbylinecount:=0;
  if fdissectcode<>nil then
  begin

    fcanvas.Font.Style:=[fsBold, fsItalic];
    refferencedby:=buildReferencedByString;
    if refferencedby<>'' then
    begin
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

        refferencedbyheight:=refferencedbyheight+fcanvas.TextHeight(refferencedbystrings[i]);

        inc(i);
      end;

      height:=height+refferencedbyheight;
      fcanvas.Font.Style:=[];
    end;
  end;



  fdisassembled:=disassemble(address,fdescription);
  fcanvas.Font.Style:=[fsbold];
  height:=height+fcanvas.TextHeight(fdisassembled)+1;
  fcanvas.Font.Style:=[];

  isbp:=((kdebugger.isactive) and (kdebugger.isBreakpoint(faddress))) or
        ((debuggerthread<>nil) and (debuggerthread.userisdebugging) and (debuggerthread.isBreakpoint(faddress)));

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

  if (fdissectcode<>nil) and (refferencedbylinecount>0) then
  begin
    fcanvas.Font.Style:=[fsBold,fsItalic];
    for i:=0 to refferencedbylinecount-1 do
    begin
      fcanvas.TextOut(fHeaders.Items[0].Left+5,linestart,refferencedbystrings[i]);
      linestart:=linestart+fcanvas.TextHeight(refferencedbystrings[i]);
    end;
    fcanvas.Font.Style:=[];

  end;

  splitDisassembledString(fdisassembled, true, addressstring, bytestring, opcodestring, specialstring);
  addressString:=truncatestring(addressString, fHeaders.Items[0].Width-2);
  bytestring:=truncatestring(bytestring, fHeaders.Items[1].Width-2);
  opcodestring:=truncatestring(opcodestring, fHeaders.Items[2].Width-2);
  specialstring:=truncatestring(specialstring, fHeaders.Items[3].Width-2);

  fcanvas.TextOut(fHeaders.Items[0].Left+1,linestart,addressString);
  fcanvas.TextOut(fHeaders.Items[1].Left+1,linestart,bytestring);
  fcanvas.TextOut(fHeaders.Items[2].Left+1,linestart,opcodestring);
  fcanvas.TextOut(fHeaders.Items[3].Left+1,linestart,specialstring);

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

function TDisassemblerLine.getHeight: integer;
begin
  result:=height;
end;

function TDisassemblerLine.getTop: integer;
begin
  result:=top;
end;

constructor TDisassemblerLine.create(bitmap: TBitmap; headersections: THeaderSections; dc: TDissectCodethread);
begin
  fCanvas:=bitmap.canvas;
  fBitmap:=bitmap;
  fheaders:=headersections;

  height:=fCanvas.TextHeight('X');
end;

end.
