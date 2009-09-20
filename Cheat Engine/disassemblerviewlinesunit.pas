unit disassemblerviewlinesunit;

interface

uses windows,sysutils, classes,ComCtrls, graphics, disassembler, debugger,kerneldebugger;

type TDisassemblerLine=class
  private
    fbitmap: tbitmap;
    fCanvas: TCanvas;
    fHeaders: THeaderSections;
    top: integer;
    height: integer; //height of the line
    isselected: boolean;

    faddress: dword;
    fdescription: string;
    fdisassembled: string;

    addressstring: string;
    bytestring: string;
    opcodestring: string;
    specialstring: string;
    function truncatestring(s: string; maxwidth: integer): string;

  public
    property address: dword read faddress;
    function getHeight: integer;
    function getTop: integer;
    property description:string read fdescription;
    property disassembled:string read fdisassembled;
    procedure renderLine(var address: dword; linestart: integer; selected: boolean=false; focused: boolean=false);
    constructor create(bitmap: TBitmap; headersections: THeaderSections);
end;

implementation

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


procedure TDisassemblerLine.renderLine(var address: dword; linestart: integer; selected: boolean=false; focused: boolean=false);
var isbp: boolean;
begin
  top:=linestart;


  faddress:=address;
  fdisassembled:=disassemble(address,fdescription);
  height:=fcanvas.TextHeight(fdisassembled)+1;

  isbp:=((kdebugger.isactive) and (address<>0) and (kdebugger.isBreakpoint(address))) or
        ((debuggerthread<>nil) and (debuggerthread.userisdebugging) and (debuggerthread.isBreakpoint(address)));

  if selected then
  begin
    if not isbp then
    begin
      //default
      fcanvas.Brush.Color:=clHighlight;
      fcanvas.Font.Color:=clHighlightText;
    end
    else
    begin
      fCanvas.Brush.Color:=clGreen;
      fCanvas.font.Color:=clWhite;
    end;
    fcanvas.Refresh;    

    fcanvas.FillRect(rect(0,top,fbitmap.width,top+height));

  end
  else
  begin
    //not selected
    if isbp then
    begin
      fCanvas.Brush.Color:=clRed;
      fCanvas.font.Color:=clBlack;
      fcanvas.Refresh
    end;
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

constructor TDisassemblerLine.create(bitmap: TBitmap; headersections: THeaderSections);
begin
  fCanvas:=bitmap.canvas;
  fBitmap:=bitmap;
  fheaders:=headersections;

  height:=fCanvas.TextHeight('X');  
end;

end.
