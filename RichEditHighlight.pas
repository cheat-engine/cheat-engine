{*******************************************************}
{               RichEdit Syntax HighLight               }
{                     version 3.2                       }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{                                                       }
{ Anderson Rodrigues Barbieri                           }
{                                                       }
{*******************************************************}
{Cheat Engine todo:                                     }
{Remove unnecesary code for the highlighting of the     }
{other types. I doubt CE will use PHP... done           }
{*******************************************************}

unit RichEditHighlight;

Interface

uses
 Windows,
 SysUtils,
 Messages,
 Classes,
 Controls,
 RichEdit,
 psvCPlusPlus,
 psvAutoAssembler,
 psvRichSyntax,
{ psvHTMLRTF,
 psvPas,
 psvDFM,
 psvJScript,
 psvPHPScript,
 psvSQL,
 psvVB,
 psvVBScript,
 psvXML, } 
 comCtrls,
 graphics;

type

 TSyntaxType = (stCPlusPlus, stDFM, stJScript, stSQL, stVB, stVBScript, stXML, stDelphi);

 TpsvTokenType =
 (
   ttAsm,
   ttComment,
   ttIdentifier,
   ttKey,
   ttNull,
   ttNumber,
   ttSpace,
   ttString,
   ttSymbol,
   ttUnknown,
   ttDirective,
   ttChar,
   ttHex,
   ttOctal,
   ttFloat,
   ttInvalidSymbol,
   ttVariable,
   ttElement,
   ttAttribute,
   ttText,
   ttCDATA,
   ttEntityRef,
   ttProcessingInstruction,
   ttQuoteAttrValue,
   ttAposAttrValue,
   ttQuoteEntityRef,
   ttAposEntityRef,
   ttEqual,
   ttnsAttribute,
   ttnsQuoteAttrValue,
   ttnsAposAttrValue,
   ttnsQuoteEntityRef,
   ttnsAposEntityRef,
   ttnsEqual);

 TpsvTokenTypes = set of TpsvTokenType;

 TRichEditSyntax = class(TCustomRichEdit)
 private
   FLoading : boolean;
   FSyntax : TpsvRTFSyntax;
   FUseDefaultColors : boolean;
   FAsmColor,
   FCommentColor,
   FIdentifierColor,
   FKeyColor,
   FNullColor,
   FNumberColor,
   FSpaceColor,
   FStringColor,
   FSymbolColor,
   FUnknownColor,
   FDirectiveColor,
   FCharColor,
   FHexColor,
   FOctalColor,
   FFloatColor,
   FInvalidSymbolColor,
   FVariableColor,
   FElementColor,
   FAttributeColor,
   FTextColor,
   FCDATAColor,
   FEntityRefColor,
   FProcessingInstructionColor,
   FQuoteAttrValueColor,
   FAposAttrValueColor,
   FQuoteEntityRefColor,
   FAposEntityRefColor,
   FEqualColor,
   FnsAttributeColor,
   FnsQuoteAttrValueColor,
   FnsAposAttrValueColor,
   FnsQuoteEntityRefColor,
   FnsAposEntityRefColor,
   FnsEqualColor : TColor;
   FSyntaxType : TSyntaxType;
   procedure SetSyntaxType(AValue : TSyntaxType);
   procedure SetUseDefaultColors(const Value: boolean);
   procedure CreateColorTable;
 protected
   procedure Change; override;
 public
   constructor Create(AOwner : TComponent); override;
   destructor Destroy; override;
   function   GetTokenSet(ALanguage : TSyntaxType) : TpsvTokenTypes;
   procedure  LoadFromStream(AStream : TStream);
   procedure  LoadFromStreamToSelection(AStream: TStream);
   procedure  SaveToStream(AStream : TStream);
   procedure  LoadFromFile(AFileName : string);
   procedure  SaveToFile(AFileName : string);
   function   SelectLine(Index: Integer): boolean;
   procedure  UpdateSyntax; virtual;
   procedure  UpdateLineSyntax; virtual;
 published
   property SyntaxType : TSyntaxType read FSyntaxType write SetSyntaxType default stDelphi;
   property AsmColor : TColor read FasmColor write FAsmColor;
   property CommentColor : TColor read FCommentColor write FCommentColor;
   property IdentifierColor : TColor read FIdentifierColor write FIdentifierColor;
   property KeyColor    : TColor read FKeyColor write FKeyColor;
   property NullColor   : TColor read FNullColor write FNullColor;
   property NumberColor : TColor read FNumberColor write FNumberColor;
   property SpaceColor  : TColor read FSpaceColor write FSpaceColor;
   property StringColor : TColor read FStringColor write FStringColor;
   property SymbolColor : TColor read FSymbolColor write FSymbolColor;
   property UnknownColor: TColor read FUnknownColor write FUnknownColor;
   property DirectiveColor : TColor read FDirectiveColor write FDirectiveColor;
   property CharColor : TColor read FCharColor write FCharColor;
   property HexColor : TColor read FHexColor write FHexColor;
   property OctalColor : TColor read FOctalColor write foctalColor;
   property FloatColor : TColor read FFloatColor write FFloatColor;
   property InvalidSymbolColor : TColor read FInvalidSymbolColor write FInvalidSymbolColor;
   property VariableColor : TColor read FVariableColor write FVariableColor;
   property ElementColor : TColor read FElementColor write FElementColor;
   property AttributeColor : TColor read FAttributeColor write FAttributeColor;
   property TextColor : TColor read FTextColor write FTextColor;
   property CDATAColor : TColor read FCDATAColor write FCDATAColor;
   property EntityRefColor : TColor read FEntityRefColor write fEntityrefColor;
   property ProcessingInstructionColor : TColor read FProcessingInstructionColor write FProcessingInstructionColor;
   property QuoteAttrValueColor : TColor read FQuoteAttrValueColor write FQuoteAttrValueColor;
   property AposAttrValueColor : TColor read FAposAttrValueColor write FAposAttrValueColor;
   property QuoteEntityRefColor : TColor read FQuoteEntityRefColor write FQuoteEntityRefColor;
   property AposEntityRefColor : TColor read FAposEntityRefColor write FAposEntityRefColor;
   property EqualColor : TColor read FEqualColor write FEqualColor;
   property nsAttributeColor : TColor read FnsAttributeColor write FnsAttributeColor;
   property nsQuoteAttrValueColor : TColor read FnsQuoteAttrValueColor write FnsQuoteAttrValueColor;
   property nsAposAttrValueColor : TColor read FnsAposAttrValueColor write FnsAposAttrValueColor;
   property nsQuoteEntityRefColor  : TColor read FnsQuoteEntityRefColor write FnsQuoteEntityRefColor;
   property nsAposEntityRefColor : TColor read FnsAposEntityRefColor write FnsAposEntityRefColor;
   property nsEqualColor : TColor read FnsEqualColor write FnsEqualColor;
   property UseDefaultColors : boolean read FUseDefaultColors write SetUseDefaultColors default true;
   property Align;
   property Alignment;
   property Anchors;
   property BiDiMode;
   property BorderStyle;
   property BorderWidth;
   property Color;
   property Ctl3D;
   property DragCursor;
   property DragKind;
   property DragMode;
   property Enabled;
   property HideScrollBars;
   property ImeMode;
   property ImeName;
   property Constraints;
   property Lines;
   property MaxLength;
   property ParentBiDiMode;
   property ParentColor;
   property ParentCtl3D;
   property ParentShowHint;
   property PopupMenu;
   property ReadOnly;
   property ScrollBars;
   property ShowHint;
   property TabOrder;
   property TabStop default True;
   property Visible;
   property WantTabs;
   property WantReturns;
   property OnChange;
   property OnContextPopup;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDock;
   property OnEndDrag;
   property OnEnter;
   property OnExit;
   property OnKeyDown;
   property OnKeyPress;
   property OnKeyUp;
   property OnMouseDown;
   property OnMouseMove;
   property OnMouseUp;
   property OnMouseWheel;
   property OnMouseWheelDown;
   property OnMouseWheelUp;
   property OnProtectChange;
   property OnResizeRequest;
   property OnSaveClipboard;
   property OnSelectionChange;
   property OnStartDock;
   property OnStartDrag;
 end;

procedure Register;

Implementation

procedure Register;
begin
  RegisterComponents('Additional', [TRichEditSyntax]);
end;


function doStreamIn(dwCookie: Longint; pbBuff: PByte;
                    cb: Longint; var pcb: Longint): Longint; stdcall;
begin
  pcb := 0;
  try
    pcb := TStream(dwCookie).Read(pbBuff^, cb);
    Result := 0;
  except
    Result := 1;
  end;
end;


{ TRichEditSyntax }

constructor TRichEditSyntax.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle:= ControlStyle + [csOpaque];
  FLoading := False;
  FSyntaxType := stDelphi;
  FSyntax := TpsvAARTF.Create;
  FAsmColor := clGreen;
  FCommentColor := clBlue;
  FIdentifierColor := clBlack;
  FKeyColor := clNavy;
  FNullColor := clBlack;
  FNumberColor := clBlack;
  FSpaceColor := clNavy;
  FStringColor := clBlack;
  FSymbolColor := clBlack;
  FUnknownColor := clBlack;
  FDirectiveColor := clBlack;
  FCharColor := clBlack;
  FHexColor := clBlack;
  FOctalColor := clBlack;
  FFloatColor := clBlack;
  FInvalidSymbolColor :=  clRed;
  FElementColor := clGreen;
  FAttributeColor := clMaroon;
  FTextColor := clBlack;
  FCDATAColor := clBlue;
  FEntityRefColor := clBlack;
  FProcessingInstructionColor := clGreen;
  FQuoteAttrValueColor := clBlue;
  FAposAttrValueColor := clBlack;
  FQuoteEntityRefColor := clRed;
  FAposEntityRefColor := clBlack;
  FEqualColor := clBlack;
  FnsAttributeColor := clGreen;
  FnsQuoteAttrValueColor := clMaroon;
  FnsAposAttrValueColor := clBlack;
  FnsQuoteEntityRefColor := clBlack;
  FnsAposEntityRefColor := clGreen;
  FnsEqualColor := clGreen;
  PlainText := true;
end;

procedure TRichEditSyntax.CreateColorTable;
begin
  case FSyntaxType of
    stCPlusPlus :
      begin
        FSyntax.CreateColorTable([
          FAsmColor,
          FCommentColor,
          FDirectiveColor,
          FIdentifierColor,
          FKeyColor,
          FNumberColor,
          FFloatColor,
          FHexColor,
          FOctalColor,
          FSpaceColor,
          FStringColor,
          FCharColor,
          FSymbolColor,
          FUnknownColor,
          clBlack]);
      end;
    stDFM :
      begin
        FSyntax.CreateColorTable([
          FCommentColor,
          FIdentifierColor,
          FKeyColor,
          FNumberColor,
          FSpaceColor,
          FStringColor,
          FSymbolColor,
          FUnknownColor]);
     end;
    stJScript :
      begin
        FSyntax.CreateColorTable([
        FCommentColor,
        FIdentifierColor,
        FKeyColor,
        FNumberColor,
        FSpaceColor,
        FStringColor,
        FSymbolColor,
        FUnknownColor]);
      end;
    stSQL :
      begin
        FSyntax.CreateColorTable([
        FCommentColor,
        FIdentifierColor,
        FKeyColor,
        FNumberColor,
        FSymbolColor,
        FStringColor,
        FUnknownColor,
        clBlack]);
      end;
    stVB :
      begin
        FSyntax.CreateColorTable([
          FCommentColor,
          FIdentifierColor,
          FKeyColor,
          FNumberColor,
          FSpaceColor,
          FStringColor,
          FSymbolColor,
          FUnknownColor]);
      end;
    stVBScript :
      begin
        FSyntax.CreateColorTable([
          FCommentColor,
          FIdentifierColor,
          FKeyColor,
          FNumberColor,
          FSpaceColor,
          FStringColor,
          FSymbolColor,
          FUnknownColor]);
      end;
    stXML :
      begin
       FSyntax.CreateColorTable([
       FElementColor,
       FAttributeColor,
       FnsAttributeColor,
       FEqualColor,
       FnsEqualColor,
       FQuoteAttrValueColor,
       FAPosAttrValueColor,
       FnsQuoteAttrValueColor,
       FnsAPosAttrValueColor,
       FTextColor,
       FCDATAColor]);
      end;
    stDelphi :
      begin
        FSyntax.CreateColorTable([
        FCommentColor,
        FIdentifierColor,
        FKeyColor,
        FNumberColor,
        FSpaceColor,
        FStringColor,
        FSymbolColor,
        FUnknownColor]);
      end;
  end;
end;

destructor TRichEditSyntax.Destroy;
begin
  if Assigned(FSyntax) then
   begin
     FSyntax.Free;
     FSyntax := nil;
   end;
  inherited;
end;

function TRichEditSyntax.GetTokenSet(
  ALanguage: TSyntaxType): TpsvTokenTypes;
begin
  case ALanguage of
   stCPlusPlus : Result := [ttAsm, ttComment, ttDirective, ttIdentifier, ttKey, ttNull,
    ttNumber, ttSpace, ttString, ttSymbol, ttUnknown,ttChar, ttFloat, ttHex, ttOctal];

   stDFM : Result := [ttComment, ttIdentifier, ttKey, ttNull, ttNumber, ttSpace,
    ttString, ttSymbol, ttUnknown];

   stJScript : Result := [ttComment, ttIdentifier, ttKey, ttNull, ttNumber, ttSpace,
    ttString, ttSymbol, ttUnknown];

   stSQL : Result := [ttComment, ttIdentifier, ttKey, ttNull,  ttNumber, ttString,
    ttSymbol, ttUnknown];

   stVB : Result := [ttComment, ttIdentifier, ttKey, ttNull, ttNumber, ttSpace,
    ttString, ttSymbol, ttUnknown];

   stVBScript : Result := [ttComment, ttIdentifier, ttKey, ttNull, ttNumber, ttSpace,
    ttString, ttSymbol, ttUnknown];

   stXML : Result := [ttNull,
    ttElement,
    ttAttribute,
    ttText,
    ttCDATA,
    ttEntityRef,
    ttProcessingInstruction,
    ttComment,
    ttQuoteAttrValue,
    ttAposAttrValue,
    ttQuoteEntityRef,
    ttAposEntityRef,
    ttSymbol,
    ttSpace,
    ttEqual,
    ttnsAttribute,
    ttnsQuoteAttrValue,
    ttnsAposAttrValue,
    ttnsQuoteEntityRef,
    ttnsAposEntityRef,
    ttnsEqual];

   stDelphi : Result := [ttAsm, ttComment, ttIdentifier, ttKey, ttNull, ttNumber,
    ttSpace, ttString, ttSymbol, ttUnknown];
  end;
end;

procedure TRichEditSyntax.LoadFromFile(AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TRichEditSyntax.LoadFromStream(AStream: TStream);
var
 S : string;
 MS : TMemoryStream;
 EditStream: TEditStream;
 Format: WPARAM;
begin
  try
  FLoading := true;
  SetLength(S,AStream.Size);
  AStream.Position := 0;
  MS := TMemoryStream.Create;
  AStream.ReadBuffer(S[1],AStream.Size);
  FSyntax.SetText(S);
  FSyntax.CreateFontTable([Self.Font]);
  FSyntax.ConvertToRTFStream(MS);
  MS.Position := 0;
  with EditStream do
    begin
    dwCookie    := Longint(MS);
    pfnCallBack := @doStreamIn;
    dwError := 0;
    end;

  Format := SF_RTF;
  SendMessage(Handle, EM_STREAMIN, Format,
            Longint(@EditStream));
  if EditStream.dwError <> 0 then
      raise EOutOfResources.Create('Cannot load RTF text');
  finally
    PlainText := true;
    FLoading := false;
  end;
end;

procedure TRichEditSyntax.LoadFromStreamToSelection(AStream: TStream);
var
 S : string;
 MS : TMemoryStream;
 EditStream: TEditStream;
 Format: WPARAM;
begin
  try
  FLoading := true;
  SetLength(S,AStream.Size);
  AStream.Position := 0;
  MS := TMemoryStream.Create;
  AStream.ReadBuffer(S[1],AStream.Size);
  FSyntax.SetText(S);
  FSyntax.CreateFontTable([Self.Font]);
  FSyntax.ConvertToRTFStream(MS);
  MS.Position := 0;
  with EditStream do
    begin
    dwCookie    := Longint(MS);
    pfnCallBack := @doStreamIn;
    dwError := 0;
    end;
  // faz carregar apenas para o texto selecionado
  Format := SF_RTF or SFF_SELECTION;
//  lockWindowUpdate(Handle);
  SendMessage(Handle, EM_STREAMIN, Format, Longint(@EditStream));
//  lockWindowUpdate(0);
  if EditStream.dwError <> 0 then
      raise EOutOfResources.Create('Cannot load RTF text');
  finally
    PlainText := true;
    FLoading := false;
  end;
end;

procedure TRichEditSyntax.SaveToFile(AFileName: string);
begin
  PlainText := true;
  Lines.SaveToFile(AFileName);
end;

procedure TRichEditSyntax.SaveToStream(AStream: TStream);
begin
  PlainText := true;
  Lines.SaveToStream(AStream);
end;

procedure TRichEditSyntax.SetSyntaxType(AValue: TSyntaxType);
begin
  FSyntaxType := AValue;
  if Assigned(FSyntax) then
   begin
     FSyntax.Free;
     FSyntax := nil;
   end;
  case FSyntaxType of
   stCPlusPlus : FSyntax := TpsvCppRTF.Create;
  // stDFM : FSyntax := TpsvDFMRTF.Create;
   //stJScript : FSyntax := TpsvJScriptRTF.Create;
   //stSQL : FSyntax := TpsvSQLRTF.Create;
   //stVB : fSyntax := TpsvVBRTF.Create;
  // stVBScript : FSyntax := TpsvVBScriptRTF.Create;
  // stDelphi : FSyntax := TpsvPasRTF.Create;
   //stXML : FSyntax := TpsvXMLRTF.Create;
  end;
  Change;
end;

procedure TRichEditSyntax.SetUseDefaultColors(const Value: boolean);
begin
  FUseDefaultColors := Value;
  if FUseDefaultColors then
   begin
     if Assigned(FSyntax) then
      begin
        FSyntax.SetupDefaultColors;
      end;
   end
    else
      begin
        if Assigned(FSyntax) then
         Self.CreateColorTable;
      end;
end;


function TRichEditSyntax.SelectLine(Index: Integer): boolean;
var
  StartPos, EndPos: integer;
begin
  result := false;
  if Index < 0 then Exit;
  StartPos :=  Perform(EM_LINEINDEX, Index, 0);
  if StartPos <> -1 then
    begin
      EndPos := SendMessage(Handle, EM_LINEINDEX, Index + 1, 0);
      if EndPos = -1 then
        EndPos := StartPos + Perform(EM_LINELENGTH, StartPos, 0);
      Perform(EM_SETSEL, StartPos, EndPos);
      result := true;
    end;
end;



procedure TRichEditSyntax.UpdateSyntax;
var
  TempMS: TStringStream;
  pos, top: Integer;
  SaveOnChange: TNotifyEvent;
  SaveOnKeyDown : TKeyEvent;
  SaveOnSelectionChange : TNotifyEvent;
  EventMask : integer;
begin
 if (Length(Text) <= 0) then
  exit;
 if FLoading then
  Exit;
 Perform(WM_SETREDRAW, 0, 0);
 EventMask := Perform(EM_SETEVENTMASK, 0, 0);
 SaveOnChange := OnChange;
 SaveOnKeyDown := OnKeyDown;
 SaveOnSelectionChange := OnSelectionChange;
 OnChange := nil;
 OnKeyDown := nil;
 OnSelectionChange := nil;
 pos := Selstart;
 top := Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
 TempMS := TStringStream.Create(Text);
  try
    LoadFromStream(TempMS);
    Perform(EM_LINESCROLL, 0, top);
  finally
    PlainText := True;
    SelStart := Pos;
    TempMS.Free;
    OnChange := SaveOnChange;
    OnKeyDown := SaveOnKeyDown;
    OnSelectionChange := SaveOnSelectionChange;
    Perform(EM_SETEVENTMASK, 0, EventMask);
    Perform(WM_SETREDRAW, -1, 0);
    InvalidateRect(Handle, nil,true);
   end;
end;

procedure TRichEditSyntax.UpdateLineSyntax;
var
  TempMS: TStringStream;
  pos: Integer;
  SaveOnChange: TNotifyEvent;
  SaveOnKeyDown : TKeyEvent;
  SaveOnSelectionChange : TNotifyEvent;
  EventMask : integer;
  I: integer;
  S: string;
begin
 if (Length(Text) < 3) then
  exit;
 if FLoading then
  Exit;

 EventMask := Perform(EM_SETEVENTMASK, 0, 0);
 Perform(EM_HIDESELECTION,1,0);
 SaveOnChange := OnChange;
 SaveOnKeyDown := OnKeyDown;
 SaveOnSelectionChange := OnSelectionChange;
 OnChange := nil;
 OnKeyDown := nil;
 OnSelectionChange := nil;
 pos := Selstart;
 enabled:= false;

  if SelectLine(Perform(EM_LINEFROMCHAR,SelStart,0)) and (SelLength > 2)  then
  begin
    I:= SelLength;
    S:= SelText;

    if (S[I]= #10) or (S[I]= #13) then
    begin
      if (S[I-1]= #10) or (S[I-1]= #13) then
        SelLength:= I - 2
      else
        SelLength:= I - 1;
    end;

      TempMS := TStringStream.Create(SelText);
      try
        LockWindowUpdate(handle);
        LoadFromStreamToSelection(TempMS);
        LockWindowUpdate(0);
        Sellength:= 0;
      finally
        PlainText := True;
        SelStart := Pos;
        TempMS.Free;
        OnChange := SaveOnChange;
        OnKeyDown := SaveOnKeyDown;
        OnSelectionChange := SaveOnSelectionChange;
        Perform(EM_SETEVENTMASK, 0, EventMask);
        Perform(EM_HIDESELECTION,0,0);
        enabled:= true;
        if (visible) and (parent.Visible) then
          SetFocus;
      end;
  end
  else
  begin
      PlainText := True;
      SelStart := Pos;
      OnChange := SaveOnChange;
      OnKeyDown := SaveOnKeyDown;
      OnSelectionChange := SaveOnSelectionChange;
      Perform(EM_SETEVENTMASK, 0, EventMask);
      Perform(EM_HIDESELECTION,0,0);
      Enabled:= true;
      if (visible) and (parent.Visible) then
        SetFocus;

  end;
end;


procedure TRichEditSyntax.Change;
begin
  if HandleAllocated then
    UpdateLineSyntax;
  inherited;
end;

end.