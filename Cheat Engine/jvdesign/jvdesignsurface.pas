{ Modified for Lazarus by Costas Velissariou (velissariouc@gmail.com) 04/01/2011}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDesingSurface.pas, released on 2005-08-21.

The Initial Developer of the Original Code is Scott J Miles
Portions created by Scott J Miles are Copyright (C) 2005 Scott J Miles.
All Rights Reserved.

Contributor(s): Olivier Sannier (JVCL Integration)

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
  Mantis 3963: When a design surface is active, the ENTIRE form where it is
               located suffers impacts from being in design mode. This can not
               be circumvented because the Designer property is to be set on
               the parent form and it MUST be set for the design mode to be
               effective. The only workaround is to not have anything else
               on the form being designed.

-----------------------------------------------------------------------------}
// $Id: JvDesignSurface.pas 12931 2010-11-28 13:36:50Z ahuser $

unit JvDesignSurface;

{$mode objfpc}{$H+}
{$DEFINE NO_DESIGNHOOK}
interface

uses
  Classes, SysUtils,
  LCLProc, LCLType, LResources, LCLIntf,
  //Messages,
  Forms, Controls, Graphics,
  Dialogs,
  //Windows,
  {$ifdef windows}
  win32proc,
  messages,
  {$endif}
  ExtCtrls, Contnrs,LMessages, Menus, strutils;

type
  TJvDesignSurface = class;

  TJvDesignMessage = function(ASender: TControl; var AMsg: TLMessage;
    const APt: TPoint): Boolean of object;

  TJvDesignCustomMessenger = class(TObject)
  private
    FContainer: TWinControl;
    FOnDesignMessage: TJvDesignMessage;
    FOnChange: TNotifyEvent;
  protected
    procedure SetContainer(AValue: TWinControl); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IsDesignMessage(ASender: TControl; var AMessage: TLMessage): Boolean; virtual;
    procedure Clear; virtual;
    procedure DesignChildren(AContainer: TWinControl; ADesigning: Boolean);
    procedure DesignComponent(AComponent: TComponent; ADesigning: Boolean); virtual;
    property Container: TWinControl read FContainer write SetContainer;
    property OnDesignMessage: TJvDesignMessage read FOnDesignMessage write FOnDesignMessage;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJvDesignCustomMessengerClass = class of TJvDesignCustomMessenger;

  TJvDesignMessageHook = class(TObject)
  private
    FClient: TWinControl;
    FOldProc: TWndMethod;
    FUser: TJvDesignCustomMessenger;
  protected
    procedure HookProc(var AMessage: TLMessage);
    procedure Unhook;
  public
    constructor Create(AUser: TJvDesignCustomMessenger; AClient: TWinControl);
    destructor Destroy; override;
    property Client: TWinControl read FClient;
  end;

  TJvDesignCustomController = class(TObject)
  private
    FSurface: TJvDesignSurface;

  protected
    function GetDragRect: TRect; virtual; abstract;
    //CV function GetShift: TShiftState;
    function KeyDown(AKeyCode: Cardinal): Boolean; virtual; abstract;
    function KeyUp(AKeyCode: Cardinal): Boolean; virtual; abstract;
    function MouseDown(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse): Boolean; virtual; abstract;
    function MouseMove(X, Y: Integer; TheMessage: TLMMouse): Boolean; virtual; abstract;
    function MouseUp(Button: TMouseButton; X, Y: Integer; TheMessage: TLMMouse): Boolean; virtual; abstract;
  public
    constructor Create(ASurface: TJvDesignSurface); virtual;
    property DragRect: TRect read GetDragRect;
    property Surface: TJvDesignSurface read FSurface;
  end;

  TJvDesignCustomControllerClass = class of TJvDesignCustomController;

  TJvDesignHandleId = (dhNone, dhLeftTop, dhMiddleTop, dhRightTop, dhLeftMiddle,
    dhRightMiddle, dhLeftBottom, dhMiddleBottom, dhRightBottom);

  TJvDesignCustomSelector = class(TComponent)
  private
    FSurface: TJvDesignSurface;
  protected
    function GetCount: Integer; virtual; abstract;
    function GetSelection(AIndex: Integer): TControl;  virtual; abstract;
    procedure SetSelection(AIndex: Integer; AValue: TControl); virtual; abstract;
  public
    constructor Create(ASurface: TJvDesignSurface); reintroduce; virtual;
    destructor Destroy; override;
    function IsSelected(AValue: TControl): Boolean; virtual; abstract;
    function GetClientControl(AControl: TControl): TControl; virtual; abstract;
    function GetCursor(AX, AY: Integer): TCursor; virtual; abstract;
    function GetHitHandle(AX, AY: Integer): TJvDesignHandleId; virtual; abstract;
    procedure AddToSelection(AValue: TControl); virtual; abstract;
    procedure ClearSelection; virtual; abstract;
    procedure RemoveFromSelection(AValue: TControl); virtual; abstract;
    procedure ToggleSelection(AValue: TControl);
    procedure Update; virtual; abstract;
    property Count: Integer read GetCount;
    property Selection[AIndex: Integer]: TControl read GetSelection write SetSelection;
    property Surface: TJvDesignSurface read FSurface;
  end;

  TJvDesignCustomSelectorClass = class of TJvDesignCustomSelector;

  TJvDesignObjectArray = array of TObject;
  TJvDesignGetAddClassEvent = procedure(Sender: TObject; var ioClass: string) of object;
{
  TJvDesignOwnerDrawGridEvent = procedure(ASender: TObject; ACanvas: TCanvas;
    ARect: TRect) of object;
}

  TJvDesignSurface = class(TComponent)
  private
    FActive: Boolean;
    FAddClass: string;
    FContainer: TWinControl;
    FContainerHook: TJvDesignMessageHook;
    FController: TJvDesignCustomController;
    FControllerClass: TJvDesignCustomControllerClass;
//    FDrawGrid: Boolean;
    FMessenger: TJvDesignCustomMessenger;
    FMessengerClass: TJvDesignCustomMessengerClass;
    FSelector: TJvDesignCustomSelector;
    FSelectorClass: TJvDesignCustomSelectorClass;
    FUpdateOwner: TComponent;

    FPopupMenu: TPopupMenu;

    procedure MessengerOnChange(sender: tobject);
    procedure fcce(Reader: TReader; const cn: string;
    var ComponentClass: TComponentClass);
  protected
    FOnChange: TNotifyEvent;
    FOnGetAddClass: TJvDesignGetAddClassEvent;
//    FOnOwnerDrawGrid: TJvDesignOwnerDrawGridEvent;
    FOnSelectionChange: TNotifyEvent;
    function GetAddBounds: TRect;
    function GetCount: Integer;
    function GetSelected: TJvDesignObjectArray;
    function GetSelectedContainer: TWinControl;
    function GetSelection(AIndex: Integer): TControl;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NeedContainer;
    procedure NeedController;
    procedure NeedMessenger;
    procedure NeedSelector;
    //procedure PaintContainerBkgnd(ADC: HDC);
    procedure ReaderError(Reader: TReader; const Msg: string; var Handled: Boolean);
    procedure SetActive(AValue: Boolean);
    procedure SetContainer(AValue: TWinControl);
    //procedure SetDrawGrid(const Value: Boolean);
    procedure SetSelection(AIndex: Integer; AValue: TControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Clear: TJvDesignSurface;
    function ContainerToSelectedContainer(const APt: TPoint): TPoint;
    function FindControl(AX, AY: Integer): TControl; virtual;
    function GetCursor(AX, AY: Integer): TCursor; virtual;
    function GetHitHandle(AX, AY: Integer): TJvDesignHandleId; virtual;
    function IsDesignMessage(ASender: TControl; var AMsg: TLMessage; const APt: TPoint): Boolean;
    function LoadFromFile(const AFileName: string): TJvDesignSurface;
    function LoadFromStream(AStream: TStream): TJvDesignSurface;
    procedure AddComponent;
    procedure Change;
    procedure ClearSelection;
    procedure CopyComponents;
    procedure CutComponents;
    procedure DeleteComponents;
    procedure GetAddClass;
    procedure GrowComponents(AGrowWidth, AGrowHeight: Integer);
    procedure NudgeComponents(ANudgeLeft, ANudgeTop: Integer);
    procedure PasteComponents;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure Select(AControl: TControl);
    procedure SelectionChange;
    procedure SelectParent;
    procedure SetSelected(const AValue: array of TObject);
    procedure UpdateDesigner; virtual;
    property Active: Boolean read FActive write SetActive;
    property AddClass: string read FAddClass write FAddClass;
    property Controller: TJvDesignCustomController read FController;
    property ControllerClass: TJvDesignCustomControllerClass read FControllerClass write FControllerClass;
    property Count: Integer read GetCount;
    property Messenger: TJvDesignCustomMessenger read FMessenger;
    property MessengerClass: TJvDesignCustomMessengerClass read FMessengerClass write FMessengerClass;
    property Selected: TJvDesignObjectArray read GetSelected;
    property SelectedContainer: TWinControl read GetSelectedContainer;
    property Selection[AIndex: Integer]: TControl read GetSelection write SetSelection;
    property Selector: TJvDesignCustomSelector read FSelector;
    property SelectorClass: TJvDesignCustomSelectorClass read FSelectorClass write FSelectorClass;
  published
    property Container: TWinControl read FContainer write SetContainer;
//    property DrawGrid: Boolean read FDrawGrid write SetDrawGrid default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetAddClass: TJvDesignGetAddClassEvent read FOnGetAddClass write FOnGetAddClass;
//    property OnOwnerDrawGrid: TJvDesignOwnerDrawGridEvent read FOnOwnerDrawGrid write FOnOwnerDrawGrid;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property PopupMenu: TPopupMenu read fPopupMenu write FPopupMenu;
  end;

  TJvDesignScrollBox = class(TScrollBox)
  protected
    procedure AutoScrollInView(AControl: TControl); //CV override;
  end;

  TJvDesignPanel = class(TCustomPanel)
  private
    FSurface: TJvDesignSurface;
    FOnPaint: TNotifyEvent;
    FDrawRules: Boolean;
    function GetActive: Boolean;
    function GetOnChange: TNotifyEvent;
    function GetOnGetAddClass: TJvDesignGetAddClassEvent;
    function GetOnSelectionChange: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetOnGetAddClass(const Value: TJvDesignGetAddClassEvent);
    procedure SetOnSelectionChange(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
    procedure Paint; override;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    procedure SetDrawRules(const Value: Boolean);
    property Active: Boolean read GetActive write SetActive;
    property Canvas;
    property Surface: TJvDesignSurface read FSurface;
  published
    property DrawRules: Boolean read FDrawRules write SetDrawRules default True;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnGetAddClass: TJvDesignGetAddClassEvent read GetOnGetAddClass write SetOnGetAddClass;
    property OnSelectionChange: TNotifyEvent read GetOnSelectionChange write SetOnSelectionChange;
    property align;
    property BevelInner;
    property BevelOuter;
    property caption;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/trunk/jvcl/run/JvDesignSurface.pas $';
    Revision: '$Revision: 12931 $';
    Date: '$Date: 2010-11-28 15:36:50 +0200 (Κυρ, 28 Νοε 2010) $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Clipbrd,
  Types, //CV
  JvDesignUtils, JvDesignClip, JvDesignImp, JvResources, JvTypes;


//=== { TJvDesignCustomMessenger } ===========================================

constructor TJvDesignCustomMessenger.Create;
begin
  //
end;

destructor TJvDesignCustomMessenger.Destroy;
begin
  //
end;

procedure TJvDesignCustomMessenger.Clear;
begin
  //
end;

procedure TJvDesignCustomMessenger.DesignComponent(AComponent: TComponent; ADesigning: Boolean);
begin
  //
end;

procedure TJvDesignCustomMessenger.DesignChildren(AContainer: TWinControl; ADesigning: Boolean);
var
  I: Integer;
begin
//  for I := 0 to AContainer.ControlCount - 1 do
  for I := 0 to AContainer.ComponentCount - 1 do
    DesignComponent(AContainer.Components[I], ADesigning);
end;

procedure TJvDesignCustomMessenger.SetContainer(AValue: TWinControl);
begin
  FContainer := AValue;
end;

function TJvDesignCustomMessenger.IsDesignMessage(ASender: TControl;
  var AMessage: TLMessage): Boolean;

  function MousePoint: TPoint;
  var r: trect;
  begin
    with TLMMouse(AMessage) do
      MousePoint := Point(XPos, YPos);

    Result := DesignClientToParent(Result, ASender, Container);



  end;

begin
  result:=false;

  if not Assigned(FOnDesignMessage) then
    Result := False
  else
    if AMessage.Msg=CN_NOTIFY then
      begin
        result:=false;
        exit;
      end;

    case AMessage.Msg of
      LM_LCL..LM_INTERFACELAST : result:=true;

      CM_BASE..CM_APPSHOWMENUGLYPHCHANGED:
      begin
       // result:=true;
        case AMessage.Msg of
          CM_SHOWINGCHANGED,CM_HITTEST: result:=false;
        end;

      end;

      $bd11: result:=true;



      LM_MOVE: result:=false;
     // LM_NOTIFY: result:=true; //?
      LM_DESTROY: result:=false;

      LM_CAPTURECHANGED,LM_SYSCOMMAND: result:=false;
     // LM_CONTEXTMENU: result:=true;

      LM_SETFOCUS,LM_SIZE: result:=false;
      LM_ACTIVATE,LM_SHOWWINDOW,LM_KILLFOCUS,LM_SETCURSOR: result:=false;
      LM_NCMOUSEMOVE..LM_NCLBUTTONDBLCLK: result:=true;

      LM_MOUSEFIRST..LM_MOUSELAST:
        Result := FOnDesignMessage(ASender, AMessage, MousePoint);
      LM_KEYDOWN..LM_KEYUP, LM_PAINT, LM_ERASEBKGND, LM_WINDOWPOSCHANGED, CN_KEYDOWN..CN_KEYUP:
        Result := FOnDesignMessage(ASender, AMessage, Point(0, 0));
      else
        Result := False;
    end;

end;

//=== { TJvDesignMessageHook } ===============================================

constructor TJvDesignMessageHook.Create(AUser: TJvDesignCustomMessenger;
  AClient: TWinControl);
begin
  FUser := AUser;
  FClient := AClient;
  FOldProc := FClient.WindowProc;
  FClient.WindowProc := @HookProc;
end;

destructor TJvDesignMessageHook.Destroy;
begin
  Unhook;
  inherited Destroy;
end;

procedure TJvDesignMessageHook.Unhook;
begin
  FClient.WindowProc := FOldProc;
end;

procedure TJvDesignMessageHook.HookProc(var AMessage: TLMessage);
begin
  if not FUser.IsDesignMessage(FClient, AMessage) then
    FOldProc(AMessage);
end;

//=== { TJvDesignCustomController } ==========================================

constructor TJvDesignCustomController.Create(ASurface: TJvDesignSurface);
begin
  FSurface := ASurface;
end;

//CV
function KeyboardStateToShiftState(const KeyboardState: TKeyboardState): TShiftState;
begin
  Result := [];
  if KeyboardState[VK_SHIFT] and $80 <> 0 then Include(Result, ssShift);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then Include(Result, ssCtrl);
  if KeyboardState[VK_MENU] and $80 <> 0 then Include(Result, ssAlt);
  if KeyboardState[VK_LBUTTON] and $80 <> 0 then Include(Result, ssLeft);
  if KeyboardState[VK_RBUTTON] and $80 <> 0 then Include(Result, ssRight);
  if KeyboardState[VK_MBUTTON] and $80 <> 0 then Include(Result, ssMiddle);
end;

{function TJvDesignCustomController.GetShift: TShiftState;
// obones: For C5/D5 compatibility, we must use a local variable
// as KeyboardStateToShiftState with no parameters was introduced
// in D6/C6
var
  KeyState: TKeyBoardState;
begin
  //CV GetKeyboardState(KeyState);
  //CV Result := KeyboardStateToShiftState(KeyState);
end;
 }
//=== { TJvDesignCustomSelector } ============================================

constructor TJvDesignCustomSelector.Create(ASurface: TJvDesignSurface);
begin
  inherited Create(nil);
  FSurface := ASurface;
end;

destructor TJvDesignCustomSelector.Destroy;
begin
  inherited Destroy;
end;

procedure TJvDesignCustomSelector.ToggleSelection(AValue: TControl);
begin
  if IsSelected(AValue) then
    RemoveFromSelection(AValue)
  else
    AddToSelection(AValue);
end;

//=== { TJvDesignSurface } ===================================================

constructor TJvDesignSurface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessengerClass := TJvDesignDesignerMessenger;
  FControllerClass := TJvDesignController;
  FSelectorClass := TJvDesignSelector;
  //FDrawGrid := True;
end;

destructor TJvDesignSurface.Destroy;
begin
  FContainerHook.Free;
  Messenger.Free;
  Controller.Free;
  Selector.Free;
  inherited Destroy;
end;

procedure TJvDesignSurface.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvDesignSurface.SetContainer(AValue: TWinControl);
begin
  FContainer := AValue;
end;

procedure TJvDesignSurface.NeedContainer;
begin
  if (Container = nil) and (Owner is TWinControl) then
    Container := TWinControl(Owner);
  if Container = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Container']);
end;

procedure TJvDesignSurface.NeedController;
begin
  if (Controller = nil) and (ControllerClass <> nil) then
    FController := ControllerClass.Create(Self);
  if Controller = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Controller']);
end;

procedure TJvDesignSurface.MessengerOnChange(sender: tobject);
begin
  change;
end;

procedure TJvDesignSurface.NeedMessenger;
begin
  if (Messenger = nil) and (MessengerClass <> nil) then
  begin
    FMessenger := MessengerClass.Create;
    Messenger.OnDesignMessage := @IsDesignMessage;
    Messenger.OnChange:=@MessengerOnChange;
  end;
  if Messenger = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Messenger']);
end;

procedure TJvDesignSurface.NeedSelector;
begin
  if (Selector = nil) and (SelectorClass <> nil) then
    FSelector := SelectorClass.Create(Self);
  if Selector = nil then
    raise EJVCLException.CreateResFmt(@RsEDesignNilFmt, [ClassName, 'Selector']);
end;

procedure TJvDesignSurface.SetActive(AValue: Boolean);

  procedure Activate;
  begin
    NeedContainer;
    NeedController;
    NeedSelector;
    NeedMessenger;
    Messenger.Container := Container;
    FContainerHook := TJvDesignMessageHook.Create(Messenger, Container);
  end;

  procedure Deactivate;
  begin
    FreeAndNil(FContainerHook);
    if selector<>nil then
      Selector.ClearSelection;
    FreeAndNil(FMessenger);
  end;

begin
  if FActive <> AValue then
  begin
    if AValue then
      Activate
    else
      Deactivate;
    FActive := AValue;
    if FActive then
      SelectionChange;

    if Assigned(Container) then
      Container.Invalidate;
  end;
end;

procedure TJvDesignSurface.UpdateDesigner;
begin
  Selector.Update;
end;

function TJvDesignSurface.GetCount: Integer;
begin
  Result := Selector.Count;
end;

function TJvDesignSurface.GetSelection(AIndex: Integer): TControl;
begin
  Result := Selector.Selection[AIndex];
end;

procedure TJvDesignSurface.SetSelection(AIndex: Integer; AValue: TControl);
begin
  Selector.Selection[AIndex] := AValue;
end;

procedure TJvDesignSurface.ClearSelection;
begin
  Selector.ClearSelection;
end;

procedure TJvDesignSurface.SelectionChange;
begin
  if not (csDestroying in ComponentState) and Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

function TJvDesignSurface.GetSelected: TJvDesignObjectArray;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Selector.Selection[I];
end;

procedure TJvDesignSurface.SetSelected(const AValue: array of TObject);
var
  I: Integer;
begin
  ClearSelection;
  for I := 0 to Length(AValue) - 1 do
    if AValue[I] is TControl then
      Selector.AddToSelection(TControl(AValue[I]));
end;

procedure TJvDesignSurface.Select(AControl: TControl);
begin
  ClearSelection;
  if AControl <> nil then
    Selector.AddToSelection(AControl);
end;

function TJvDesignSurface.FindControl(AX, AY: Integer): TControl;
var
  C, C0: TControl;
  P: TPoint;
  r: trect;
begin
  P := Point(AX, AY);
  //C := Container.ControlAtPos(P, True, True);
  c:=Container.ControlAtPos(p, [capfAllowDisabled, capfAllowWinControls]);

  while (C <> nil) and (C is TWinControl) do
  begin
    {$ifdef windows}
    if GetLCLClientBoundsOffset(c, R) then
    begin
      dec(p.x, R.Left);
      dec(p.Y, R.Top);
    end;
    {$endif}

    Dec(P.X, C.Left);
    Dec(P.Y, C.Top);
    C0 := TWinControl(C).ControlAtPos(P, [capfAllowDisabled, capfAllowWinControls]);
    if (C0 = nil) or (C0.Owner <> C.Owner) then
      Break;
    C := C0;
  end;
  if C = nil then
    C := Container;
  Result := Selector.GetClientControl(C);
end;

function TJvDesignSurface.GetSelectedContainer: TWinControl;
begin
  if Count <> 1 then
    Result := Container
  else
  if (Selection[0] is TWinControl) and
    (csAcceptsControls in Selection[0].ControlStyle) then
    Result := TWinControl(Selection[0])
  else
    Result := Selection[0].Parent;
end;

function TJvDesignSurface.ContainerToSelectedContainer(const APt: TPoint): TPoint;
var
  C: TControl;
  r: trect;
begin
  Result := APt;
  C := SelectedContainer;
  while (C <> Container) and (C <> nil) do
  begin
    {$ifdef windows}
    if (c is Twincontrol) then
    begin
      if GetLCLClientBoundsOffset(C, R) then
      begin
        Dec(result.x, R.Left);
        Dec(result.Y, R.Top);
      end;
    end;
    {$endif}
    Dec(Result.X, C.Left);
    Dec(Result.Y, C.Top);
    C := C.Parent;
  end;
end;

function TJvDesignSurface.GetAddBounds: TRect;
begin
  with Result, Controller do
  begin
    TopLeft := ContainerToSelectedContainer(DragRect.TopLeft);
    BottomRight := ContainerToSelectedContainer(DragRect.BottomRight);
  end;
end;

procedure TJvDesignSurface.GetAddClass;
begin
  if Assigned(FOnGetAddClass) then
    FOnGetAddClass(Self, FAddClass);
end;

procedure TJvDesignSurface.AddComponent;
var
  CC: TComponentClass;
  C: TComponent;
  CO: TControl;

  function GetBounds: TRect;
  begin
    Result := GetAddBounds;
    if DesignRectWidth(Result) = 0 then
      Result.Right := Result.Left + CO.Width;
    if DesignRectHeight(Result) = 0 then
      Result.Bottom := Result.Top + CO.Height;
  end;

begin
  CC := TComponentClass(GetClass(AddClass));
  if (CC <> nil) and (SelectedContainer <> nil) then
  begin
    //C := CC.Create(Owner);
    //C.Name := DesignUniqueName(Owner, AddClass);
    C := CC.Create(Container);
    C.Name := DesignUniqueName(Container, AddClass);
    if C is TControl then
    begin
      CO := TControl(C);
      CO.Parent := SelectedContainer;
      CO.BoundsRect := GetBounds;
      CO.PopupMenu:= container.PopupMenu;


      Select(CO);
    end;
    Messenger.DesignComponent(C, Active);
    SelectionChange;
    Change;
    AddClass := '';
  end;
end;

procedure TJvDesignSurface.NudgeComponents(ANudgeLeft, ANudgeTop: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Selection[I] do
    begin
      Left := Left + ANudgeLeft;
      Top := Top + ANudgeTop;
    end;
  Change;
end;

procedure TJvDesignSurface.GrowComponents(AGrowWidth, AGrowHeight: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Selection[I] do
    begin
      Width := DesignMax(1, Width + AGrowWidth);
      Height := DesignMax(1, Height + AGrowHeight);
    end;
  Change;
end;

procedure DeleteComponent(c: TObject);
var i: integer;
begin
  if (c is TWinControl) then
  begin
    while TWinControl(c).ControlCount>0 do
      deleteComponent((c as TWinControl).controls[0]);
  end;

  if (c is TComponent) then
  begin
    //delete the possible children it might have
    while (c as tcomponent).ComponentCount>0 do
      deleteComponent((c as tcomponent).Components[0]);
  end;

  c.Free;
end;

procedure TJvDesignSurface.DeleteComponents;
var
  I: Integer;
  z: array of tobject;
begin
  setlength(z,count);
  for i:=0 to count-1 do
    z[i]:=selection[i];

  ClearSelection;
  SelectionChange;

  for i:=0 to length(z)-1 do
  begin
    deleteComponent(z[i]); //.free;
  end;

{  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      z:=selection[i];
      z.free;
//      selection[i].Caption:='del me';
      //Selection[I].Free;
    end;}


  Change;

end;

procedure TJvDesignSurface.CopyComponents;
var
  LFMStream: TStringStream;
  BinStream: TMemoryStream;
  DestroyDriver: Boolean;
  w: TWriter;
  i,j: integer;

  found: boolean;
begin
  LFMStream:=tstringstream.create('');

  try
    for i:=0 to count-1 do
    begin
      if (selected[i] is TComponent) then
      begin
        found:=false;
        for j:=0 to count-1 do
          if selection[j].IsParentOf(selection[i]) then
          begin
            found:=true;
            break;
          end;

        if found then continue; //don't save children of selected objects

        BinStream:=TMemoryStream.Create;
        try
          try
            // write component to binary stream

            DestroyDriver:=false;
            w:=CreateLRSWriter(BinStream,DestroyDriver);
            try
              w.Root:=container;
              w.WriteComponent(TComponent(selected[i]));
            finally
              if DestroyDriver then w.Driver.Free;
              w.Free;
            end;
          except
            exit;
          end;
          try
            // transform binary to text
            BinStream.Position:=0;
            LRSObjectBinaryToText(BinStream,LFMStream);
          except
            exit;
          end;
        finally
          binstream.free;
        end
      end;
    end;

    clipboard.AsText:=LFMStream.DataString;
  finally
    LFMStream.free;
  end;
end;

procedure TJvDesignSurface.CutComponents;
begin
  CopyComponents;
  DeleteComponents;
end;

procedure TJvDesignSurface.fcce(Reader: TReader; const cn: string;
    var ComponentClass: TComponentClass);
begin
  //find component class event
  ComponentClass:=TComponentClass(GetClass(cn));

end;

procedure TJvDesignSurface.PasteComponents;
var
  CO: TControl=nil;
  C: TComponent=nil;
//  P: TWinControl=nil;
  s: tstringstream=nil;
  ms: TMemoryStream=nil;

 { procedure KeepInParent;
  begin
    with P do
    begin
      if CO.Left > ClientWidth then
        CO.Left := ClientWidth - CO.Width;
      if CO.Top > ClientHeight then
        CO.Top := ClientHeight - CO.Height;
    end;
  end;

  procedure PasteComponent;
  begin
    C.Name := DesignUniqueName(Container, C.ClassName);

    p.InsertComponent(C);
    if C is TControl then
    begin
      CO := TControl(C);
      KeepInParent;
      CO.Parent := P;
      Selector.AddToSelection(CO);
    end;

  end;
         }

var l: TObjectList;
    i: integer;
    newparent: TComponent;

begin
  s:=TStringStream.Create(clipboard.AsText);
  ms:=TMemoryStream.Create;

  newparent:=SelectedContainer;

  try
    LRSObjectTextToBinary(s,ms);
    ms.position:=0;

    l:=tobjectlist.create;
    ClearSelection;
    while ms.position<ms.size do
    begin
      C:=nil;
      try
        ReadComponentFromBinaryStream(ms, C, @fcce, container, newparent, container);
        l.add(c);
      except
        break;
      end;
    end;

  finally
    ms.free;
    s.free;
  end;



  active:=false;
  active:=true;

  for i:=0 to l.count-1 do
    selector.AddToSelection(TControl(l[i]));
  SelectionChange;

end;

procedure TJvDesignSurface.SelectParent;
begin
  if Count > 0 then
    Select(Selection[0].Parent);
end;

{
procedure TJvDesignSurface.PaintContainerBkgnd(ADC: HDC);
var
  r: TRect;
  canvas: TCanvas;
begin
  if DrawGrid then
  begin
    canvas := TCanvas.Create;
    try
      SelectClipRgn(ADC, 0);
      canvas.Handle := ADC;
      canvas.Brush.Color := Container.Brush.Color;
      r := canvas.ClipRect;
      if Assigned(FOnOwnerDrawGrid) then
        FOnOwnerDrawGrid(Self, canvas, Container.ClientRect)
      else begin
        canvas.FillRect(Container.ClientRect);
        DesignPaintRules(canvas, Container.ClientRect);
      end;
    finally
      canvas.Free;
    end;
  end;
end;
}

type
  TAccessWinControl = class(TWinControl);

function TJvDesignSurface.IsDesignMessage(ASender: TControl;
  var AMsg: lmessages.TLMessage; const APt: TPoint): Boolean;

  function VirtKey: Cardinal;
  begin
    Result := AMsg.WParam and $ffff;
  end;

{
  function HandlePaint: Boolean;
  begin
    Result := False;
  end;

  function HandleEraseBkgnd: Boolean;
  begin
    if (ASender <> Container) then
      Result := False
    else begin
       PaintContainerBkgnd(TWMPaint(AMsg).DC);
       AMsg.Result := 1;
       Result := True;
    end;
  end;
}
var
  PosChangedHandle: HWND;
  I: Integer;
  Control: TAccessWinControl;



  p: tpoint;
  c: tcontrol;
begin
  if not Active then
    Result := False
  else
    case AMsg.Msg of
{
      WM_ERASEBKGND:
        Result := HandleEraseBkgnd;
      WM_PAINT:
        Result := HandlePaint;
}
      $bd01:
      //, LM_LBUTTONDBLCLK:
        result:=true;

      LM_MOUSEENTER: result:=true;
      LM_LBUTTONDBLCLK: result:=true;

      LM_RBUTTONDOWN:
      begin
        Result := Controller.MouseDown(mbRight, APt.X, APt.Y, TLMMOUSE(AMsg));

        if result and (FPopupMenu<>nil) then
        begin
          p:=container.ClientToScreen(APt);

          FPopupMenu.PopupComponent:=TComponent(FindControl(Apt.x, apt.y));
          FPopupMenu.PopUp(p.x,p.y);
        end;

      //  result:=true;
      end;
      LM_RBUTTONUP:
      begin
        Result := Controller.MouseUp(mbRight, APt.X, APt.Y, TLMMouse( aMsg));
       // result:=true;
      end;


      LM_LBUTTONDOWN:
      begin
        Result := Controller.MouseDown(mbLeft, APt.X, APt.Y, TLMMOUSE(AMsg));
        result:=true;
      end;
      LM_LBUTTONUP:
      begin
        Result := Controller.MouseUp(mbLeft, APt.X, APt.Y, TLMMouse( aMsg));
        result:=true;
      end;

      LM_MOUSEMOVE:
      begin
        Result := Controller.MouseMove(APt.X, APt.Y, TLMMouse( aMsg));
        result:=true;
      end;

      LM_KEYDOWN{, CN_KEYDOWN}:
      begin
        Result := Controller.KeyDown(VirtKey);
        result:=true;
      end;

      LM_KEYUP:
      begin
        Result := Controller.KeyUp(VirtKey);
        result:=true;
      end;

     {LM_WINDOWPOSCHANGED:
        begin
          if AMsg.lParam <> 0 then
          begin
            //CVPosChangedHandle := PWindowPos(AMsg.lParam).hwnd;
            PosChangedHandle := PWindowPos(AMsg.lParam)^.hwnd;

            // If the window that has changed is a control owned by our container
            // then we must update the designer. This allows to programatically
            // change the location of a control while making the designer handles
            // follow it around (Mantis 4693).
            // For this to work properly, we MUST update the bounds of the
            // control before calling UpdateDesigner because the VCL has not yet
            // processed the WM_WINDOWPOSCHANGED message when this code executes.
            // If we did not, the designer would use the previous position of the
            // control to display the handles.
            // Additionnaly, we must not work with controls that don't have their
            // handle allocated. In some instances, creating the handle may trigger
            // a second WM_WINDOWPOSCHANGED message, thus leading to an infinite
            // loop and a crash (Mantis 5225)
            for I := 0 to Container.ComponentCount - 1 do
            begin
              if Container.Components[I] is TWinControl then
              begin
                Control := TAccessWinControl(Container.Components[I]);
                if Control.HandleAllocated and (PosChangedHandle = Control.Handle) then
                begin
                  if not (csDestroyingHandle in Control.ControlState) then
                   //$IFDEF DELPHI10_UP
                    //CV Control.UpdateBounds;
                    //$ELSE
                    Control.Dispatch(AMsg);
                    //$ENDIF DELPHI10_UP

                  UpdateDesigner;
                end;
              end;
            end;//for
          end;

          // Must return False to let the VCL do its own work of placing the window
          Result := False;
        end;   }

        LM_WINDOWPOSCHANGED,LM_ERASEBKGND: result:=false;
        LM_PAINT: result:=false;
        {LM_RBUTTONDOWN,}LM_MBUTTONDOWN{,LM_RBUTTONUP}: result:=true;

        CN_KEYDOWN,CN_CHAR,CN_SYSKEYUP,CN_SYSKEYDOWN,CN_SYSCHAR : result:=true;

      else
        Result := False;
    end;


end;

function TJvDesignSurface.GetCursor(AX, AY: Integer): TCursor;
begin
  // Using FindControl is inefficient.
  // All we really want to know is if Selected[0] contains (AX, AY)
  if (Count > 0) and (FindControl(AX, AY) = Selected[0]) then
    Result := Selector.GetCursor(AX, AY)
  else
    Result := crDefault;
end;

function TJvDesignSurface.GetHitHandle(AX, AY: Integer): TJvDesignHandleId;
begin
  Result := Selector.GetHitHandle(AX, AY);
end;

procedure TJvDesignSurface.BeginUpdate;
begin
  Active := False;
  FUpdateOwner := Owner;
  Owner.RemoveComponent(Self);
end;

procedure TJvDesignSurface.EndUpdate;
begin
  FUpdateOwner.InsertComponent(Self);
  Active := True;
end;

procedure TJvDesignSurface.ReaderError(Reader: TReader; const Msg: string;
  var Handled: Boolean);
begin
  Handled := True;
end;

function TJvDesignSurface.Clear: TJvDesignSurface;
begin
  BeginUpdate;
  try
    Container.DestroyComponents;
  finally
    EndUpdate;
  end;
  Result := Self;
end;

procedure TJvDesignSurface.SaveToStream(AStream: TStream);
begin
  BeginUpdate;
  try
    DesignSaveComponentToStream(Container, AStream);
  finally
    EndUpdate;
  end;
end;

function TJvDesignSurface.LoadFromStream(AStream: TStream): TJvDesignSurface;
var
  SavedName: string;
begin
  BeginUpdate;
  SavedName := Container.Name;
  try
    Container.DestroyComponents;
    DesignLoadComponentFromStream(Container, AStream, @ReaderError);
    Container.Name := SavedName;
  finally
    Container.Name := SavedName;
    EndUpdate;
  end;
  Result := Self;
end;

procedure TJvDesignSurface.SaveToFile(const AFileName: string);
begin
  BeginUpdate;
  try
    DesignSaveComponentToFile(Container, AFileName);
  finally
    EndUpdate;
  end;
end;

function TJvDesignSurface.LoadFromFile(const AFileName: string): TJvDesignSurface;
var
  SavedName: string;
begin
  BeginUpdate;
  SavedName := Container.Name;
  try
    Container.DestroyComponents;
    DesignLoadComponentFromFile(Container, AFileName, @ReaderError);
  finally
    Container.Name := SavedName;
    EndUpdate;
  end;
  Result := Self;
end;

{
procedure TJvDesignSurface.SetDrawGrid(const Value: Boolean);
begin
  FDrawGrid := Value;
  if Active then
    Container.Invalidate;
end;
}

//=== { TJvDesignScrollBox } =================================================

procedure TJvDesignScrollBox.AutoScrollInView(AControl: TControl);
begin
  //
end;

//=== { TJvDesignPanel } =====================================================

constructor TJvDesignPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDrawRules := True;
  FSurface := TJvDesignSurface.Create(Self);
  Surface.Name := 'Surface';
  Surface.Container := Self;
end;

procedure TJvDesignPanel.SetDrawRules(const Value: Boolean);
begin
  FDrawRules := Value;
  Invalidate;
end;

procedure TJvDesignPanel.Paint;
begin
  inherited Paint;
  if Surface.Active or (csDesigning in ComponentState) then
  begin
    if DrawRules then
      DesignPaintRules(Canvas, ClientRect);
    if Assigned(FOnPaint) then
      FOnPaint(Self);
  end;
end;

procedure TJvDesignPanel.Clear;
begin
  // DesignSurface property value is lost on clear.
  // Restore it with the value returned from Clear.
  FSurface := Surface.Clear;
end;

procedure TJvDesignPanel.SaveToStream(AStream: TStream);
begin
  Surface.SaveToStream(AStream);
end;

procedure TJvDesignPanel.LoadFromStream(AStream: TStream);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromStream.
  FSurface := Surface.LoadFromStream(AStream);
end;

procedure TJvDesignPanel.SaveToFile(const AFileName: string);
begin
  Surface.SaveToFile(AFileName);
end;

procedure TJvDesignPanel.LoadFromFile(const AFileName: string);
begin
  // DesignSurface property value is lost on load.
  // Restore it with the value returned from LoadFromFile.
  FSurface := Surface.LoadFromFile(AFileName);
end;

function TJvDesignPanel.GetActive: Boolean;
begin
  Result := Surface.Active;
end;

function TJvDesignPanel.GetOnChange: TNotifyEvent;
begin
  Result := Surface.OnChange;
end;

function TJvDesignPanel.GetOnGetAddClass: TJvDesignGetAddClassEvent;
begin
  Result := Surface.OnGetAddClass;
end;

function TJvDesignPanel.GetOnSelectionChange: TNotifyEvent;
begin
  Result := Surface.OnSelectionChange;
end;

procedure TJvDesignPanel.SetActive(const Value: Boolean);
begin
  Surface.Active := Value;
end;

procedure TJvDesignPanel.SetOnChange(const Value: TNotifyEvent);
begin
  Surface.OnChange := Value;
end;

procedure TJvDesignPanel.SetOnGetAddClass(const Value: TJvDesignGetAddClassEvent);
begin
  Surface.OnGetAddClass := Value;
end;

procedure TJvDesignPanel.SetOnSelectionChange(const Value: TNotifyEvent);
begin
  Surface.OnSelectionChange := Value;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

