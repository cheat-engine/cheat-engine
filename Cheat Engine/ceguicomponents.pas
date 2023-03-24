unit ceguicomponents;

{Modified components so they don't show unsupported properties}

{$warn 3057 off}

{$mode delphi}

interface

uses
  zstream, Classes, SysUtils, Controls, forms,ComCtrls, StdCtrls, ExtCtrls, Buttons, lcltype,
  dialogs, JvDesignSurface, DOM, typinfo, LResources, JvDesignImp, JvDesignUtils,
  graphics, math, xmlread,xmlwrite, WSStdCtrls, custombase85, PropEdits,
  ComponentEditors, CEListviewItemEditor, TreeViewPropEdit, menus, MenuIntf, LCLProc,
  Calendar, CECustomButton, betterControls;

type TCEPageControl=class(TPageControl);
type
  TCETreeview=class(TTreeview)
  public
{$ifdef cpu32}
    destructor destroy; override;
{$endif}
  end;

type TCESplitter=class(TSplitter);
{  property Align;
  property Anchors;
  property AutoSnap;
  property Beveled;
  property Color;
  property Constraints;
  property Cursor;
  property Height;
  property MinSize;
  property OnCanResize;
  property OnChangeBounds;
  property OnMoved;
  property ParentColor;
  property ParentShowHint;
  property PopupMenu;
  property ResizeAnchor;
  property ResizeStyle;
  property ShowHint;
  property Visible;
  property Width;
end;  }

type TCETimer=class(Ttimer);

type TCESaveDialog=class(TSaveDialog);
type TCEOpenDialog=class(TOpendialog);

type TCEListView=class(TListView);
{published
  property Align;
  property AllocBy;
  property Anchors;
  property AutoSort;
  property BorderSpacing;
  property BorderStyle;
  property BorderWidth;
  property Checkboxes;
  property Color;
  property Items;
  property Columns;
  property ColumnClick;
  property Constraints;
 // property DragCursor;
//  property DragKind;
//  property DragMode;

  property Enabled;
  property Font;
  property GridLines;
  property HideSelection;
  property IconOptions;

 // property LargeImages;
  property MultiSelect;
  property OwnerData;
  property ParentColor default False;
  property ParentFont;
  property ParentShowHint;
  property PopupMenu;
  property ReadOnly;
  property RowSelect;
  property ScrollBars;
  property ShowColumnHeaders;
  property ShowHint;
//  property SmallImages;
  property SortColumn;
  property SortType;
  property SortDirection;
//  property StateImages;
  property TabStop;
  property TabOrder;
  property ToolTips;
  property Visible;
  property ViewStyle;
//  property OnAdvancedCustomDraw;
///  property OnAdvancedCustomDrawItem;
 // property OnAdvancedCustomDrawSubItem;
//  property OnChange;
  property OnClick;
  property OnColumnClick;
  property OnCompare;
  property OnContextPopup;
  property OnCustomDraw;
  property OnCustomDrawItem;
  property OnCustomDrawSubItem;
  property OnData;
  property OnDblClick;
 // property OnDeletion;
 // property OnDragDrop;
 // property OnDragOver;
 // property OnEndDock;
//  property OnEndDrag;
  property OnEnter;
  property OnExit;
  property OnItemChecked;
  property OnKeyDown;
  property OnKeyPress;
  property OnKeyUp;
  property OnMouseDown;
  property OnMouseEnter;
  property OnMouseLeave;
  property OnMouseMove;
  property OnMouseUp;
  property OnResize;
  property OnSelectItem;
 // property OnStartDock;
 // property OnStartDrag;
 // property OnUTF8KeyPress;
end;  }

type TCEProgressBar=class(TProgressBar);
{published
  property Align;
  property Anchors;
  property BorderSpacing;
  property BorderWidth;
  property Constraints;
//  property DragCursor;
//  property DragKind;
//  property DragMode;
  property Enabled;
  property Hint;
  property Max;
  property Min;
  property Position;
  property OnContextPopup;
//  property OnDragDrop;
//  property OnDragOver;
//  property OnEndDrag;
  property OnEnter;
  property OnExit;
  property OnMouseDown;
  property OnMouseEnter;
  property OnMouseLeave;
  property OnMouseMove;
  property OnMouseUp;
//  property OnStartDock;
//  property OnStartDrag;
  property Orientation;
  property ParentShowHint;
  property PopupMenu;

  property ShowHint;
  property Smooth;
  property Step;
  property Style;
  property TabOrder;
  property TabStop;
  property Visible;
  property BarShowText;
end;     }

type TCETrackBar=class(TTrackBar);
{  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
 //   property DragCursor;
 //   property DragMode;
    property Enabled;
    property Frequency;
    property Hint;
    property LineSize;
    property Max;
    property Min;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
//    property OnDragDrop;
//    property OnDragOver;
//    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
 //   property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
 //   property OnStartDrag;
 //   property OnUTF8KeyPress;
    property Orientation;
    property PageSize;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property Reversed;
    property ScalePos;
    property SelEnd;
    property SelStart;
    property ShowHint;
    property ShowSelRange;
    property TabOrder;
    property TabStop;
    property TickMarks;
    property TickStyle;
    property Visible;
  end;    }

type TCEListBox=class(TListBox);
{  published
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property ClickOnSelChange;
    property Color;
    property Columns;
    property Constraints;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
    property ExtendedSelect;
    property Enabled;
    property Font;
    property IntegralHeight;
    property Items;
    property ItemHeight;
    property MultiSelect;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
  //  property OnDragDrop;
  //  property OnDragOver;
    property OnDrawItem;
    property OnEnter;
  //  property OnEndDrag;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
 //   property OnMeasureItem;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
 //   property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnSelectionChange;
 //   property OnShowHint;
//    property OnStartDrag;
//    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentShowHint;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end; }

type TCEComboBox=class(TComboBox);
{published
  property Align;
  property Anchors;
  property ArrowKeysTraverseList;
  property AutoComplete;
  property AutoCompleteText;
  property AutoDropDown;
  property AutoSelect;
  property AutoSize;// Note: windows has a fixed height in some styles
  property BidiMode;
  property BorderSpacing;
  property CharCase;
  property Color;
  property Constraints;
//  property DragCursor;
//  property DragKind;
//  property DragMode;
  property DropDownCount;
  property Enabled;
  property Font;
  property ItemHeight;
  property ItemIndex;
  property Items;
  property ItemWidth;
  property MaxLength;
  property OnChange;
  property OnChangeBounds;
  property OnClick;
  property OnCloseUp;
  property OnContextPopup;
  property OnDblClick;
////  property OnDragDrop;
 // property OnDragOver;
  property OnDrawItem;
//  property OnEndDrag;
  property OnDropDown;
  property OnEditingDone;
  property OnEnter;
  property OnExit;
  property OnGetItems;
  property OnKeyDown;
  property OnKeyPress;
  property OnKeyUp;
 // property OnMeasureItem;
  property OnMouseDown;
  property OnMouseEnter;
  property OnMouseLeave;
  property OnMouseMove;
  property OnMouseUp;
 // property OnStartDrag;
  property OnSelect;
 // property OnUTF8KeyPress;
 // property ParentBidiMode;
  property ParentColor;
  property ParentFont;
  property ParentShowHint;
  property PopupMenu;
  property ReadOnly;
  property ShowHint;
  property Sorted;
  property Style;
  property TabOrder;
  property TabStop;
  property Text;
  property Visible;
end; }


type TCEGroupBox=class(TGroupBox);
{published
  property Align;
  property Anchors;
  property AutoSize;
  property BidiMode;
  property BorderSpacing;
  property Caption;
  property ChildSizing;
  property ClientHeight;
  property ClientWidth;
  property Color;
  property Constraints;
  //property DockSite;
 // property DragCursor;
 // property DragKind;
 // property DragMode;
  property Enabled;
  property Font;
  property ParentBidiMode;
  property ParentColor;
  property ParentFont;
  property ParentShowHint;
  property PopupMenu;
  property ShowHint;
  property TabOrder;
  property TabStop;
  property Visible;
  property OnChangeBounds;
  property OnClick;
  property OnContextPopup;
  property OnDblClick;
//  property OnDragDrop;
//  property OnDockDrop;
//  property OnDockOver;
//  property OnDragOver;
//  property OnEndDock;
//  property OnEndDrag;
  property OnEnter;
  property OnExit;
 // property OnGetSiteInfo;
  property OnKeyDown;
  property OnKeyPress;
  property OnKeyUp;
  property OnMouseDown;
  property OnMouseEnter;
  property OnMouseLeave;
  property OnMouseMove;
  property OnMouseUp;
  property OnResize;
//  property OnStartDock;
//  property OnStartDrag;
//  property OnUnDock;
//  property OnUTF8KeyPress;
end;  }

type TCERadioGroup=class(TRadioGroup);
{published
    property Align;
    property Anchors;
    property AutoFill;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property ColumnLayout;
    property Columns;
    property Constraints;
//    property DragCursor;
//    property DragMode;
    property Enabled;
    property Font;
    property ItemIndex;



    property Items;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
  //  property OnDragDrop;
  //  property OnDragOver;
  //  property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
 //   property OnStartDrag;
 //   property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
end;    }

type TCECheckBox=class(TCheckBox);
{  public
    constructor Create(TheOwner: TComponent); override;
  published
    //property Action;
    property Align;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property Color nodefault;
    property Constraints;
  //  property DragCursor;
  //  property DragKind;
  //  property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
   // property OnDragDrop;
   // property OnDragOver;
   // property OnEditingDone;
   // property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
 //   property OnStartDrag;
 //   property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ParentBidiMode;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop default True;
    property Visible;
  end;  }


type TCEToggleBox=class(TToggleBox); //there is no custom...


type TCEEdit=class(TEdit)
  private
    fTextHintFontColor: TColor;
    fTextHintFontStyle: TFontStyle;
  public
    property AutoSelected;
  published
  //  property Action;
  {  property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderStyle;
    property BorderSpacing;
    property CharCase;
    property Color;
    property Constraints;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
 //   property OnDragDrop;
 //   property OnDragOver;
 //   property OnEditingDone;
 //   property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
 //   property OnStartDrag;
  //  property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Text;
    property Visible; }

    property SelStart;
    property SelLength;
    property SelText;

    property TextHint;
    property TextHintFontColor: Tcolor read fTextHintFontColor write fTextHintFontColor;
    property TextHintFontStyle: TFontStyle read fTextHintFontStyle write fTextHintFontStyle;
  end;

type TCEForm=class(TForm) //TCustomForm)
  private
    saving: boolean;
    fVisible: boolean;
    saveddesign: TMemorystream;
    fDoNotSaveInTable: boolean;

    procedure OnWriteMethod(Writer: TWriter; Instance: TPersistent; PropInfo: PPropInfo; const MethodValue, DefMethodValue: TMethod; var Handled: boolean);
    procedure WriteComponentAsBinaryToStreamWithMethods(Astream: TStream);
    procedure setActive(state: boolean);
    function getActive: boolean;

    procedure SetMethodProperty(Reader: TReader; Instance: TPersistent; PropInfo: PPropInfo; const TheMethodName: string; var Handled: boolean);
  protected
    procedure paint; override;
  public
    designsurface: TJvDesignSurface;
    procedure ResyncWithLua(Base: TComponent); overload;
    procedure ResyncWithLua; overload;
    procedure SaveToStream(s: tstream);
    procedure SaveToFile(filename: string);
    procedure SaveToFileLFM(filename: string);
    procedure LoadFromStream(s: TStream);
    procedure LoadFromFile(filename: string);
    procedure LoadFromFileLFM(filename: string);
    procedure SaveToXML(Node: TDOMNode; dontdeactivate:boolean=false);
    procedure LoadFromXML(Node: TDOMNode);
    procedure RestoreToDesignState;
    procedure SaveCurrentStateasDesign;
    function getVisible:boolean;
    procedure setVisible(state: boolean);
    destructor destroy; override;

    property  active: boolean read getActive write setActive;
  published
    property Align;
    property AllowDropFiles;
    property AlphaBlend default False;
    property AlphaBlendValue default 255;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    property BorderIcons;
    property BorderStyle;
    property BorderWidth;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DefaultMonitor;
    property DockSite;
    property DragKind;
    property DragMode;
    //property Enabled;
    property Font;
    property FormStyle;
   // property HelpFile;
    property Icon;
    property KeyPreview;
    property Menu;
    property OnActivate;
    property OnChangeBounds;
    property OnClick;
    property OnClose;
    property OnCloseQuery;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDeactivate;
    property OnDestroy;
   // property OnDockDrop;
  //  property OnDockOver;
   // property OnDragDrop;
  //  property OnDragOver;
    property OnDropFiles;
  //  property OnEndDock;
  //  property OnGetSiteInfo;
 //   property OnHelp;
    property OnHide;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
   // property OnShortCut;
    property OnShow;
   // property OnShowHint;
    //property OnStartDock;
  //  property OnUnDock;
  //  property OnUTF8KeyPress;
    property OnWindowStateChange;
    property ParentBiDiMode;
    property ParentFont;
    property PixelsPerInch;
    property PopupMenu;
    property PopupMode;
  //  property PopupParent;
    property Position;
   // property SessionProperties;
  //  property ShowHint;
    property ShowInTaskBar;
  //  property UseDockManager;
 //   property LCLVersion: string read FLCLVersion write FLCLVersion stored LCLVersionIsStored;
    property Visible read getVisible write setVisible;
    property WindowState;

    property DoNotSaveInTable: boolean read fDoNotSaveInTable write fDoNotSaveInTable default False;
end;

type TCEMemo=class(TMemo)
  published
 {   property Align;
    property Alignment;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
   // property Text;
    property Lines;
    property MaxLength;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
   // property OnDragDrop;
  //  property OnDragOver;
    property OnEditingDone;
   // property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
 //   property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  //  property OnStartDrag;
  //  property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ParentShowHint;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantReturns;
    property WantTabs;
    property WordWrap; }
    property SelStart;
    property SelLength;
    property SelText;
  end;


type TCEImage=class(TImage);
{  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Constraints;
 //   property DragCursor;
//    property DragMode;
    property Enabled;
    //property OnChangeBounds;
    property OnClick;
    property OnDblClick;
   // property OnDragDrop;
   // property OnDragOver;
  //  property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  //  property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnPictureChanged;
    property OnResize;
    //property OnStartDrag;
    property ParentShowHint;
    property Picture;
    property PopupMenu;
    property Proportional;
    property ShowHint;
    property Stretch;
    property Transparent;
    property Visible;
  end; }

type TCEPanel=class(TPanel);
{published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
   // property OnDragDrop;
  //  property OnDragOver;
  //  property OnEndDock;
  //  property OnEndDrag;
    property OnEnter;
    property OnExit;
  //  property OnGetSiteInfo;
  //  property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  //  property OnStartDock;
   // property OnStartDrag;
   // property OnUnDock;
  end; }

type TCELabel=class(TLabel);
{published
  property Align;
  property Alignment;
  property Anchors;
  property AutoSize;
  property BidiMode;
  property BorderSpacing;
  property Caption;
  property Color;
  property Constraints;
//  property DragCursor;
// property DragKind;
 // property DragMode;
  property Enabled;
//  property FocusControl;
  property Font;
  property Layout;
  property ParentBidiMode;
  property ParentColor;
  property ParentFont;
  property ParentShowHint;
  property PopupMenu;
  property ShowAccelChar;
  property ShowHint;
  property Transparent;
  property Visible;  //doesn't work well
  property WordWrap;
  property OnClick;
  property OnDblClick;
  //property OnDragDrop;
  //property OnDragOver;
  //property OnEndDrag;
  property OnMouseDown;
  property OnMouseMove;
  property OnMouseUp;
  property OnMouseEnter;
  property OnMouseLeave;
  property OnChangeBounds;
  property OnContextPopup;
  property OnResize;
  property OnStartDrag;
  property OptimalFill;
end;   }

type TCEButton=class(TButton);
 { private
  published
   //
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Cancel;
    property Caption;
   // property Color;
    property Constraints;
    property Default;
 //   property DragCursor;
 //   property DragKind;
 //   property DragMode;
    property Enabled;
    property Font;
    //property ParentBidiMode;
    property ModalResult;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    //property OnDragDrop;
    //property OnDragOver;
    //property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    //property OnStartDrag;
   // property OnUTF8KeyPress;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
end;  }


implementation

uses luahandler,luacaller, formdesignerunit, CheckLst, colorbox;

resourcestring
  rsInvalidFormData = 'Invalid formdata';

{$ifdef cpu32}
//In this implementation the data field of treenodes contain a pointer to an 8 byte storage  (I don't think I could just change/add to the Data field of the TTreenode components)
destructor TCETreeview.destroy;
var i: integer;
begin
  for i:=0 to items.count-1 do
  begin
    if items[i].data<>nil then
      freemem(items[i].data);
  end;

  inherited destroy;
end;
{$endif}
        {
constructor TCECheckBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csCheckbox;
  TabStop := True;
  AutoSize := True;
end;  }

//ceform

procedure ModifiedCheck(AComponent: TComponent);
var f: TCEForm;
begin
  if AComponent is TControl then
  begin
    f:=TCEForm(GetParentForm(TControl(AComponent)));
    if (f is TCEForm) and (f.active) then
      f.designsurface.Change;

  end;

end;

procedure TCEForm.setActive(state: boolean);
var oldstate: boolean;
begin
  if formdesigner=nil then exit;

  if state then
  begin

    if active=false then
      RestoreToDesignState; //it was disabled so change it to the saved state if possible and edit from there

    OwnerFormDesignerModifiedProc:=ModifiedCheck;



    //check if it is currenty being designed, if not, create a surface for this form
    if designsurface=nil then
    begin
      //still here so the surface needs to be created for this form
      designsurface:=TJvDesignSurface.Create(nil);

      designsurface.Container:=self;
      designsurface.ControllerClass:=TJvDesignController;;

      designsurface.MessengerClass:= TJvDesignWinControlHookMessenger;
      //designsurface.MessengerClass:=TJvDesignDesignerMessenger;
      designsurface.SelectorClass:=TJvDesignSelector;

      designsurface.OnGetAddClass:=formdesigner.DesignerGetAddClass;
      designsurface.OnSelectionChange:=formdesigner.DesignerSelectionChange;
      designsurface.OnChange:=formdesigner.surfaceOnChange;




      designsurface.name:='Surface';

    end;

    designsurface.Active:=true;
  end
  else
  begin

    if designsurface<>nil then
    begin
      if designsurface.active then
        SaveCurrentStateasDesign; //save the current state as the designed state

      designsurface.active:=false;
    end;
  end;


end;

function TCEForm.getActive: boolean;
begin
  result:=false;
  if designsurface<>nil then
    result:=designsurface.active;
end;

procedure TCEForm.OnWriteMethod(Writer: TWriter; Instance: TPersistent; PropInfo: PPropInfo; const MethodValue, DefMethodValue: TMethod; var Handled: boolean);
begin
  if (MethodValue.data<>nil) and (tobject(MethodValue.data) is TLuaCaller)  then
  begin
    writer.Driver.BeginProperty(propinfo.Name);
    writer.Driver.WriteMethodName(TLuaCaller(MethodValue.data).luaroutine);
    writer.Driver.EndProperty;
  end;
  handled:=true;
end;

procedure TCEForm.WriteComponentAsBinaryToStreamWithMethods(AStream: TStream);
var
  Writer: TWriter;
  DestroyDriver: Boolean;
  g: tguid;
  s: string;
  i: integer;
begin
  if name='' then
  begin
    //an object NEEDS a name
    CreateGUID(g);

    s:=GUIDToString(g);
    for i:=1 to length(s) do
      if s[i] in ['{','}','-'] then
        s[i]:='_';

    name:='NoName_'+s;
  end;

  DestroyDriver:=false;
  Writer:=nil;
  try
    Writer:=CreateLRSWriter(AStream,DestroyDriver);
    Writer.OnWriteMethodProperty:=OnWriteMethod;
    Writer.WriteDescendent(self,nil);
  finally
    if DestroyDriver then
      Writer.Driver.Free;
    Writer.Free;
  end;
end;

procedure TCEForm.SetMethodProperty(Reader: TReader; Instance: TPersistent; PropInfo: PPropInfo; const TheMethodName: string; var Handled: boolean);
var t: TLuaCaller;
  m: TMethod;
begin
  t:=TLuaCaller.create;
  t.luaroutine:=TheMethodName;
  t.owner:=Instance;

  m:=luacaller_getFunctionHeaderAndMethodForType(propinfo.PropType, t, '',nil);
  SetMethodProp(instance, propinfo, m);

  handled:=true;
end;



procedure TCEForm.RestoreToDesignState;
var wasactive: boolean;
  reader: TReader;
  DestroyDriver: boolean;

  i: integer;
begin
  wasactive:=active;
  active:=false;

  if designsurface<>nil then
    freeandnil(designsurface);

  //RegisterPropertyToSkip(TCEForm, 'Visible', '','');

  if savedDesign<>nil then
  begin
    name:='';
    while ComponentCount>0 do
      Components[0].Free;

    while ControlCount>0 do
      Controls[0].Free;

    savedDesign.position:=0;

    DestroyDriver:=false;
    reader:=CreateLRSReader(savedDesign,DestroyDriver);

    reader.OnSetMethodProperty:=SetMethodProperty;

    reader.ReadRootComponent(self);

    if DestroyDriver then
      reader.Driver.free;

  end;

  active:=wasactive;
  ResyncWithLua;
end;

procedure TCEForm.SaveCurrentStateasDesign;
//var ss: tstringstream;
begin
  if saveddesign=nil then
    savedDesign:=Tmemorystream.create;

  savedDesign.size:=0;
  saving:=true;
  try
    WriteComponentAsBinaryToStreamWithMethods(savedDesign);
  finally
    saving:=false;
  end;


  savedDesign.position:=0;

  ResyncWithLua;
end;

procedure TCEForm.SaveToXML(Node: TDOMNode; dontdeactivate: boolean=false);
var doc: TXMLDocument;
  outputastext: pchar;
  g: TGuid;
  wasactive: boolean;

  m: TMemorystream;
  c: Tcompressionstream;

  size: dword;

  a: TDOMAttr;
  formnode: TDOMNode;
begin
  wasactive:=active;

  if dontdeactivate then
  begin
    SaveCurrentStateasDesign;
  end
  else
  begin
    if active then active:=false;
  end;

  if saveddesign=nil then
    SaveCurrentStateasDesign;

  if saveddesign=nil then
    exit; //give up

  //for now use a binarystream instead of xml. the xmlwriter/reader does not support stringlists
  //create a stream for storage
  outputastext:=nil;
  try

{
    WriteComponentAsBinaryToStreamWithMethods(m);}

    //compress the design
    m:=tmemorystream.create;
    c:=Tcompressionstream.create(clmax, m, true);
    size:=saveddesign.size;
    c.write(size, sizeof(size));
    c.write(saveddesign.Memory^, size);
    c.free;

    //and now save the stream as text to the xml file
    doc:=TXMLDocument(node.OwnerDocument);


    getmem(outputastext, (m.size div 4) * 5 + 5 );
    BinToBase85(pchar(m.Memory), outputastext, m.Size);


    m.free;


    formnode:=Node.AppendChild(doc.CreateElement(name));
    formnode.TextContent:=outputastext;


    a:=doc.CreateAttribute('Encoding');
    a.TextContent:='Ascii85';
    formnode.Attributes.SetNamedItem(a);

    a:=doc.CreateAttribute('Class');
    a.TextContent:=ClassName;
    formnode.Attributes.SetNamedItem(a);



  finally
    if outputastext<>nil then
      FreeMemAndNil(outputastext);
  end;

  //if dontdeactivate=false then
  //  active:=wasactive;
end;

procedure TCEForm.LoadFromXML(Node: TDOMNode);
var s: string;
  b: pchar;
  m: TMemorystream;
  dc: Tdecompressionstream;
  size: integer;
  read: integer;

  realsize: dword;
  wasActive: boolean;

  useascii85: boolean;
  a: TDOMNode;
begin
  wasActive:=active;
  active:=false;


  if saveddesign=nil then
    saveddesign:=TMemorystream.create;

  saveddesign.Clear;

  s:=node.TextContent;

  //check the "Encoding" attribute of this node
  //If it's "Ascii85" then use ascii85, else use hextobin
  useascii85:=false;

  if node.HasAttributes then
  begin
    a:=node.Attributes.GetNamedItem('Encoding');
    useascii85:=(a<>nil) and (a.TextContent='Ascii85');
  end;


  b:=nil;
  try
    if useascii85 then
    begin
      size:=(length(s) div 5)*4+(length(s) mod 5);
      getmem(b, size);
      size:=Base85ToBin(pchar(s), b);
    end
    else
    begin
      size:=length(s) div 2;
      getmem(b, size);
      HexToBin(pchar(s), b, size);
    end;

    //b now contains the data

    m:=tmemorystream.create;
    m.WriteBuffer(b^, size);
    m.position:=0;
    dc:=Tdecompressionstream.create(m, true);

    dc.read(realsize,sizeof(realsize));

    FreeMemAndNil(b);
    getmem(b, realsize);

    read:=dc.read(b^, realsize);
    saveddesign.WriteBuffer(b^, read);
  finally
    if b<>nil then
      FreeMemAndNil(b);
  end;



  RestoreToDesignState;

  active:=wasActive;
end;


procedure TCEForm.SaveToStream(s: tstream);
var
  xmldoc: TXMLDocument;
  formnode: TDOMNode;
begin
  xmldoc:=TXMLDocument.Create;

  formnode:=xmldoc.appendchild(xmldoc.createElement('FormData'));

  SaveCurrentStateasDesign;
  SaveToXML(formnode,true);

  WriteXML(xmldoc, s);
end;


procedure TCEForm.SaveToFile(filename: string);
var
  fs: TFilestream;
begin
  fs:=TFileStream.Create(filename, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.free;
  end;
end;

procedure TCEForm.SaveToFileLFM(filename: string);
var
  ms: Tmemorystream;
begin
  SaveCurrentStateasDesign;
  ms:=TMemoryStream.Create;
  LRSObjectBinaryToText(savedDesign, ms);
  ms.SaveToFile(filename);
  ms.Destroy;
end;

procedure TCEForm.LoadFromStream(s: TStream);
var
  formnode: TDOMNode;
  xmldoc: TXMLDocument;
begin
  xmldoc:=nil;
  ReadXMLFile(xmldoc, s);

  if xmldoc<>nil then
  begin
    formnode:=xmldoc.FindNode('FormData');

    if formnode.ChildNodes.Count>0 then
    begin
      LoadFromXML(formnode.ChildNodes.item[0]);
      ResyncWithLua;
    end
    else
      raise exception.create(rsInvalidFormData);
  end;

  if ShouldAppsUseDarkMode() then
    if color=clDefault then
    begin
      color:=clWindow;
      font.color:=clWindowtext;
    end;
end;

procedure TCEForm.LoadFromFile(filename: string);
var
  fs: TFileStream;
begin
  fs:=TFileStream.Create(filename, fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    fs.free;
  end;
end;

procedure TCEForm.LoadFromFileLFM(filename: string);
var
  ms: Tmemorystream;
  wasActive: boolean;
begin
  wasActive:=active;
  active:=false;

  ms:=TMemoryStream.Create;
  ms.LoadFromFile(filename);
  saveddesign.Size:=0;
  LRSObjectTextToBinary(ms,saveddesign);
  ms.Destroy;

  active:=wasActive;

  if ShouldAppsUseDarkMode() then
    if color=clDefault then
    begin
      color:=clWindow;
      font.color:=clWindowtext;
    end;
end;

procedure TCEForm.paint;
begin
  inherited paint;

  if active then
  begin
    if color<>clDefault then
      DesignPaintGrid(Canvas, ClientRect, ColorToRGB(color), InvertColor(ColorToRGB(color)), scalex(8,96))
    else
      DesignPaintGrid(Canvas, ClientRect, clBtnFace,clWindowtext, scalex(8,96));
  end;
end;

procedure TCEForm.ResyncWithLua(base: TComponent);
var i: integer;
begin
  for i:=0 to base.ComponentCount-1 do
    ResyncWithLua(base.Components[i]);

  if base=self then
    Lua_RegisterObject(base.Name, base)
   else
    Lua_RegisterObject(name+'_'+base.Name, base)


end;

procedure TCEForm.ResyncWithLua;
begin
  ResyncWithLua(self); //still needed for backwards compatibility
 // Lua_RegisterObject(self.name, self)
end;

function TCEForm.getVisible:boolean;
begin
  if active or saving then
    result:=fVisible
  else
    result:=Inherited visible;

end;

procedure TCEForm.setVisible(state: boolean);
begin
  fVisible:=state;
  if active=false then
  begin
    Inherited visible:=state;
  end;
end;

destructor TCEForm.destroy;
var i: integer;
begin
  self.Menu:=nil;

  if designsurface<>nil then
  begin
    designsurface.active:=false;
    freeandnil(designsurface);
  end;

  self.BeginFormUpdate;
  i:=0;
  while i<ComponentCount do
    components[i].Free;

  i:=0;
  while i<ControlCount do
    Controls[i].Free;






  self.EndFormUpdate;

  inherited destroy;
end;


initialization
  RegisterClass(TCEButton);
  RegisterClass(TCELabel);
  RegisterClass(TCEPanel);
  RegisterClass(TCEImage);
  RegisterClass(TCEMemo);
  RegisterClass(TCEEdit);
  {$ifdef windows} //some components are not implemented in other os's yet
  RegisterClass(TCEToggleBox);
  {$endif}

  RegisterClass(TCEComboBox);
  RegisterClass(TCEListBox);

  RegisterClass(TCECheckBox);
  RegisterClass(TCEGroupBox);
  RegisterClass(TCERadioGroup);
  RegisterClass(TCETimer);
  RegisterClass(TCESaveDialog);
  RegisterClass(TCEOpenDialog);
  RegisterClass(TCEProgressBar);
  RegisterClass(TCETrackbar);
  RegisterClass(TCEListView);
  RegisterClass(TCESplitter);
  RegisterClass(TPaintBox);
  RegisterClass(TCETreeview);

  RegisterClass(TCEPageControl);
  RegisterClass(TTabSheet);
  RegisterClass(TMainMenu);
  RegisterClass(TPopupMenu);
  RegisterClass(TMenuItem);
  RegisterClass(TCalendar);
  RegisterClass(TFindDialog);
  RegisterClass(TSelectDirectoryDialog);
  RegisterClass(TScrollBox);

  RegisterClass(TRadioButton);

  RegisterClass(tceform);


  //some support for those that use lazarus. This way ce can load these components  {
  RegisterClass(TButton);
  RegisterClass(TLabel);
  RegisterClass(TPanel);
  RegisterClass(TImage);
  RegisterClass(TMemo);
  RegisterClass(TEdit);
  RegisterClass(TToggleBox);

  RegisterClass(TComboBox);
  RegisterClass(TListBox);



  RegisterClass(TCheckBox);
  RegisterClass(TGroupBox);
  RegisterClass(TRadioGroup);
  RegisterClass(TTimer);
  RegisterClass(TSaveDialog);
  RegisterClass(TOpenDialog);
  RegisterClass(TProgressBar);
  RegisterClass(TTrackbar);
  RegisterClass(TListView);
  RegisterClass(TSplitter);
  RegisterClass(TTreeview);

  RegisterClass(TPageControl);
  RegisterClass(TTrayIcon);
  registerclass(TStatusBar);
  registerclass(TCheckListBox);
  registerclass(TColorDialog);
  registerclass(TColorBox);

  registerclass(TFontDialog);
  registerclass(TBitBtn);
  registerclass(TSpeedButton);
  registerclass(TStaticText);
  registerclass(TShape);
  registerclass(TBevel);
  registerclass(TNotebook); //eww...
  registerclass(TLabeledEdit);
  registerclass(TControlBar);
  registerclass(TFlowPanel);
  registerclass(TApplicationProperties); //might be usefull...
  registerclass(TColorListBox);
  registerclass(TLazVirtualStringTree);



  RegisterPropertyEditor(ClassTypeInfo(TListItems), TCEListView, 'Items', TCEListViewItemsPropertyEditor);

  //Hide some properties (mainly for the newer objects that have been introduced since 6.4+)
  //example:   RegisterPropertyEditor(TypeInfo(TNotifyEvent), TCEButton, 'OnClick', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStartDragEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStartDockEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TUnDockEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDockDropEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDockOverEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragDropEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDragOverEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TEndDragEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVExpandedEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVAdvancedCustomDrawEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVAdvancedCustomDrawItemEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVChangedEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVChangingEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVCompareEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVCreateNodeClassEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVCustomCreateNodeEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVEditedEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVEditingEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTVEditingEndEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TControlShowHintEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TUTF8KeyPressEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabChangingEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TTabGetImageEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TGetSiteInfoEvent), nil, '', THiddenPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TGetDockCaptionEvent), nil, '', THiddenPropertyEditor);


end.

