unit scrollTreeView;
{Helper class for treeviews that need more control over the horizontal scroll}

{$mode delphi}

interface

uses
  Classes, SysUtils, controls, StdCtrls, ExtCtrls, comctrls, lmessages;

type


  //-----old imp-----------


  THScrollEvent=procedure (sender: TObject; scrolledleft, maxscrolledleft: integer) of object;

  TTreeView = class(TCustomTreeView)
  private
    fOnHScroll: THScrollEvent;
    fOnVScroll: TNotifyEvent;
  public
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure ForceScrollbarChange;



   // procedure resize;
  published
    property ScrolledLeft;
    property onHScroll: THScrollEvent read fOnHScroll write fOnHScroll;
    property onVScroll: TNotifyEvent read fOnVScroll write fOnVScroll;
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property DefaultItemHeight;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExpandSignType;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnCustomCreateItem;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    //property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnSelectionChanged;
    property OnShowHint;
    //property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property Options;
    property Items;
    property TreeLineColor;
    property TreeLinePenStyle;
    property ExpandSignColor;
    property MultiSelect;
    property MultiSelectStyle;
  end;

implementation

procedure TTreeview.ForceScrollbarChange;
begin
  GetMaxScrollLeft;
end;

procedure TTreeview.WMVScroll(var Msg: TLMScroll);
begin
  inherited WMVScroll(msg);

  if assigned(fOnVScroll) then
    fOnVScroll(self);
end;

procedure TTreeview.WMHScroll(var Msg: TLMScroll);
begin
  inherited WMHScroll(msg);

  if assigned(fOnHScroll) then
    fOnHScroll(self, self.ScrolledLeft, self.GetMaxScrollLeft);
end;

end.

