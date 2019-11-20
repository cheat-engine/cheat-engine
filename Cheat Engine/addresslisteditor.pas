unit AddresslistEditor;
{
Editor for the addresslist

an editor at the location of the value field
self:

when the enter key is pressed set the new value on the provided memoryrecord

when the edit box loses focus apply the change and close the editor (OnEditorClose(sender: Editor))
when escape is pressed, do not apply the change and just close (OnEditorClose)
When the edit box is clicked between the create time and create time+GetDoubleClickTime(), return an "OnDoubleclick(sender: Editor)"
When doubleclicked return an OnDoubleClick

When the up or down key is pressed, apply the change, and send an event that the entry is changed (OnEntryChange(sender: editor; direction))
If a dropdown list is implemented, make this an combobox and feed it the list on creation

owner:
when the view is scrolled/collapsed/expanded the owner should call UpdatePosition, or destroy the editor(optionally applying the current value)
}

{$mode delphi}

//{$warn 3057 off}

interface

uses
  windows, Classes, SysUtils, ComCtrls, Controls, StdCtrls,  MemoryRecordUnit,
  Graphics, LCLType;

type
  TAddressListEditor=class(TCustomEdit)
  private
    fOnEditorClose: TNotifyEvent;
    fOnDoubleClick: TNotifyEvent;
    fmemrec: TMemoryrecord;
    edited: boolean;

    starttime: dword;
    canselect: boolean;
  protected
    procedure DoClose;
    procedure DblClick; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure TextChanged; override;
    procedure DoExit; override;

    procedure SetSelStart(Val: integer); override;
    procedure SetSelLength(Val: integer); override;
  public
    procedure UpdatePosition(left: integer);
    constructor create(owner: TTreeView; memrec: TMemoryrecord; left: integer); overload;
    destructor destroy; override;
  published
    property memrec: TMemoryrecord read fmemrec;
    property OnEditorClose: TNotifyEvent read fOnEditorClose write fOnEditorClose;
    property OnDblClick; //: TNotifyEvent read fOnDoubleclick write fOnDoubleClick;
  end;

implementation

uses addresslist;

procedure TAddressListEditor.SetSelStart(Val: integer);
begin
  if canselect then
    inherited; //(val)
end;

procedure TAddressListEditor.SetSelLength(Val: integer);
begin
  if canselect then
    inherited; //(val)
end;

procedure TAddressListEditor.DblClick;
begin
  edited:=false;
  DoClose;
  inherited DblClick;
end;

procedure TAddressListEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//  if gettickcount<starttime+GetDoubleClickTime then
//    DblClick
//  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TAddressListEditor.DoClose;
begin
  visible:=false;
  memrec.endEdit;

  if assigned(fOnEditorClose) then
    fOnEditorClose(self);
end;

procedure TAddressListEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case key of
    VK_ESCAPE:
    begin
      edited:=false;
      DoClose;
    end;

    VK_RETURN:
    begin
      edited:=true;
      DoExit;
    end;

    VK_UP:
    begin
      DoExit;

      //send an VK_UP to the owner
      SendMessage(TTreeView(Owner).Handle, WM_KEYDOWN, VK_UP, 0);

      TAddresslist(TTreeview(owner).Owner).doValueChange;
    end;

    VK_DOWN:
    begin
      DoExit;

      //send an VK_UP to the owner
      SendMessage(TTreeView(Owner).Handle, WM_KEYDOWN, VK_DOWN, 0);
      TAddresslist(TTreeview(owner).Owner).doValueChange;
    end;


    else
      inherited KeyDown(Key, Shift);
  end;


end;

procedure TAddressListEditor.TextChanged;
begin
  edited:=true;
  inherited TextChanged;
end;

procedure TAddressListEditor.DoExit;
begin
  if edited then
  begin
    try
      memrec.Value:=text;
    except
      beep;
    end;

    DoClose;
  end
  else
    inherited DoExit;
end;

procedure TAddressListEditor.UpdatePosition(left: integer);
var dr: Trect;
begin
  dr:=memrec.treenode.DisplayRect(true);
  self.height:=dr.Bottom-dr.top;
  self.top:=dr.top;




  dr:=memrec.treenode.DisplayRect(false);
  self.width:=dr.Right-left;
end;

destructor TAddressListEditor.destroy;
begin
  memrec.endEdit; //jic
  inherited destroy;
end;

constructor TAddressListEditor.create(owner: TTreeView; memrec: TMemoryrecord; left: integer);
var pt: TPoint;
  i: integer;
begin
  fmemrec:=memrec;
  memrec.beginEdit;

  inherited create(owner);

  self.autosize:=false;
  self.BorderStyle:=bsNone;
  self.Left:=left;


  self.color:=clHighlight;
  self.text:=memrec.Value;


  self.font:=owner.Font;
  self.Font.Color:=clred;

  updateposition(left);




  self.parent:=owner;
  SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN, 0);

  self.SetFocus;




  starttime:=GetTickCount;

  if ((GetKeyState(VK_RETURN) shr 15) and 1)=1 then  //if launched with RETURN then select all
  begin
    canselect:=true;
    self.SelectAll;
    canselect:=false;
  end
  else
  begin
    pt:=self.ScreenToClient(mouse.cursorpos);

    pt.x:=owner.Canvas.TextFitInfo(text, pt.x);
    pt.y:=0;

    self.SetCaretPos(pt);
  end;

end;

end.

