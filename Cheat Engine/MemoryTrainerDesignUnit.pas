unit MemoryTrainerDesignUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,handles, ExtCtrls, StdCtrls,ExtraTrainerComponents,grids, Menus;

type
  TfrmTrainerDesigner = class(TForm)
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    Bringtofront1: TMenuItem;
    Sendtoback1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Bringtofront1Click(Sender: TObject);
    procedure Sendtoback1Click(Sender: TObject);
  private
    { Private declarations }
    drawrect: TRect;
    paint: boolean;
    changed: boolean;
    stretchhandle: TStretchHandle;
    procedure FillBevelInOutList(x: string);
    procedure stretchhandleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    { Public declarations }
    AddItem: integer;
    startx,starty: integer;
    selectedobject: tobject;
    procedure updatecheats;
    procedure updateeditor;
    procedure MouseUp(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject;  Shift: TShiftState; X, Y: Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure stretchhandleDblClick(Sender: TObject);
  end;

var
  frmTrainerDesigner: TfrmTrainerDesigner;

implementation

uses MemoryTrainerDesignObjectViewUnit, MemoryTrainerDesignControlsUnit,
  formMemoryModifier, formMemoryTrainerUnit;

{$R *.dfm}



function cursortostr(x: tcursor):string;
begin
  if x=crhandpoint then result:='Handpoint' else result:='Default';
end;

procedure fillcursor;
begin
  with frmTrainerDesignObjectView do
  begin
    valuelisteditor1.ItemProps['Cursor'].ReadOnly:=true;
    valuelisteditor1.ItemProps['Cursor'].PickList.Add('Handpoint');
    valuelisteditor1.ItemProps['Cursor'].PickList.Add('Default');
  end;
end;

function bevelkindToStr(x:tbevelkind): string;
begin
  case x of
    bkNone: result:='None';
    bkTile: result:='Tile';
    bkSoft: result:='Soft';
    bkFlat: result:='Flat';
  end;
end;

procedure fillboolean(x:string);
begin
  with frmTrainerDesignObjectView do
  begin
    valuelisteditor1.ItemProps[x].ReadOnly:=true;
    valuelisteditor1.ItemProps[x].PickList.Text:='';
    valuelisteditor1.ItemProps[x].PickList.Add(Booltostr(true,true));
    valuelisteditor1.ItemProps[x].PickList.Add(Booltostr(false,true));
  end;
end;

procedure fillclick;
var i: integer;
begin
  with frmTrainerDesignObjectView do
  begin
    valuelisteditor1.ItemProps['On Click'].ReadOnly:=true;
    valuelisteditor1.ItemProps['On Click'].PickList.Text:='';
    valuelisteditor1.ItemProps['On Click'].PickList.Add('Nothing');
    valuelisteditor1.ItemProps['On Click'].PickList.Add('Exit');
    valuelisteditor1.ItemProps['On Click'].PickList.Add('Launch App');
    valuelisteditor1.ItemProps['On Click'].PickList.Add('Show Aboutbox');
    valuelisteditor1.ItemProps['On Click'].picklist.add('Execute command');

    for i:=0 to length(frmmemorymodifier.trainerdata)-1 do
      valuelisteditor1.ItemProps['On Click'].picklist.add('Execute cheat '+inttostr(i));

    valuelisteditor1.ItemProps['On Click'].EditStyle:=esPickList;
  end;
end;


procedure fillBevelKindList(x:string);
begin
  with frmTrainerDesignObjectView do
  begin
    valuelisteditor1.ItemProps[x].PickList.Text:='';
    valuelisteditor1.ItemProps[x].PickList.Add('None');
    valuelisteditor1.ItemProps[x].PickList.Add('Tile');
    valuelisteditor1.ItemProps[x].PickList.Add('Soft');
    valuelisteditor1.ItemProps[x].PickList.Add('Flat');
  end;
end;

function bevelinout(x:TBevelCut):string;
begin
      case x of
        bvNone:   result:='None';
        bvLowered:result:='Lowered';
        bvRaised: result:='Raised';
        bvSpace:  result:='Space';
      end;
end;

procedure tfrmtrainerdesigner.FillBevelInOutList(x: string);
begin
  with frmTrainerDesignObjectView do
  begin
  valuelisteditor1.ItemProps[x].PickList.Text:='';
  valuelisteditor1.ItemProps[x].PickList.Add('None');
  valuelisteditor1.ItemProps[x].PickList.Add('Lowered');
  valuelisteditor1.ItemProps[x].PickList.Add('Raised');
  valuelisteditor1.ItemProps[x].PickList.Add('Space');
  end;
end;

procedure TfrmTrainerDesigner.stretchhandleDblClick(Sender: TObject);
begin
  frmTrainerDesignObjectView.Show;

end;

procedure tfrmtrainerdesigner.updateeditor;
var i: integer;
    a: string;
begin
  //set the info in the valuelisteditor
  if selectedobject<>nil then
  with frmTrainerDesignObjectView do
  begin
    valuelisteditor1.Strings.text:='';  //clear  (valuelist editor is buggy)

    if (selectedobject is tbutton) then
    begin
      valuelisteditor1.InsertRow('Text',tbutton2(selectedobject).Caption,true);
      valuelisteditor1.InsertRow('Word Wrap',booltostr(tbutton2(selectedobject).WordWrap,true),true);;

      //with the tag of the object i define what happens when clicked
      case tbutton2(selectedobject).Tag of
        0: valuelisteditor1.InsertRow('On Click','Nothing',true);
        1: valuelisteditor1.InsertRow('On Click','Exit',true);
        2: valuelisteditor1.InsertRow('On Click','Launch App',true);
        3: valuelisteditor1.InsertRow('On Click','Show Aboutbox',true);
        4: valuelisteditor1.InsertRow('On Click','Execute command',true);
        else
        begin
          if tbutton2(selectedobject).Tag>=5 then //it has an cheat assigned
            valuelisteditor1.InsertRow('On Click','Execute cheat '+inttostr(tbutton2(selectedobject).Tag-5),true);
        end;

      end;
      valuelisteditor1.InsertRow('Command',tbutton2(selectedobject).command,true);

      fillclick;
      fillboolean('Word Wrap');

    end;

    if (selectedobject is tcheatlist) then
    begin
      valuelisteditor1.InsertRow('Activation color',inttohex(tcheatlist(selectedobject).activationcolor,6),true);
      valuelisteditor1.InsertRow('Background color',inttohex(tcheatlist(selectedobject).color,6),true);
      valuelisteditor1.InsertRow('Text color',IntToHex(tcheatlist(selectedobject).TextColor,6),true);
      valuelisteditor1.InsertRow('Hotkey left',Inttostr(tcheatlist(selectedobject).hotkeyleft),true);
      valuelisteditor1.InsertRow('Description left',Inttostr(tcheatlist(selectedobject).descriptionleft),true);
      valuelisteditor1.InsertRow('Edit left',Inttostr(tcheatlist(selectedobject).editleft),true);
      valuelisteditor1.InsertRow('Edit width',Inttostr(tcheatlist(selectedobject).editwidth),true);
      valuelisteditor1.InsertRow('Show hotkeys',booltostr(tcheatlist(selectedobject).ShowHotkeys,true),true);
      valuelisteditor1.InsertRow('Show checkboxes',booltostr(tcheatlist(selectedobject).HasCheckbox,true),true);

      valuelisteditor1.InsertRow('Bevel Inner',bevelinout(tcheatlist(selectedobject).BevelInner),true);
      valuelisteditor1.InsertRow('Bevel Outer',bevelinout(tcheatlist(selectedobject).BevelOuter),true);
      valuelisteditor1.InsertRow('Bevel Width',IntToStr(tcheatlist(selectedobject).BevelWidth),true);
      valuelisteditor1.InsertRow('Bevel Kind',bevelkindTostr(tcheatlist(selectedobject).bevelkind),true);

      FillBevelInOutList('Bevel Inner');
      FillBevelInOutList('Bevel Outer');
      FillBevelKindList('Bevel Kind');

      valuelisteditor1.ItemProps['Bevel Inner'].ReadOnly:=true;
      valuelisteditor1.ItemProps['Bevel Outer'].ReadOnly:=true;
      valuelisteditor1.ItemProps['Bevel Kind'].ReadOnly:=true;

      valuelisteditor1.ItemProps['Activation color'].EditStyle:=esEllipsis;
      valuelisteditor1.ItemProps['Background color'].EditStyle:=esEllipsis;
      valuelisteditor1.ItemProps['Text color'].EditStyle:=esEllipsis;

      fillboolean('Show hotkeys');
      fillboolean('Show checkboxes');
    end;

    if (selectedobject is tcheat) then
    begin
      valuelisteditor1.InsertRow('Cheat',IntToStr(tcheat(selectedobject).cheatnr),true);
      valuelisteditor1.ItemProps['Cheat'].ReadOnly:=true;

      //add the existing cheats to the list
      valuelisteditor1.ItemProps['Cheat'].PickList.Text:='';
      for i:=0 to length(frmmemorymodifier.trainerdata)-1 do
        valuelisteditor1.ItemProps['Cheat'].PickList.Add(inttostr(i));

      valuelisteditor1.ItemProps['Cheat'].EditStyle:=esPickList;

      valuelisteditor1.InsertRow('Text color',inttohex(tcheat(selectedobject).textcolor,6),true);
      valuelisteditor1.InsertRow('Background color',inttohex(tcheat(selectedobject).color,6),true);
      valuelisteditor1.InsertRow('Activation color',inttohex(tcheat(selectedobject).activationcolor,6),true);

      valuelisteditor1.InsertRow('Hotkey left',inttostr(tcheat(selectedobject).hotkeyleft),true);
      valuelisteditor1.InsertRow('Description left',inttostr(tcheat(selectedobject).descriptionleft),true);
      valuelisteditor1.InsertRow('Edit left',inttostr(tcheat(selectedobject).editleft),true);
      valuelisteditor1.InsertRow('Edit width',inttostr(tcheat(selectedobject).editwidth),true);

      valuelisteditor1.InsertRow('Show hotkey',booltostr(tcheat(selectedobject).showhotkey,true),true);
      valuelisteditor1.InsertRow('Show checkbox',booltostr(tcheat(selectedobject).HasCheckbox,true),true);

      valuelisteditor1.ItemProps['Activation color'].EditStyle:=esEllipsis;
      valuelisteditor1.ItemProps['Background color'].EditStyle:=esEllipsis;
      valuelisteditor1.ItemProps['Text color'].EditStyle:=esEllipsis;

      fillboolean('Show hotkey');
      fillboolean('Show checkbox');
    end;

    if (selectedobject is timage2) then
    begin
      valuelisteditor1.InsertRow('Cursor',cursortostr(timage2(selectedobject).cursor),true);
      valuelisteditor1.InsertRow('Picture','...',true);
      valuelisteditor1.InsertRow('Stretch',booltostr(timage2(selectedobject).Stretch,true),true);
      valuelisteditor1.InsertRow('Transparent',booltostr(timage2(selectedobject).Transparent,true),true);

      case timage(selectedobject).Tag of
        0: valuelisteditor1.InsertRow('On Click','Nothing',true);
        1: valuelisteditor1.InsertRow('On Click','Exit',true);
        2: valuelisteditor1.InsertRow('On Click','Launch App',true);
        3: valuelisteditor1.InsertRow('On Click','Show Aboutbox',true);
        4: valuelisteditor1.InsertRow('On Click','Execute command',true);
        else
        begin
          if tbutton2(selectedobject).Tag>=5 then //it has an cheat assigned
            valuelisteditor1.InsertRow('On Click','Execute cheat '+inttostr(tbutton2(selectedobject).Tag-5),true);
        end;
        
      end;
      valuelisteditor1.InsertRow('Command',tImage2(selectedobject).command,true);

      fillclick;
      fillcursor;

      fillboolean('Stretch');
      fillboolean('Transparent');
      valuelisteditor1.ItemProps['Picture'].EditStyle:=esEllipsis;
    end;

    if (selectedobject is tlabel2) then
    begin
      valuelisteditor1.Strings.text:='';
      valuelisteditor1.InsertRow('Text',tlabel2(selectedobject).Caption,true);
      valuelisteditor1.InsertRow('Word Wrap',booltostr(tlabel2(selectedobject).WordWrap,true),true);
      valuelisteditor1.InsertRow('Color',IntToHex(tlabel2(selectedobject).font.Color,6),true);
      valuelisteditor1.ItemProps['Color'].EditStyle:=esEllipsis;
      Valuelisteditor1.InsertRow('Underline',booltostr(tlabel2(selectedobject).Font.Style=[fsUnderline],true),true);;


      valuelisteditor1.InsertRow('Cursor',cursortostr(tlabel2(selectedobject).cursor),true);

      case tlabel2(selectedobject).Tag of
        0: valuelisteditor1.InsertRow('On Click','Nothing',true);
        1: valuelisteditor1.InsertRow('On Click','Exit',true);
        2: valuelisteditor1.InsertRow('On Click','Launch App',true);
        3: valuelisteditor1.InsertRow('On Click','Show Aboutbox',true);
        4: valuelisteditor1.InsertRow('On Click','Execute command',true);
        else
        begin
          if tbutton2(selectedobject).Tag>=5 then //it has an cheat assigned
            valuelisteditor1.InsertRow('On Click','Execute cheat '+inttostr(tbutton2(selectedobject).Tag-5),true);
        end;
        
      end;
      valuelisteditor1.InsertRow('Command',tlabel2(selectedobject).command,true);
      fillclick;
      fillcursor;

      fillboolean('Word Wrap');
      fillBoolean('Underline');
    end;


  end;

end;

procedure tfrmtrainerdesigner.updatecheats;
var i,j:integer;
begin
//this procedure searches all cheats and updates them
//cheatlists alse got updated
//cheats that dont exist anymore will be set to undefined
  for i:=0 to ControlCount-1 do
  begin
    if (Controls[i] is tcheat) then
    begin
      j:=tcheat(controls[i]).cheatnr;
      if j<=length(frmmemorymodifier.trainerdata)-1 then
      begin
        tcheat(controls[i]).Description:=frmmemorymodifier.trainerdata[j].description;
        tcheat(controls[i]).Hotkey:=frmmemorymodifier.trainerdata[j].hotkeytext;
        tcheat(controls[i]).HasEditBox:=frmmemorymodifier.trainerdata[j].hasedit;
        tcheat(controls[i]).Editvalue:=frmmemorymodifier.trainerdata[j].editvalue;

      end
      else
      begin
        tcheat(controls[i]).Description:='undefined '+IntToStr(j);
        tcheat(controls[i]).Hotkey:='undefined '+IntToStr(j);
      end;

      if selectedobject=controls[i] then
      begin
//        frmTrainerDesignObjectView.valuelisteditor1.ItemProps['Cheat'].PickList.Text:='';
  //      for j:=0 to length(frmmemorymodifier.trainerdata)-1 do
    //      frmTrainerDesignObjectView.valuelisteditor1.ItemProps['Cheat'].PickList.Add(inttostr(j));
      end
    end;

    if (controls[i] is tcheatlist) then
    begin
      while tcheatlist(controls[i]).Count>length(frmmemorymodifier.trainerdata) do tcheatlist(controls[i]).deletelast;

      for j:=0 to tcheatlist(controls[i]).Count-1 do
      begin
        tcheatlist(controls[i]).Items[j].Description:=frmmemorymodifier.trainerdata[j].description;
        tcheatlist(controls[i]).Items[j].Hotkey:=frmmemorymodifier.trainerdata[j].hotkeytext;
        tcheatlist(controls[i]).Items[j].HasEditBox:=frmmemorymodifier.trainerdata[j].hasedit;
        tcheatlist(controls[i]).Items[j].Editvalue:=frmmemorymodifier.trainerdata[j].editvalue;
      end;

      //add the cheats
      while tcheatlist(controls[i]).Count<length(frmmemorymodifier.trainerdata) do
        tcheatlist(controls[i]).addcheat(frmmemorymodifier.trainerdata[tcheatlist(controls[i]).Count].hotkeytext,frmmemorymodifier.trainerdata[tcheatlist(controls[i]).Count].description,frmmemorymodifier.trainerdata[tcheatlist(controls[i]).Count].editvalue,frmmemorymodifier.trainerdata[tcheatlist(controls[i]).Count].hasedit);

    end;
  end
end;

procedure TfrmTrainerDesigner.MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  paintbox1.OnMouseup(sender,button,shift,twincontrol(sender).left+x,twincontrol(sender).top+y);
end;

procedure TfrmTrainerDesigner.MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  paintbox1.OnMousemove(sender,shift,twincontrol(sender).left+x,twincontrol(sender).top+y);
end;

procedure TfrmTrainerDesigner.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if stretchhandle.Attached then stretchhandle.Detach;

  if additem=0 then
  begin
    if ((sender is tlabel) or (sender is tedit)) and (twincontrol(sender).Parent is tcheat) then
      sender:=twincontrol(sender).Parent;

    if (sender is tcheat) and (tcheat(sender).Parent is tcheatlist) then
      sender:=(tcheat(sender).Parent);

    stretchhandle.Attach(tcontrol(sender));
    selectedobject:=sender;

    updateEditor;
  end
  else paintbox1.OnMouseDown(sender,button,shift,twincontrol(sender).left+x,twincontrol(sender).top+y);

end;

procedure TfrmTrainerDesigner.stretchhandleKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var i: integer;
    check: integer;
    found: boolean;
begin
  inherited;
  case key of
    vk_right:
      begin
        if ssshift in shift then
        begin
          stretchhandle.Width:=stretchhandle.Width+1;
          stretchhandle.Left:=stretchhandle.Left-1;
        end;
      end;

    vk_left:
      begin
        if ssshift in shift then
        begin
          stretchhandle.Width:=stretchhandle.Width-1;
          stretchhandle.Left:=stretchhandle.Left+1;
        end;
      end;

    vk_up:
      begin
        if ssshift in shift then
        begin
          stretchhandle.Width:=stretchhandle.height-1;
          stretchhandle.Left:=stretchhandle.top+1;
        end;
      end;

    vk_down:
      begin
        if ssshift in shift then
        begin
          stretchhandle.Width:=stretchhandle.height+1;
          stretchhandle.Left:=stretchhandle.top-1;
        end;
      end;


    vk_delete:
      begin
        for i:=0 to stretchhandle.ChildCount-1 do
          stretchhandle.Children[i].Free;
        stretchhandle.Detach;
      end;

    vk_escape:
      begin
        stretchhandle.Detach;
        application.ProcessMessages;
        
        frmtrainerdesigner.HorzScrollBar.Position:=0;
        frmtrainerdesigner.VertScrollBar.Position:=0;
        frmtrainerdesigner.width:=frmtrainerdesigner.width+1;
        frmtrainerdesigner.height:=frmtrainerdesigner.height+1;
        frmtrainerdesigner.width:=frmtrainerdesigner.width-1;
        frmtrainerdesigner.height:=frmtrainerdesigner.height-1;


      end;

    ord('N'):
      if ssctrl in shift then
      begin
        found:=false;
        for i:=0 to frmTrainerDesigner.ControlCount-1 do
        begin
          if frmtrainerdesigner.Controls[i]=selectedobject then
          begin
            check:=i+1;
            check:=check mod frmTrainerDesigner.ControlCount;
            while (not found) and (check<>i) do
            begin
              if (frmtrainerdesigner.Controls[check] is tlabel) or
              (frmtrainerdesigner.Controls[check] is tbutton) or
              (frmtrainerdesigner.Controls[check] is tcheat) or
              (frmtrainerdesigner.Controls[check] is tcheatlist) or
              (frmtrainerdesigner.Controls[check] is timage) then
              begin
                found:=true;
                stretchhandle.Detach;
                stretchhandle.Attach(frmtrainerdesigner.Controls[check]);
                selectedobject:=frmtrainerdesigner.Controls[check];
                updateeditor;
                break;
              end;
              inc(check);
              check:=check mod frmTrainerDesigner.ControlCount;
            end;
            if found then break;
          end;
        end;
      end;
  end;
end;

procedure TfrmTrainerDesigner.FormCreate(Sender: TObject);
begin
  stretchhandle:=tstretchhandle.Create(self);
  stretchhandle.OnKeyDown:=stretchhandleKeyDown;
  stretchhandle.OnDblClick:=stretchhandledblclick;
  stretchhandle.PopupMenu:=popupmenu1;
 // frmTrainerDesigner.KeyPreview:=true;


  frmTrainerDesignObjectView:=tfrmTrainerDesignObjectView.create(self);


  frmTrainerDesignControls:= tfrmTrainerdesignControls.create(self);



end;

procedure TfrmTrainerDesigner.FormShow(Sender: TObject);
begin
  frmTrainerDesignControls.Left:=frmMemorymodifier.Left;
  frmTrainerDesignControls.Width:=frmMemorymodifier.width;
  frmTrainerDesignControls.top:=frmMemorymodifier.top-frmTrainerDesignControls.height;

  frmTrainerDesignObjectView.show;
  frmTrainerDesignControls.show;
  frmTrainerDesignObjectView.valuelisteditor1.Strings.text:='';
  frmTrainerDesignObjectView.valuelisteditor1.Col:=1;
end;

procedure TfrmTrainerDesigner.PaintBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  stretchhandle.Detach;
  selectedobject:=nil;
  frmTrainerDesignObjectView.ValueListEditor1.Strings.Text:='';

  if additem<>0 then
  begin
    startx:=x;
    starty:=y;
    drawrect.Left:=x;
    drawrect.Top:=y;
    drawrect.Right:=x;
    drawrect.Bottom:=y;
    paint:=true;
  end;
end;

procedure TfrmTrainerDesigner.PaintBox1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var temp: integer;

begin
  if paint then
  begin
    frmTrainerDesigner.Repaint;


    if (startx>x) and (starty>y) then //exchange them
      drawrect:=rect(x,y,startx,starty);

    if (startx>x) and (starty<y) then
      drawrect:=rect(x,starty,startx,y);

    if (startx<x) and (starty>y) then
      drawrect:=rect(startx,y,x,starty);

    if (startx<x) and (starty<y) then
      drawrect:=rect(startx,starty,x,y);


    dec(drawrect.Left,frmTrainerDesigner.HorzScrollBar.ScrollPos);
    deC(drawrect.Right,frmTrainerDesigner.HorzScrollBar.ScrollPos);
    dec(drawrect.top,frmTrainerDesigner.VertScrollBar.ScrollPos);
    deC(drawrect.bottom,frmTrainerDesigner.VertScrollBar.ScrollPos);

    frmTrainerDesigner.Canvas.DrawFocusRect(drawrect);
  end;
end;

procedure TfrmTrainerDesigner.PaintBox1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var comp: Tcontrol;
    i:integer;
begin
  //reset trainerdesignercontrols
  for i:=0 to frmTrainerDesignControls.toolbar1.ButtonCount-1 do
    frmTrainerDesignControls.toolbar1.Buttons[i].Down:=false;

  if paint then
  begin
    changed:=true;
    paint:=false;

    case additem of
      1: begin
           comp:=tbutton2.create(self);
           tbutton(comp).OnMouseDown:=mousedown;
           tbutton(comp).OnMouseMove:=mousemove;
           tbutton(comp).OnMouseUp:=mouseup;
         end;

      2: begin
           comp:=tcheatlist.Create(self);
           tcheatlist(comp).OnMouseDown:=mousedown;
           tcheatlist(comp).OnMouseMove:=mousemove;
           tcheatlist(comp).OnMouseUp:=mouseup;

         end;

      3: begin
           comp:=tcheat.Create(self);
           tcheat(comp).OnMouseDown:=mousedown;
           tcheat(comp).OnMouseMove:=mousemove;
           tcheat(comp).OnMouseUp:=mouseup;
         end;

      4: begin //image
           comp:=timage2.Create(self);
           timage(comp).OnMouseDown:=mousedown;
           timage(comp).Width:=100;
           timage(comp).Height:=100;
           timage(comp).OnMouseMove:=mousemove;
           timage(comp).OnMouseUp:=mouseup;

         end;

      5: begin
           comp:=TLabel2.Create(self);
           tlabel(comp).Caption:='Type some text';
           tlabel(comp).OnMouseDown:=mousedown;
           tlabel(comp).OnMouseMove:=mousemove;
           tlabel(comp).OnMouseUp:=mouseup;
         end;

      6: begin
           comp:=TScrollbox.Create(self);
           tscrollbox(comp).OnMouseDown:=mousedown;
           tscrollbox(comp).OnMouseMove:=mousemove;
           tscrollbox(comp).OnMouseUp:=mouseup;
         end;

    end;


    with comp do
    begin
      if drawrect.Right-drawrect.Left<=1 then
      begin
        top:=drawrect.top;
        left:=drawrect.Left;
      end else BoundsRect:=drawrect;

      parent:=frmTrainerDesigner;
    end;

    selectedobject:=comp;
    stretchhandle.Attach(comp);
  end;

  updatecheats;
  updateeditor;

  additem:=0;
end;

procedure TfrmTrainerDesigner.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  stretchhandle.Free;
  frmTrainerDesignObjectView.free;
  frmTrainerDesignControls.free;
  frmMemoryTrainerPreview.visible:=true;
  frmmemorymodifier.Button2.Enabled:=true;
  frmmemorymodifier.Button7.Caption:='Design own userinterface';
  action:=cafree;

end;

procedure TfrmTrainerDesigner.FormDestroy(Sender: TObject);
begin
  frmTrainerDesigner:=nil;
end;

procedure TfrmTrainerDesigner.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  canclose:=(not changed) or (messagedlg('If you close this window you''ll lose your changes! Continue?',mtwarning,[mbyes,mbno],0)=mryes);
end;

procedure TfrmTrainerDesigner.Bringtofront1Click(Sender: TObject);
begin
  twincontrol(selectedobject).BringToFront;
end;

procedure TfrmTrainerDesigner.Sendtoback1Click(Sender: TObject);
begin
  twincontrol(selectedobject).SendToBack;
end;

end.
