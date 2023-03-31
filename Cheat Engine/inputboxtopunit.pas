unit inputboxtopunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, ExtCtrls{$ifdef darwin},macport{$endif}, betterControls;

type

  { TInputboxTop }

  TInputboxTop = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    selecttext: boolean;
    combobox: tcombobox;
  public
    { Public declarations }
  end;

function InputBoxTop(const ACaption, APrompt, ADefault: string): string; overload;
function InputBoxTop(const ACaption, APrompt, ADefault: string; Aselecttext: boolean): string; overload;
function InputBoxTop(const ACaption, APrompt, ADefault: string; Aselecttext: boolean; var canceled: boolean): string; overload;
function InputBoxTop(const ACaption, APrompt, ADefault: string; Aselecttext: boolean; var canceled: boolean; history: tstrings): string; overload;

implementation


{$ifndef tester}
uses MemoryBrowserFormUnit;
{$else}
uses unit1;
{$endif}


  
function InputBoxTop(const ACaption, APrompt, ADefault: string): string;
var canceled: boolean;
begin
  result:=InputBoxTop(ACaption,APrompt,Adefault,false,canceled);
end;

function InputBoxTop(const ACaption, APrompt, ADefault: string; Aselecttext: boolean): string;
var canceled: boolean;
begin
  result:=InputBoxTop(ACaption,APrompt,Adefault,false,canceled);
end;

function InputBoxTop(const ACaption, APrompt, ADefault: string; Aselecttext: boolean; var canceled: boolean): string; overload;
var emptylist: tstringlist;
begin
  emptylist:=tstringlist.create;
  result:=InputBoxTop(ACaption,APrompt,Adefault,false,canceled, emptylist);
  emptylist.free;
end;

function InputBoxTop(const ACaption, APrompt, ADefault: string; Aselecttext: boolean; var canceled: boolean; history: tstrings): string;
var
    inputbox: TInputboxtop;
begin


  inputbox:=TInputboxtop.Create(Memorybrowser);
  with inputbox do
  begin
   // button1.setfocus;

    combobox:=nil;
    try
      canceled:=true;
      caption:=ACaption;
      label1.Caption:=APrompt;
      edit1.Text:=ADefault;
      selecttext:=Aselecttext;

      if history.Count>0 then
      begin
        //fill combobox
        combobox:=tcombobox.Create(inputbox);

        combobox.AnchorSideTop:=edit1.AnchorSideTop;
        combobox.AnchorSideLeft:=edit1.AnchorSideLeft;
        combobox.AnchorSideRight:=edit1.AnchorSideRight;
        combobox.Anchors:=edit1.Anchors;

        combobox.Visible:=true;
        {combobox.Left:=edit1.left;
        combobox.Top:=edit1.Top;   }
        combobox.Width:=edit1.Width;
        combobox.style:=csDropDown;
        combobox.Parent:=inputbox;
        combobox.Items.AddStrings(history);
        combobox.Text:=ADefault;
        edit1.Visible:=false;

        panel1.AnchorSideTop.Control:=combobox;
        combobox.BorderSpacing.Left:=4;
        combobox.BorderSpacing.Right:=4;
      end;

      if showmodal=mrok then
      begin
        if history.count>0 then
          result:=combobox.Text
        else
          result:=edit1.Text;

        //Check if it is already in the history
        if history.IndexOfName(result)=-1 then
        begin
          //if not, add it
          history.Insert(0,result);

          //if the list got bigger than 25 delete the last one
          while history.Count>25 do //should usually only do 1 time, but a while in case another routine somehow messes up the history 
            history.Delete(25);
        end;

        canceled:=false;
      end
      else
        result:=ADefault;


    finally
      if combobox<>nil then
        combobox.Free;
      free;
    end;
  end;
end;

procedure TInputboxTop.FormShow(Sender: TObject);
var t: string;
    i: integer;
begin
  if combobox<>nil then
    combobox.SetFocus
  else
    edit1.SetFocus;

  if selecttext then
  begin
    edit1.SelStart:=0;
    edit1.SelLength:=length(edit1.Text);
    if combobox<>nil then
    begin
      combobox.SelStart:=0;
      combobox.SelLength:=length(combobox.Text);
    end;
  end else
  begin
    edit1.SelLength:=0;
    t:=edit1.text;
    edit1.SelStart:=length(t);

    if combobox<>nil then
    begin
      combobox.SelLength:=0;
      t:=edit1.text;
      combobox.SelStart:=length(t);
      combobox.SelLength:=length(combobox.Text)-1;
    end;
  end;

  Constraints.MinWidth:=panel1.Width+8;

end;

procedure TInputboxTop.Label1Click(Sender: TObject);
begin
  if combobox<>nil then
    combobox.SelectAll;
end;

procedure TInputboxTop.Timer1Timer(Sender: TObject);
begin
  FormShow(self);
  timer1.enabled:=false;
end;


initialization
  {$i inputboxtopunit.lrs}

end.

