{
Copyright © Norstedts Juridik AB
Made by Per-Erik Andersson, inspired by J Hamblin - Qtools Software.
Author grants unrestricted use of this software code.
All use is on your own risk.

J Hamblin has made a component TjhALTBugFix to solve a problem in Vista.
It can be downloaded from CodeGear Quality centre here:
http://qc.codegear.com/wc/qcmain.aspx?d=37403
Below is the text J Hamblin wrote that describes the problem:

** Quote **************
There seems to be a problem with THEMES support in Delphi, in which
TButton, TCheckBox, TRadioButton and TStaticText standard controls
vanish in VISTA when the ALT key is pressed. (only TStaticText vanishes in XP).
If the OS is set to default, pressing the ALT key in XP and Vista has the
behavior of displaying the underline under the accelerator keys.

The mentioned controls vanish the first time ALT is pressed. They can be
restored by repainting the control in code. Once restored, they are not
affected by subsequent ALT key presses -- unless a pagecontrol on the form
changes to a new tabsheet, then all affected controls, both on the tabsheet
and on the form, will vanish on next ALT press. Due to the pagecontrol issue
there is no way to set a flag to do the repaint op only once. In MDI applications,
an ALT key press has the same affect on all child forms at the same time.
** End quote **************

The TjhALTBugFix needs to be put on each form in the application which
is a problem in many large applications. Therefore I made this component
that can be dropped on the main form and then handles all delphi forms
that are created.

The component works like this: In Idle it goes through the list of existing
Delphi forms in TScreen. When a new form is found, its WindowProc is replaced
with a hook that listens for the event WM_UPDATEUISTATE which is the
message triggering the error.
When a form has got an WM_UPDATEUISTATE it gets a flag the says it needs to
be redrawn. The next time the application enters Idle a repaint is made,
depending on the property RepaintAll. If it is true all TWinControls on the
form gets a repaint. If its false only the component that probably needs a
repaint is repainted (that code mady by J Hamblin).
The "repaint all" is an precausion for third part components that might behave in
the same way. RepaintAll is default true.
Note that this component is only active in Vista. If you want it to
handle the TStaticText in XP you have to remove the VistaWithTheme check
in TVistaAltFix.Create.

Usage:
If you want to use this as an component you have to install it into the Delphi IDE.
If you don't want to do that just add this code in your main form OnCreate:

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TVistaAltFix.Create(Self);
end;

}

unit VistaAltFixUnit;

interface
uses
  ExtCtrls, Classes, Contnrs, AppEvnts;

type
  TVistaAltFix = class(TComponent)
  private
    FList: TObjectList;
    FApplicationEvents: TApplicationEvents;
    FRepaintAll: Boolean;
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    function VistaWithTheme: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RepaintAll: Boolean read FRepaintAll write FRepaintAll default True;
  end;

procedure Register;

implementation
uses
  Forms, Windows, Messages, Buttons, ComCtrls, Controls, StdCtrls, Themes;

type
  TFormObj = class(TObject)
  private
    procedure WndProc(var Message: TMessage);
  public
    Form: TForm;
    OrgProc: TWndMethod;
    Used: Boolean;
    NeedRepaint: Boolean;
    RepaintAll: Boolean;
    constructor Create(aForm: TForm; aRepaintAll: Boolean);
    procedure DoRepaint;
  end;

procedure Register;
begin
  RegisterComponents('MEP', [TVistaAltFix]);
end;

{ TVistaAltFix }

procedure TVistaAltFix.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
var
  I: Integer;
  J: Integer;
  TestForm: TForm;
begin
  // Initialize
  for I := 0 to FList.Count - 1 do
    TFormObj(FList[i]).Used := False;

  // Check for new forms
  for I := 0 to Screen.FormCount - 1 do
  begin
    TestForm := Screen.Forms[i];
    for J := 0 to FList.Count - 1 do
    begin
      if TFormObj(FList[J]).Form = TestForm then
      begin
        TFormObj(FList[J]).Used := True;
        TestForm := nil;
        Break;
      end;
    end;
    if Assigned(TestForm) then
      FList.Add(TFormObj.Create(TestForm, RepaintAll));
  end;

  // Remove destroyed forms, repaint others if needed.
  for I := FList.Count - 1 downto 0 do
  begin
    if not TFormObj(FList[i]).Used then
      FList.Delete(i)
    else
      TFormObj(FList[i]).DoRepaint;
  end;
end;

constructor TVistaAltFix.Create(AOwner: TComponent);
begin
  inherited;
  FRepaintAll := True;
  if VistaWithTheme and not (csDesigning in ComponentState) then
  begin
    FList := TObjectList.Create;
    FApplicationEvents := TApplicationEvents.Create(nil);
    FApplicationEvents.OnIdle := ApplicationEventsIdle;
  end;
end;

destructor TVistaAltFix.Destroy;
begin
  FApplicationEvents.Free;
  FList.Free;
  inherited;
end;

function TVistaAltFix.VistaWithTheme: Boolean;
var
  OSVersionInfo: TOSVersionInfo;
begin
  OSVersionInfo.dwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) and
     (OSVersionInfo.dwMajorVersion >= 6) and
     ThemeServices.ThemesEnabled then
    Result := True
  else
    Result := False;
end;

{ TFormObj }

constructor TFormObj.Create(aForm: TForm; aRepaintAll: Boolean);
begin
  inherited Create;
  Form := aForm;
  RepaintAll := aRepaintAll;
  Used := True;
  OrgProc := Form.WindowProc;
  Form.WindowProc := WndProc;
end;

procedure TFormObj.DoRepaint;
  procedure RepaintBtnControls(TheCtrl: TControl);
  // This method made by J Hamblin - Qtools Software.
  var
    i: integer;
  begin
    if not (TheCtrl is TWinControl) or (TheCtrl is TBitBtn) then
      exit;

    // repaint only controls of affected type
    if (TheCtrl is TButtonControl) or (TheCtrl is TStaticText) then
    begin
      TWinControl(TheCtrl).Repaint;
      exit; // TButtonControls, TStaticText do not contain controls so skip rest
    end;

    //

    for i := 0 to TWinControl(TheCtrl).ControlCount - 1 do
    begin
      // only paint controls on active tabsheet of page control
      if (TheCtrl is TTabSheet) and
          (TTabSheet(TheCtrl).PageIndex <> TTabSheet(TheCtrl).PageControl.ActivePageIndex) then
        continue;
      // recurse
      RepaintBtnControls(TWinControl(TheCtrl).Controls[i]);
    end;
  end;

  procedure DoRepaint(Ctrl: TControl);
  var
    i: integer;
  begin
    if (Ctrl is TWinControl) then
    begin
      TWinControl(Ctrl).Repaint;
      for i := 0 to TWinControl(Ctrl).ControlCount - 1 do
        DoRepaint(TWinControl(Ctrl).Controls[i]);
    end;
  end;

begin
  if NeedRepaint then
  begin
    NeedRepaint := False;
    if RepaintAll then
      DoRepaint(Form)
    else
      RepaintBtnControls(Form);
  end;
end;

procedure TFormObj.WndProc(var Message: TMessage);
begin
  OrgProc(Message);
  if (Message.Msg = WM_UPDATEUISTATE) then
    NeedRepaint := True;
end;

end.

