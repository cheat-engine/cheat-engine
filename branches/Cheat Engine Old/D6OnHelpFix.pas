{*******************************************************}
{                                                       }
{  D6OnHelpFix.pas                                      }
{  Description: Fixes OnHelp events in Delphi6+         }
{    Requires Delphi6/7/...                             }
{                                                       }
{  Robert Chandler<robertc@Helpware.net>                }
{  http://Helpware.net                                  }
{  Copyright (c) 2001-2008 The Helpware Group           }
{-------------------------------------------------------}

{
  5-Nov-2002: RWC                                      
    Tested file under Delphi 7. D6 problem still there 
    under D7. Added $I statement and COMPILER_6_UP     
  9-Jan-2003: RWC                                      
    When .HLP file is specified we get a stack overflow
    Thanks to Paul van der Eijk for the fix            
  6-Jan-2006: RWC
    A couple of users found a small memory leak of 20 bytes 
    in THelpSelector and added HelpSelector._Release to the 
    finalization section of the unit to fix this.See "RWC_RELEASE".
  27-Aug-2008: RWC
    User Charles McAllister posted this solution to get rid 
    of the small shutdown memory leak (found using FastMM 4.88).
    Search for initials CBM.
}



{$I Compilers_.inc}
{$IFNDEF COMPILER_6_UP}
  {Attention: This unit only works with the Delphi 6+ compiler.}
  Only_For_Delphi67_Compiler;  //Generates compiler error if <> D6+ compiler
{$ENDIF}

unit D6OnHelpFix;
{
  Before D6, if we wanted to get a non-WinHelp Help system working for a Delphi
  application, we simply took over the Form.OnHelp or Application.OnHelp event(s).
  Our OnHelp code would typically divert WinHelp calls off to another API, such
  as MS HTML Help or MS Help 2.

  There is a bug in Delphi 6 where the Form.OnHelp and Application.OnHelp
  events do not work as they did in D2/3/4/5. You can still hook OnHelp
  however only F1 help events and the Application.HelpCommand() calls
  work. And only if the current forms (biHelp in BorderIcons) property is set.

  This module fixes this problem by making sure all help events are
  diverted to Application.OnHelp like it was in previous versions of Delphi.

  Simply include this Unit in your project, and OnHelp will fire correctly.

ABout the D6 OnHelp bug:

  Delphi6 does not pass on all help calls to the OnHelp event.
  Infact if you search for WMHelp code in Forms.pas you will see that
  only Application.InvokeHelp() handles FOnHelp. And that it only works
  if the (biHelp in BorderIcons) property is true. 

  Application.HelpCommand() is ok as it calls InvokeHelp(). But all other help
  calls bypass InvokeHelp() and call the D6 Help Manager directly.
  EG. Application.HelpContext(), Application.HelpJump().

  The provided D6OnHelpFix.pas module implements a second help viewer which
  catches all lost help calls and passes them onto OnHelp.

Mini Tutorial:

  Lets look at D6OnHelpFix.pas in some more detail. The module registers itself
  as a Help Viewer in the initialization section. Because we are now second in
  the Help Manager's list of help viewers (this cannot be changed), we need to
  make a few adjustments so that we get a chance to grab the help calls as they
  come through.

  Look at the Help Manager code (HelpIntfs.pas) and viewer 1
  code (WinHelpViewer.pas). The help manager in most cases will use the
  first viewer found in it's list of viewers. How do we get a foot in?

  == WinHelpTester

  Notice in Viewer 1 code (WinHelpViewer.pas) there is a thing called
  WinHelpTester. It is normally unused (unassigned). If we create an instance
  of it (it is global in scope in WinHelpViewer.pas), then we can change the
  behavior of Viewer 1's code. When can Help Manager asks viewer 1, Can you do
  something we can force a NO and Help Manager will go on and ask our viewer.
  Search for "WinHelpTester" in this module. You will see we have created it
  and assigned some responses that will be used by Viewer 1 code (WinHelpViewer.pas).

  Note: Our viewer module (D6OnHelpFix.pas) also exports a function called
  WinHelpTester_Enable() which can be used to enable/disable WinHelpTester.
  Calling WinHelpTester_Enable(false) allows Viewer 1 to take charge again
  and handle any WinHelp calls you might have.

  Thus typically you might do this
  a) call WinHelpTester_Enable(false)
  b) Make your WinHelp calls
  c) call WinHelpTester_Enable(true) to reenable our OnHelp fix again.

  == IHelpSelector

  HelpManager uses IHelpSelector if more than one viewer offers to help with
  a Keyword or TableOfContents. By implementing IHelpSelector we have another
  way of getting in before Viewer 1.

  Note: IHelpSeelctor is not really necessary. You can override our
  implementation if necessary simply by creating your own HelpSelector
  and calling Application.HelpSystem.AssignHelpSelector(HelpSelector);

  == IExtendedHelpViewer

  This interface is needed so we can catch WinHelp Context and Jump calls.
  We pass Jump calls onto the OnHelp event as a HELP_KEY (keyword) command.
  Viewer 1 also implements the interface, however you will notice
  that our WinHelpTester implementation changes the response for
  viewer 1's UnderstandsTopic() and UnderstandsContext() so that they return
  false letting viewer 2 get a go.

  Thats the end of the quick tour. Study HelpIntfs.pas (HelpManager),
  WinHelpViewer.pas (Viewer 1 code) and D6OnHelp.pas (our viewer 2 code)
  to learn more about how it all works. Also read the online help.

  Enjoy.
    Robert Chandler,



--------------------------------------------------------------------------------

* = WinHelp Commands passed onto Form.OnHelp or Application.OnHelp if assigned

  *HELP_CONTEXT = 1;       //Display topic in ulTopic
  *HELP_QUIT = 2;          // Terminate help - application is closing
  *HELP_CONTENTS = 3;      // Show Help contents
  *HELP_KEY = 257;         //Keyword
  *HELP_SETPOPUP_POS = 13; //Sends control x,y position before a context call

  HELP_INDEX = 3;          // Display index
  HELP_HELPONHELP = 4;     // Display help on using help
  HELP_SETINDEX = 5;       // Set current Index for multi index help
  HELP_SETCONTENTS = 5;
  HELP_CONTEXTPOPUP = 8;
  HELP_FORCEFILE = 9;
  HELP_COMMAND = 258;
  HELP_PARTIALKEY = 261;
  HELP_MULTIKEY = 513;
  HELP_SETWINPOS = 515;
  HELP_CONTEXTMENU = 10;
  HELP_FINDER = 11;
  HELP_WM_HELP = 12;
  HELP_TCARD = $8000;
  HELP_TCARD_DATA = $10;
  HELP_TCARD_OTHER_CALLER = 17;

}


interface

uses Classes;

procedure WinHelpTester_Enable(aEnable: Boolean);

{ =========================================================================== }

implementation

{$IFDEF MSWINDOWS}
uses HelpIntfs, WinHelpViewer, SysUtils, Windows, Forms, Controls;
{$ENDIF}
{$IFDEF LINUX}
uses HelpIntfs, SysUtils, Libc;
{$ENDIF}


var ViewerName : String = 'D6OnHelpFix';


{ THTMLHelpViewer.
  THTMLHelpViewer implements the interfaces supported by WinHelp ---
  ICustomHelpViewer (required of all Help Viewers),
  IExtendedHelpViewer (Topic and Context),
  ISpecialWinHelpViewer (Winhelp-specific messages),
  IHelpSelector interface to handle Keyword and Table Of Contents requests
  }

type
 THTMLHelpViewer = class(TInterfacedObject, ICustomHelpViewer, IExtendedHelpViewer{, ISpecialWinHelpViewer})
  private
   FViewerID: Integer;
  public
   FHelpManager: IHelpManager;

   constructor Create;
   destructor Destroy; override;

   function HelpFile(const Name: String) : String;
   procedure InternalShutDown;
   procedure HelpCommand_HELP_SETPOPUP_POS;

   { ICustomHelpViewer }
   function GetViewerName : String;
   procedure NotifyID(const ViewerID: Integer);
   procedure SoftShutDown;
   procedure ShutDown;

   function UnderstandsKeyword(const HelpString: String): Integer;
   function GetHelpStrings(const HelpString: String): TStringList;
   function CanShowTableOfContents: Boolean;
   procedure ShowTableOfContents;
   procedure ShowHelp(const HelpString: String);

   { IExtendedHelpViewer }
   function UnderstandsTopic(const Topic: String): Boolean;
   procedure DisplayTopic(const Topic: String);
   function UnderstandsContext(const ContextID: Integer;
                               const HelpFileName: String): Boolean;
   procedure DisplayHelpByContext(const ContextID: Integer;
                                  const HelpFileName: String);

//   { ISpecialWinHelpViewer }
//   function CallWinHelp(Handle: LongInt; const HelpFileName: String;
//                        Command: Word; Data: LongInt) : Boolean;

   property ViewerID : Integer read FViewerID;
   property HelpManager : IHelpManager read FHelpManager write FHelpManager;
  end;


{ global instance of THTMLHelpViewer which HelpIntfs can talk to. }
var
  HelpViewer : THTMLHelpViewer;

{----------------------------------------------------------------------------}
{ THelpSelector                                                              }
{----------------------------------------------------------------------------}

 { IHelpSelector. IHelpSelector is used by the HelpSystem to ask the
   application to decide which keyword, out of multiple matches returned
   by multiple different Help Viewers, it wishes to support. If an application
   wishes to support this, it passes an IHelpSelector interface into
   IHelpSystem.AssignHelpSelector. }
type
  THelpSelector = class (TInterfacedObject, IHelpSelector)
  public
    function SelectKeyword(Keywords: TStrings) : Integer;
    function TableOfContents(Contents: TStrings): Integer;
  end;

var
  HelpSelector : IHelpSelector;

{Note: Never called - Since we are the only Keyword player in town}
function THelpSelector.SelectKeyword(Keywords: TStrings) : Integer;
begin
  Result := 0;      //return index of first item in supplied keyword list
end;

{Returning our name poistion in the provided list will ensure that we are used to display the TOC}
function THelpSelector.TableOfContents(Contents: TStrings): Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Contents.count-1 do
    if Contents[I] = ViewerName then    //Found ourselves in the list
      Result := I;
end;

{----------------------------------------------------------------------------}
{ TWinHelpTester                                                             }
{----------------------------------------------------------------------------}

{ Look though the standard viewer code, WinHelpViewer.pas which this
  module is based on. Typically viewer 1 is first cab off the rank and
  wants to handle the help call. Except... there is this global thing
  "WinHelpTester" which if implemented can override the decisions made
  in Viewer 1. That's what this section is. We implement WinHelpTester
  and can manipulate the decisions made by Viewer 1. Viewer 2 now gets
  a chance to handle all help calls.

}
type
  TWinHelpTester = class (TInterfacedObject, IWinHelpTester)
  public
    function CanShowALink(const ALink, FileName: string): Boolean;
    function CanShowTopic(const Topic, FileName: string): Boolean;
    function CanShowContext(const Context: Integer; const FileName: string): Boolean;
    function GetHelpStrings(const ALink: string): TStringList;
    function GetHelpPath: string;
    function GetDefaultHelpFile: string;
  end;


function TWinHelpTester.CanShowALink(const ALink, FileName: string): Boolean;
begin
  Result := FALSE;
end;


function TWinHelpTester.CanShowTopic(const Topic, FileName: string):
Boolean;
begin
  Result := False;
end;


function TWinHelpTester.CanShowContext(const Context: Integer; const FileName: string): Boolean;
begin
  Result := False;
end;


function TWinHelpTester.GetHelpStrings(const ALink: string): TStringList;
begin
  Result := TStringList.Create;
end;


{Used by HelpeViewer 1 for Linux - ignored for now}
function TWinHelpTester.GetHelpPath: string;
begin
  Result := '';
end;

{This will do for now - Not too important if using OnHelp}
function TWinHelpTester.GetDefaultHelpFile: string;
begin
  Result := '';
  if Assigned(HelpViewer) then
    Result := HelpViewer.HelpFile('');
end;


{----------------------------------------------------------------------------}
{ THTMLHelpViewer                                                            }
{----------------------------------------------------------------------------}

constructor THTMLHelpViewer.Create;
begin
  inherited Create;
end;

destructor THTMLHelpViewer.Destroy;
begin
  inherited Destroy;
end;


function THTMLHelpViewer.HelpFile(const Name: String): String;
var
  FileName : String;
begin
  if (Name = '') and Assigned(FHelpManager) then
    FileName := HelpManager.GetHelpFile
  else
    FileName := Name;
  Result := FileName;
end;


{ InternalShut Down is called from unit finalization if the unit is exiting
  and the Help Manager needs to be informed. }

procedure THTMLHelpViewer.InternalShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then
  begin
    HelpManager.Release(ViewerID);

   //27-Aug-2008
   //!!CBM: I think Self is no longer valid after HelpManager.Release, as the release 
   //!!CBM: will destroy help viewers so its not necessary to nil out HelpManager.
   // if Assigned(FHelpManager) then HelpManager := nil;

  end;
end;

{ Send the HELP_SETPOPUP_POS command
  just before a Context help call. }

procedure THTMLHelpViewer.HelpCommand_HELP_SETPOPUP_POS;
var
  Control: TWinControl;
  Pt: TSmallPoint;

  function ControlHasHelp(const Control: TWinControl) : Boolean;
  begin
    Result := false;
    if (Control.HelpType = htContext) and (Control.HelpContext <> 0)
      then Result := true
    else if (Control.HelpType = htKeyword) and (Control.HelpKeyword <> '') then
      Result := true;
  end;

begin
  {This is not the best - since F1 press could have come from a memu -- no way of telling}
  Control := Screen.ActiveControl;
  while (Control <> nil) and ( not ControlHasHelp(Control)) do
    Control := Control.Parent;
  if Control <> nil then begin
    Pt := PointToSmallPoint(Control.ClientToScreen(Point(0, 0)));
    Application.HelpCommand(HELP_SETPOPUP_POS, Longint(Pt));
  end;
end;


{----------------------------------------------------------------------------}
{ THTMLHelpViewer - ICustomHelpViewer                                        }
{----------------------------------------------------------------------------}

function THTMLHelpViewer.GetViewerName : String;
begin
  Result := ViewerName;
end;

{ UnderstandsKeyword is a querying function that the Help Manager calls to
  determine if the Viewer provide helps on a particular keyword string. }
function THTMLHelpViewer.UnderstandsKeyword(const HelpString: String): Integer;
begin
  Result := 1;
end;

{ GetHelpStrings is used by the Help Manager to display a list of keyword
  matches from which an application's user can select one. It assumes
  that the String List is properly allocated, so this function should
  never return nil. }

function THTMLHelpViewer.GetHelpStrings(const HelpString: String): TStringList;
begin
  Result := TStringList.Create;
  Result.Add(GetViewerName + ': ' + HelpString);
end;


{ CanShowTableOfContents is a querying function that the Help Manager
  calls to determine if the Viewer supports tables of contents. WinHelp
  and HyperHelp both do. }

function THTMLHelpViewer.CanShowTableOfContents : Boolean;
begin
  Result := true;
end;


{ ShowTableOfContents is a command function that the Help Manager uses
  to direct the Viewer to display a table of contents. It is never
  called without being preceded by a call to CanShowTableOfContents. }

procedure THTMLHelpViewer.ShowTableOfContents;
begin
  { The Fix!! - data ignored we set to zero}
  Application.HelpCommand(HELP_CONTENTS, 0);
end;


{ ShowHelp is the function that the Help Manager calls to request that
  a Help Viewer display help for a given keyword. }
procedure THTMLHelpViewer.ShowHelp(const HelpString: String);
var
  HelpCommand: array[0..255] of Char;
begin
  StrLFmt(HelpCommand, SizeOf(HelpCommand) -1, '%s', [HelpString]);

  { The Fix!! }
  Self.HelpCommand_HELP_SETPOPUP_POS;
  Application.HelpCommand(HELP_KEY, Longint(@HelpCommand));
end;


{ NotifyID is called by the Help Manager after a successful registration
  to provide the Help Viewer with a cookie which uniquely identifies the
  Viewer to the Manager, and can be used in communications between the two. }

procedure THTMLHelpViewer.NotifyID(const ViewerID: Integer);
begin
  FViewerID := ViewerID;
end;

{ SoftShutDown is called by the help manager to ask the viewer to
  terminate any externally spawned subsystem without shutting itself down. }

procedure THTMLHelpViewer.SoftShutDown;
begin
  { The Fix!! }
  {rob: Commented this out - caused an error on a user in Win98}
  //Application.HelpCommand(HELP_QUIT, 0);
end;

procedure THTMLHelpViewer.ShutDown;
begin
  SoftShutDown;
  if Assigned(FHelpManager) then HelpManager := nil;
end;

{----------------------------------------------------------------------------}
{ THTMLHelpViewer --- IExtendedHelpViewer                                     }
{----------------------------------------------------------------------------}

{ UnderstandsTopic is called by the Help Manager to ask if the Viewer
  is capable of displaying a topic-based help query for a given topic. }

function THTMLHelpViewer.UnderstandsTopic(const Topic: String): Boolean;
begin
  Result := true;
end;

{ DisplayTopic is called by the Help Manager if a Help Viewer claims
  in its response to UnderstandsTopic to be able to provide Topic-based
  help for a particular keyword. }

procedure THTMLHelpViewer.DisplayTopic(const Topic: String);
var
  HelpCommand: array[0..255] of Char;
begin
  StrLFmt(HelpCommand, SizeOf(HelpCommand) -1, '%s', [Topic + 'zzz']);

  { The Fix!! }
  Self.HelpCommand_HELP_SETPOPUP_POS;
  Application.HelpCommand(HELP_KEY, Longint(@HelpCommand));
end;


{ UnderstandsContext is a querying function called by the Help Manager
  to determine if an Extended Help Viewer is capable of providing
  help for a particular context-ID. }

function THTMLHelpViewer.UnderstandsContext(const ContextID: Integer;
                                           const HelpFileName: String): Boolean;
begin
  Result := true;
end;


{ DisplayHelpByContext is used by the Help Manager to request that a
  Help Viewer display help for a particular Context-ID. }

procedure THTMLHelpViewer.DisplayHelpByContext(const ContextID: Integer; const HelpFileName: String);
var SaveWinHelpTester: IWinHelpTester;
begin
  { The Fix!! }
  Self.HelpCommand_HELP_SETPOPUP_POS;
  { 9-Jan-2003: Stop recursion by saving state}
  SaveWinHelpTester := WinHelpViewer.WinHelpTester;
  WinHelpViewer.WinHelpTester := nil;
    Application.HelpCommand(HELP_CONTEXT, ContextID);
  WinHelpViewer.WinHelpTester := SaveWinHelpTester;
end;




//{----------------------------------------------------------------------------}
//{ THTMLHelpViewer --- ISpecialWinHelpViewer                                   }
//{----------------------------------------------------------------------------}
//
//function THTMLHelpViewer.CallWinHelp(Handle: LongInt; const HelpFileName: String;
//                                    Command: Word; Data: LongInt) : Boolean;
//begin
//  Result := false;
//end;


{ Uses this function to enable or disable WinHelpViewer.WinHelpTester.
  WinHelpTester is enabled when you use this module (viewer 2). It
  is used to stop Viewer 1 being favoured by the HelpManager.
  If you need to call a WinHelp function (viewer 1 needed) then
  you would need to disable winhelptester for that call.
  See Delphi source WinHelpViewer.pas for where WinHelpTester is used.

  Note: WinHelpTester is set to Nil by WinHelpViewer cleanup
}
procedure WinHelpTester_Enable(aEnable: Boolean);
begin
  {Enable WinHelpTester - ie. Make HelpManager use this viewer}
  if aEnable then begin
    if not Assigned(WinHelpViewer.WinHelpTester) then
      WinHelpViewer.WinHelpTester := TWinHelpTester.Create;
  end
  {Disable WinHelpTester - ie. Make HelpManager use WinHelpViewer again}
  else begin
    if Assigned(WinHelpViewer.WinHelpTester) then
      WinHelpViewer.WinHelpTester := Nil;
  end;
end;


{============================================================================}

initialization
  WinHelpTester_Enable(true);
  HelpSelector := THelpSelector.Create;                  //set to Nil by HelpIntfs cleanup
  HelpViewer := THTMLHelpViewer.Create;
  Application.HelpSystem.AssignHelpSelector(HelpSelector);
  HelpIntfs.RegisterViewer(HelpViewer {ICustomHelpViewer}, HelpViewer.FHelpManager {IHelpManager});
finalization
  //16-Jan-2003: RWC - Daniel Waeber added this line
  //If you find problems safer to comment this line out and put up with a 20 byte memory leak.
  //This fix may not be compatible with future versions of Delphi.
//HelpSelector._Release;   //RWC_RELEASE

  // Another user says HelpSelector._Release;
  // causes an exception in D7, followed by a runtime error 216. It is unsafe should be replaced with 
// HelpSelector := nil;

  if Assigned(HelpViewer.FHelpManager) then
  begin
    HelpViewer.InternalShutDown;
  end;

  //27-Aug-2008: !!CBM 
  HelpSelector := nil;  
end.

