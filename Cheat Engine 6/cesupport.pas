unit cesupport;

{$mode delphi}

interface

uses
  Classes, SysUtils,forms, controls, windows, activex, comobj;

type TADWindow=class(TCustomForm)
  private
    browserisvalid: boolean;
    browser: Olevariant;
    optional: string;
    function getoptionalstring: string;
    function getBase: string;
  public
    procedure LoadAd;
    constructor CreateNew(AOwner: TComponent);
end;

var adwindow: TADWindow;

implementation

function TADWindow.getoptionalstring: string;
begin
  if optional<>'' then
    result:='&'+optional
  else
    result:='';
end;

function TADWindow.getBase: string;
begin
  result:='http://www.cheatengine.org/ceads.php';
end;

procedure TADWindow.LoadAd;
var url: widestring;
begin
  if browserisvalid then
  begin
    url:=getbase+'?cewidth='+inttostr(clientwidth)+'&ceheight='+inttostr(clientheight)+getoptionalstring;
    browser.Navigate(url);
  end;
end;

constructor TADWindow.createNew(AOwner: TComponent);
begin
  inherited createnew(AOwner);

 {$ifdef windows}
  try
    browser := CreateOleObject('InternetExplorer.Application');
    windows.setparent(browser.hwnd, handle); // you can use panel1.handle, etc..
    browser.toolbar:=false;
    browser.fullscreen:=true;
    browser.Resizable:=false;
    browser.visible:=true;
    browserisvalid:=true; //we got to this point without a horrible crash, so I guess it's ok
  except

  end;
 {$endif}

end;


end.

