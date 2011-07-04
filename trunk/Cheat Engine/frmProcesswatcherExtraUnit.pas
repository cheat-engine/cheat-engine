unit frmProcesswatcherExtraUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

type
  TfrmProcessWatcherExtra = class(TForm)
    data: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProcessWatcherExtra: TfrmProcessWatcherExtra;

implementation


initialization
  {$i frmProcesswatcherExtraUnit.lrs}

end.
