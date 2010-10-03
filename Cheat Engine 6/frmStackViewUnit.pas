unit frmStackViewUnit;

{$mode delphi}

interface

uses
  windows, cefuncproc, newkernelhandler, Classes, SysUtils, FileUtil, LResources,
  Forms, Controls, Graphics, Dialogs, StdCtrls, stacktrace2;

type

  { TfrmStackView }

  TfrmStackView = class(TForm)
    ListBox1: TListBox;
  private
    { private declarations }
  public
    { public declarations }
    procedure SetContextPointer(c: PContext; stack: pbyte; size: integer);
  end; 

var
  frmStackView: TfrmStackView;

implementation

procedure TfrmStackView.SetContextPointer(c: PContext; stack: pbyte; size: integer);
begin
  listbox1.Items.Clear;
  ce_stacktrace(c.{$ifdef cpu64}rsp{$else}esp{$endif}, c.{$ifdef cpu64}rbp{$else}ebp{$endif}, c.{$ifdef cpu64}rip{$else}eip{$endif}, PPtrUintArray(stack), size, listbox1.Items);
end;

initialization
  {$I frmStackViewUnit.lrs}

end.

