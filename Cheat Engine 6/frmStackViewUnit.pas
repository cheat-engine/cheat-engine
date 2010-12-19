unit frmStackViewUnit;

{$mode delphi}

interface

uses
  windows, cefuncproc, newkernelhandler, Classes, SysUtils, FileUtil, LResources,
  Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, stacktrace2;

type

  { TfrmStackView }

  TfrmStackView = class(TForm)
    ListBox1: TListBox;
    miAddESP: TMenuItem;
    miAddEBP: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure miAddESPClick(Sender: TObject);
  private
    { private declarations }
    c: PContext;
    stack: pbyte;
    size: integer;
  public
    { public declarations }
    procedure SetContextPointer(c: PContext; stack: pbyte; size: integer);
  end; 

var
  frmStackView: TfrmStackView;

implementation

procedure TfrmStackView.miAddESPClick(Sender: TObject);
begin
  SetContextPointer(c, stack, size);
end;

procedure TfrmStackView.SetContextPointer(c: PContext; stack: pbyte; size: integer);
begin
  self.c:=c;
  self.stack:=stack;
  self.size:=size;

  listbox1.Items.Clear;
  ce_stacktrace(c.{$ifdef cpu64}rsp{$else}esp{$endif}, c.{$ifdef cpu64}rbp{$else}ebp{$endif}, c.{$ifdef cpu64}rip{$else}eip{$endif}, pbytearray(stack), size, listbox1.Items, true,false,false,0,miAddEBP.checked);
end;

initialization
  {$I frmStackViewUnit.lrs}

end.

