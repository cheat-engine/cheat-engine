unit frmMergePointerscanResultSettingsUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TfrmMergePointerscanResultSettings }

  TfrmMergePointerscanResultSettings = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    lblDescription: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    rgGroupMethod: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure rgGroupMethodSelectionChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMergePointerscanResultSettings: TfrmMergePointerscanResultSettings;

implementation

{$R *.lfm}

resourcestring
  rsCopiesTheSourcePtrFilesToTheWorkingFolder = 'Copies the source .ptr.# files to the working folder of the main PTR';
  rsMovesTheSourcePtrFilesToTheWorkingFolder = 'Moves the source .ptr.# files to the working folder of the main PTR'#13#10'Note: This destroys the source';
  rsLinksDirectlyToThePathsOfTheSourcePtrFiles = 'Links directly to the paths of the source .ptr.# files.'#13#10'Note: The result will be unusable by other systems that do not have the same path layout, until you generate new results';

{ TfrmMergePointerscanResultSettings }

const GroupDescription: array [-1..2] of string=('WTF',rsCopiesTheSourcePtrFilesToTheWorkingFolder,rsMovesTheSourcePtrFilesToTheWorkingFolder, rsLinksDirectlyToThePathsOfTheSourcePtrFiles);

procedure TfrmMergePointerscanResultSettings.rgGroupMethodSelectionChanged(Sender: TObject);
begin
  lbldescription.caption:=GroupDescription[rgGroupMethod.itemindex];
end;

procedure TfrmMergePointerscanResultSettings.FormCreate(Sender: TObject);
begin
  rgGroupMethodSelectionChanged(rgGroupMethod);
end;

end.

