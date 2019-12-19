unit frmStructureLinkerUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, ExtCtrls, symbolhandler, StructuresFrm2, cefuncproc, newkernelhandler;

type

  Tstructinfo=record
    s: TDissectedStruct;
    address: ptruint;
  end;

  pstructinfo=^Tstructinfo;

  { TfrmStructureLinker }

  TfrmStructureLinker = class(TForm)
    btnCancel: TButton;
    btnLink: TButton;
    cbNoExactMatches: TCheckBox;
    cbOverrideLocal: TCheckBox;
    cbFillLocal: TCheckBox;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    sgStructureAddress: TStringGrid;
    procedure btnLinkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { private declarations }
    struct: array of Tstructinfo;
    function FindStructWithAddress(address: ptruint): pstructinfo;
    procedure fillInStruct(s: Tstructinfo);
  public
    { public declarations }
  end; 

implementation

{$R *.lfm}

{ TfrmStructureLinker }

uses ProcessHandlerUnit{$ifdef darwin},macport{$endif};

resourcestring
  strIsNotAValidAddress = '%s is not a valid address';



procedure TfrmStructureLinker.Panel1Resize(Sender: TObject);
begin
  btnLink.left:=(panel1.ClientWidth div 2) - (btnLink.Width - 2);
  btnCancel.left:=(panel1.ClientWidth div 2) + 2;
end;


procedure TfrmStructureLinker.FormCreate(Sender: TObject);
var i,j: integer;
begin
  //fill the list with structures
  setlength(struct, DissectedStructs.Count);

  sgStructureAddress.RowCount:=DissectedStructs.Count+1;
  for i:=0 to DissectedStructs.Count-1 do
  begin
    struct[i].s:=TDissectedStruct(DissectedStructs[i]);
    sgStructureAddress.Cells[0,i+1]:=TDissectedStruct(DissectedStructs[i]).name;

    //go through the frmStructure list and see if it is already defined
    for j:=0 to frmStructures2.count-1 do
      if TfrmStructures2(frmStructures2[j]).mainStruct=TDissectedStruct(DissectedStructs[i]) then
      begin
        //get the first address
        if TfrmStructures2(frmStructures2[j]).columnCount>0 then //should always be true, but check anyhow
        begin
          sgStructureAddress.Cells[1,i+1]:=IntToHex(TfrmStructures2(frmStructures2[j]).columns[0].Address,8);
          break;
        end;
      end;
  end;

end;


procedure TfrmStructureLinker.FormShow(Sender: TObject);
begin
  sgStructureAddress.Update; //fix graphical bug
end;

procedure TfrmStructureLinker.btnLinkClick(Sender: TObject);
var i: integer;
  e: boolean;
  value: string;
begin
  for i:=0 to length(struct)-1 do
  begin
    value:=sgStructureAddress.Cells[1,i+1];
    if value<>'' then
    begin
      struct[i].address:=symhandler.getAddressFromName(value,false, e);
      if e then raise exception.create(format(strIsNotAValidAddress, [struct[i].s.name+' : '+sgStructureAddress.Cells[1,i+1]]));
    end
    else
      struct[i].address:=0;
  end;

  //addressparsing passed, fill in the links

  for i:=0 to length(struct)-1 do
    fillInStruct(struct[i]);

  modalresult:=mrOk;
end;

function TfrmStructureLinker.FindStructWithAddress(address: ptruint): pstructinfo;
var i: integer;
begin
  result:=nil;
  for i:=0 to length(struct)-1 do
  begin
    if (struct[i].s.count>0) then
    begin
      if ((cbNoExactMatches.checked=false) and (address=struct[i].address)) or (InRangeX(address, struct[i].address, struct[i].address+struct[i].s[struct[i].s.count-1].Offset)) then
      begin
        result:=@struct[i];
        exit;
      end;
    end;
  end;
end;

procedure TfrmStructureLinker.fillInStruct(s: Tstructinfo);
var i: integer;
  newStruct: pstructinfo;

  temps: Tstructinfo;
  pvalue: ptruint;
  x: ptruint;
begin
  s.s.beginUpdate;
  try

    for i:=0 to s.s.count-1 do
    begin
      if s.s.element[i].isPointer then
      begin
        pvalue:=0;
        if ReadProcessMemory(processhandle, pointer(s.address+s.s.element[i].Offset), @pvalue, processhandler.pointersize, x) then
        begin
          if s.s.element[i].ChildStruct<>nil then
          begin
            //already filled in
            if s.s.element[i].ChildStruct.isInGlobalStructList then continue; //it's already a pointer to a defined struct

            //'Local' struct

            if cbOverrideLocal.checked then
            begin

              newStruct:=FindStructWithAddress(pvalue);
              if newstruct<>nil then
              begin
                s.s.element[i].ChildStruct:=newStruct.s;
                s.s.element[i].ChildStructStart:=pvalue-newStruct.address;
                continue;
              end;
            end;

            //still here so not redefined
            if cbFillLocal.checked then
            begin
              temps.s:=s.s.element[i].ChildStruct;
              temps.address:=s.address+s.s.element[i].Offset;
              if ReadProcessMemory(processhandle, pointer(temps.address), @temps.address, processhandler.pointersize,x) then
                fillInStruct(temps);
            end;
          end
          else
          begin
            newStruct:=FindStructWithAddress(pvalue);

            if newstruct<>nil then //found a match
            begin
              s.s.element[i].ChildStruct:=newStruct.s;
              s.s.element[i].ChildStructStart:=pvalue-newStruct.address;
            end;
          end;

        end;

      end; //else not a pointer and not interesting enough to fill in
    end;

  finally
    s.s.endUpdate;
  end;
end;

end.

