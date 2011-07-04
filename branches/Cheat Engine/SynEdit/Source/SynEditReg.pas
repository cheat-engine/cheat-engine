{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditReg.pas, released 2000-04-07.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditReg.pas,v 1.33 2004/06/13 17:22:40 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITREG}
unit SynEditReg;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  // SynEdit components
  QSynEdit,
  QSynMemo,
  {$IFNDEF SYN_DELPHI_PE}
  QSynDBEdit,
  {$ENDIF}
  QSynEditStrConst,
  QSynEditHighlighter,
  QSynEditMiscClasses,
  QSynEditPlugins,
  QSynEditExport,
  QSynExportHTML,
  QSynExportRTF,
  QSynExportTeX,
  QSynHighlighterMulti,
  QSynCompletionProposal,
  QSynEditPythonBehaviour,
  QSynEditPrint,
  QSynEditPrintPreview,
  QSynMacroRecorder,
  QSynAutoCorrect,
  QSynEditSearch,
  QSynEditRegexSearch,
  QSynHighlighterManager,
  QSynEditOptionsDialog,
  QSynHighlighterADSP21xx,
  QSynHighlighterAsm,
  QSynHighlighterAWK,
  QSynHighlighterBaan,
  QSynHighlighterBat,
  QSynHighlighterCAC,
  QSynHighlighterCache,
  QSynHighlighterCobol,
  QSynHighlighterCpp,
  QSynHighlighterCS,
  QSynHighlighterCss,
  QSynHighlighterDfm,
  QSynHighlighterDml,
  QSynHighlighterDOT,
  QSynHighlighterEiffel,
  QSynHighlighterFortran,
  QSynHighlighterFoxpro,
  QSynHighlighterGalaxy,
  QSynHighlighterGeneral,
  QSynHighlighterHaskell,
  QSynHighlighterHC11,
  QSynHighlighterHP48,
  QSynHighlighterHtml,
  QSynHighlighterIni,
  QSynHighlighterInno,
  QSynHighlighterJava,
  QSynHighlighterJScript,
  QSynHighlighterKix,
  QSynHighlighterModelica,
  QSynHighlighterM3,
  QSynHighlighterPas,
  QSynHighlighterPerl,
  QSynHighlighterPHP,
  QSynHighlighterProgress,
  QSynHighlighterPython,
  QSynHighlighterRC,
  QSynHighlighterRuby,
  QSynHighlighterSml,
  QSynHighlighterSQL,
  QSynHighlighterTclTk,
  QSynHighlighterTeX,
  QSynHighlighterUNIXShellScript,
  QSynHighlighterURI,
  QSynHighlighterVB,
  QSynHighlighterVBScript,
  QSynHighlighterVrml97,
  QSynHighlighterGWS,
  QSynHighlighterCPM,
  QSynHighlighterSDD,
  QSynHighlighterXML,
  QSynHighlighterMsg,
  QSynHighlighterIDL,
  QSynHighlighterUnreal,
  QSynHighlighterST,
  QSynHighlighterLDraw,
  QSynURIOpener,
{$ELSE}
  // SynEdit components
  SynEdit,
  SynMemo,
  {$IFNDEF SYN_DELPHI_PE}
  SynDBEdit,
  {$ENDIF}
  SynEditStrConst,
  SynEditHighlighter,
  SynEditMiscClasses,
  SynEditPlugins,
  SynEditExport,
  SynExportHTML,
  SynExportRTF,
  SynExportTeX,
  SynHighlighterMulti,
  SynCompletionProposal,
  SynEditPythonBehaviour,
  SynEditPrint,
  SynEditPrintPreview,
  SynMacroRecorder,
  SynAutoCorrect,
  SynEditSearch,
  SynEditRegexSearch,
  {$IFDEF SYN_COMPILER_4_UP}
  SynHighlighterManager,
  {$ENDIF}
  SynEditOptionsDialog,
  SynHighlighterADSP21xx,
  SynHighlighterAsm,
  SynHighlighterAWK,
  SynHighlighterBaan,
  SynHighlighterBat,
  SynHighlighterCAC,
  SynHighlighterCache,
  SynHighlighterCobol,
  SynHighlighterCpp,
  SynHighlighterCS,
  SynHighlighterCss,
  SynHighlighterDfm,
  SynHighlighterDml,
  SynHighlighterDOT,
  SynHighlighterEiffel,
  SynHighlighterFortran,
  SynHighlighterFoxpro,
  SynHighlighterGalaxy,
  SynHighlighterGeneral,
  SynHighlighterHaskell,
  SynHighlighterHC11,
  SynHighlighterHP48,
  SynHighlighterHtml,
  SynHighlighterIni,
  SynHighlighterInno,
  SynHighlighterJava,
  SynHighlighterJScript,
  SynHighlighterKix,
  SynHighlighterModelica,
  SynHighlighterM3,
  SynHighlighterPas,
  SynHighlighterPerl,
  SynHighlighterPHP,
  SynHighlighterProgress,
  SynHighlighterPython,
  SynHighlighterRC,
  SynHighlighterRuby,
  SynHighlighterSml,
  SynHighlighterSQL,
  SynHighlighterTclTk,
  SynHighlighterTeX,
  SynHighlighterUNIXShellScript,
  SynHighlighterURI,
  SynHighlighterVB,
  SynHighlighterVBScript,
  SynHighlighterVrml97,
  SynHighlighterGWS,
  SynHighlighterCPM,
  SynHighlighterSDD,
  SynHighlighterXML,
  SynHighlighterMsg,
  SynHighlighterIDL,
  SynHighlighterUnreal,
  SynHighlighterST,
  SynHighlighterLDraw,
  SynURIOpener,
{$ENDIF}
  Classes;

procedure Register;

implementation

procedure Register;
begin
// SynEdit main components
  RegisterComponents(SYNS_ComponentsPage, [TSynEdit, TSynMemo]);

{$IFNDEF SYN_DELPHI_PE}
  RegisterComponents(SYNS_ComponentsPage, [TDBSynEdit]);
{$ENDIF}

{$IFDEF SYN_COMPILER_6_UP}
  GroupDescendentsWith(TSynCustomHighlighter, TSynEdit);
  GroupDescendentsWith(TSynEditSearchCustom, TSynEdit);
  GroupDescendentsWith(TSynCustomExporter, TSynEdit);
  GroupDescendentsWith(TSynMultiSyn, TSynEdit);
  GroupDescendentsWith(TSynBaseCompletionProposal, TSynEdit);
  GroupDescendentsWith(TSynAutoComplete, TSynEdit);
  GroupDescendentsWith(TAbstractSynPlugin, TSynEdit);
  GroupDescendentsWith(TCustomSynAutoCorrect, TSynEdit);
  GroupDescendentsWith(TSynEditPrint, TSynEdit);
  GroupDescendentsWith(TSynEditPrintPreview, TSynEdit);
  GroupDescendentsWith(TSynEditPythonBehaviour, TSynEdit);
  GroupDescendentsWith(TSynHighlighterManager, TSynEdit);
  GroupDescendentsWith(TSynEditOptionsDialog, TSynEdit);
  GroupDescendentsWith(TSynURIOpener, TSynEdit);
{$ENDIF}

// SynEdit extra components
  RegisterComponents(SYNS_ComponentsPage, [TSynExporterHTML, TSynExporterRTF,
    TSynExporterTeX, TSynEditPythonBehaviour, TSynMultiSyn,
    TSynCompletionProposal, TSynAutoComplete, TSynMacroRecorder,
    TSynEditPrint, TSynEditPrintPreview, TSynAutoCorrect,
    TSynEditSearch, TSynEditRegexSearch, TSynEditOptionsDialog, TSynURIOpener]);
{$IFDEF SYN_COMPILER_4_UP}
  RegisterComponents(SYNS_ComponentsPage, [TSynHighlighterManager]);
{$ENDIF}

// SynEdit highlighters
  RegisterComponents(SYNS_HighlightersPage, [
    //classic
    TSynCppSyn, TSynEiffelSyn, TSynFortranSyn, TSynGeneralSyn, TSynJavaSyn,
    TSynM3Syn, TSynPasSyn, TSynVBSyn, TSynCobolSyn, TSynCSSyn,
    // internet
    TSynCssSyn, TSynHTMLSyn, TSynJScriptSyn, TSynPHPSyn, TSynVBScriptSyn,
    TSynXMLSyn, TSynVrml97Syn,
    //interpreted
    TSynAWKSyn, TSynBATSyn, TSynKixSyn, TSynPerlSyn, TSynPythonSyn,
    TSynTclTkSyn, TSynGWScriptSyn, TSynRubySyn, TSynUNIXShellScriptSyn, 
    //database
    TSynCACSyn, TSynCacheSyn, TSynFoxproSyn, TSynSQLSyn, TSynSDDSyn,
    //assembler
    TSynADSP21xxSyn, TSynAsmSyn, TSynHC11Syn, TSynHP48Syn, TSynSTSyn,
    //data modeling
    TSynDmlSyn, TSynModelicaSyn, TSynSMLSyn,
    //data
    TSynDfmSyn, TSynIniSyn, TSynInnoSyn,
    // other
    TSynBaanSyn, TSynGalaxySyn, TSynProgressSyn, TSynMsgSyn,
    TSynIdlSyn, TSynUnrealSyn, TSynCPMSyn, TSynTeXSyn, 
    TSynHaskellSyn, TSynLDRSyn, TSynURISyn, TSynDOTSyn, TSynRCSyn
  ]);
end;

end.
