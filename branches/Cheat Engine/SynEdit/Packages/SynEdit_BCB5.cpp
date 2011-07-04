//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("SynEdit_BCB5.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\Source\SynEditReg.pas");
USERES("..\Source\SynEditReg.dcr");
USEUNIT("..\Source\SynEditPropertyReg.pas");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcldb50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
