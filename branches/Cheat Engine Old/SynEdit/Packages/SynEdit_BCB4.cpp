//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("SynEdit_BCB4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("..\Source\SynEditReg.pas");
USERES("..\Source\SynEditReg.dcr");
USEUNIT("..\Source\SynEditPropertyReg.pas");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("vcldb40.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
