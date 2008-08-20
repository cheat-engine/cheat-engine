//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("SynEdit_BCB3.res");
USEUNIT("..\Source\SynEditReg.pas");
USERES("..\Source\SynEditReg.dcr");
USEUNIT("..\Source\SynEditPropertyReg.pas");
USEPACKAGE("vcl35.bpi");
USEPACKAGE("vcldb35.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package-Quelle.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
