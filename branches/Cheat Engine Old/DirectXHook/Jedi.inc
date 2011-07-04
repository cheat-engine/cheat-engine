{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JEDI.INC.                                               }
{                                                                              }
{ The Initial Developer of the Original Code is Project JEDI                   }
{ http://www.delphi-jedi.org                                                   }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This file defines various generic compiler directives used in the JEDI Code  }
{ Library (JCL) and JEDI Visual Component Library Library (J-VCL). The         }
{ directives in this file are of generic nature and consist mostly of mappings }
{ from the VERXXX directives defined by Delphi and C++ Builder to friendly     }
{ names such as DELPHI5 and SUPPORTS_WIDESTRING. These friendly names are      }
{ subsequently used in both libraries to test for compiler versions and/or     }
{ whether the compiler supports certain features (such as widestring's or 64   }
{ bit integers. Both libraries provide an additional, library specific,        }
{ include file. For the JCL this is JCL.INC. These files should be included in }
{ source files instead of this file (which is pulled in automatically).        }
{                                                                              }
{ Maintainer: Marcel van Brakel                                                }
{ Last modified: May  25, 2002                                                 }
{                                                                              }
{******************************************************************************}

(*

- Development environment directives

  This file defines two driectives to indicate which development environment the
  library is being compiled with. Currently this can either be Delphi or
  C++ Builder (in the near future "Kylix" will be added).

  Directive    Description
  ------------------------------------------------------------------------------
  DELPHI       Defined if compiled with Delphi
  BCB          Defined if compiled with C++ Builder

- Platform Directives

  Platform directives are not explicitly defined in this file but are defined
  by the compiler itself. They are listed here only for completeness.

  Directive    Description
  ------------------------------------------------------------------------------
  WIN32        Defined when target platform is 32 bit Windows
  LINUX        Defined when target platform is Linux

- Delphi Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is Delphi (ie DELPHI is defined).

  Directive    Description
  ------------------------------------------------------------------------------
  DELPHI1      Defined when compiling with Delphi 1
  DELPHI2      Defined when compiling with Delphi 2
  DELPHI3      Defined when compiling with Delphi 3
  DELPHI4      Defined when compiling with Delphi 4
  DELPHI5      Defined when compiling with Delphi 5
  DELPHI6      Defined when compiling with Delphi 6
  DELPHI1_UP   Defined when compiling with Delphi 1 or higher
  DELPHI2_UP   Defined when compiling with Delphi 2 or higher
  DELPHI3_UP   Defined when compiling with Delphi 3 or higher
  DELPHI4_UP   Defined when compiling with Delphi 4 or higher
  DELPHI5_UP   Defined when compiling with Delphi 5 or higher
  DELPHI6_UP   Defined when compiling with Delphi 6 or higher
  DELPHI7_UP   Defined when compiling with Delphi 7 or higher

- C++ Builder Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. These directives are only defined if
  the compiler is C++ Builder (ie BCB is defined).

  Directive    Description
  ------------------------------------------------------------------------------
  BCB1         Defined when compiling with C++ Builder 1
  BCB3         Defined when compiling with C++ Builder 3
  BCB4         Defined when compiling with C++ Builder 4
  BCB5         Defined when compiling with C++ Builder 5
  BCB6         Defined when compiling with C++ Builder 6
  BCB7         Defined when compiling with C++ Builder 7
  BCB1_UP      Defined when compiling with C++ Builder 1 or higher
  BCB3_UP      Defined when compiling with C++ Builder 3 or higher
  BCB4_UP      Defined when compiling with C++ Builder 4 or higher
  BCB5_UP      Defined when compiling with C++ Builder 5 or higher
  BCB6_UP      Defined when compiling with C++ Builder 6 or higher
  BCB7_UP      Defined when compiling with C++ Builder 7 or higher

- Compiler Versions

  The following directives are direct mappings from the VERXXX directives to a
  friendly name of the associated compiler. Unlike the DELPHI_X and BCB_X
  directives, these directives are indepedent of the development environment.
  That is, they are defined regardless of whether compilation takes place using
  Delphi or C++ Builder.

  Directive    Description
  ------------------------------------------------------------------------------
  COMPILER1    Defined when compiling with Delphi 1
  COMPILER2    Defined when compiling with Delphi 2 or C++ Builder 1
  COMPILER3    Defined when compiling with Delphi 3
  COMPILER35   Defined when compiling with C++ Builder 3
  COMPILER4    Defined when compiling with Delphi 4 or C++ Builder 4
  COMPILER5    Defined when compiling with Delphi 5 or C++ Builder 5
  COMPILER6    Defined when compiling with Delphi 6 or C++ Builder 6
  COMPILER1_UP Defined when compiling with Delphi 1 or higher
  COMPILER2_UP Defined when compiling with Delphi 2 or C++ Builder 1 or higher
  COMPILER3_UP Defined when compiling with Delphi 3 or higher
  COMPILER35_UP Defined when compiling with C++ Builder 3 or higher
  COMPILER4_UP Defined when compiling with Delphi 4 or C++ Builder 4 or higher
  COMPILER5_UP Defined when compiling with Delphi 5 or C++ Builder 5 or higher
  COMPILER6_UP Defined when compiling with Delphi 6 or C++ Builder 6 or higher
  COMPILER7_UP Defined when compiling with Delphi 6 or C++ Builder 6 or higher

- Feature Directives

  The features directives are used to test if the compiler supports specific
  features, such as method overloading, and adjust the sources accordingly. Use
  of these directives is preferred over the use of the DELPHI and COMPILER
  directives.

  Directive              Description
  ------------------------------------------------------------------------------
  SUPPORTS_WIDESTRING    Compiler supports the WideString type (D3/BCB3 up)
  SUPPORTS_INTERFACE     Compiler supports interfaces (D3/BCB3)
  SUPPORTS_EXTSYM        Compiler supports the $EXTERNALSYM directive (D4/BCB3)
  SUPPORTS_NODEFINE      Compiler supports the $NODEFINE directive (D4/BCB3)
  SUPPORTS_INT64         Compiler supports the Int64 type (D4/BCB4)
  SUPPORTS_DYNAMICARRAYS Compiler supports dynamic arrays (D4/BCB4)
  SUPPORTS_DEFAULTPARAMS Compiler supports default parameters (D4/BCB4)
  SUPPORTS_OVERLOAD      Compiler supports overloading (D4/BCB4)

- Compiler Settings

  The compiler settings directives indicate whether a specific compiler setting
  is in effect. This facilitates changing compiler settings locally in a more
  compact and readible manner.

  Directive              Description
  ------------------------------------------------------------------------------
  ALIGN_ON               Compiling in the A+ state (no alignment)
  BOOLEVAL_ON            Compiling in the B+ state (complete boolean evaluation)
  ASSERTIONS_ON          Compiling in the C+ state (assertions on)
  DEBUGINFO_ON           Compiling in the D+ state (debug info generation on)
  IMPORTEDDATA_ON        Compiling in the G+ state (creation of imported data references)
  LONGSTRINGS_ON         Compiling in the H+ state (string defined as AnsiString)
  IOCHECKS_ON            Compiling in the I+ state (I/O checking enabled)
  WRITEABLECONST_ON      Compiling in the J+ state (typed constants can be modified)
  LOCALSYMBOLS           Compiling in the L+ state (local symbol generation)
  TYPEINFO_ON            Compiling in the M+ state (RTTI generation on)
  OPTIMIZATION_ON        Compiling in the O+ state (code optimization on)
  OPENSTRINGS_ON         Compiling in the P+ state (variable string parameters are openstrings)
  OVERFLOWCHECKS_ON      Compiling in the Q+ state (overflow checing on)
  RANGECHECKS_ON         Compiling in the R+ state (range checking on)
  TYPEDADDRESS_ON        Compiling in the T+ state (pointers obtained using the @ operator are typed)
  SAFEDIVIDE_ON          Compiling in the U+ state (save FDIV instruction through RTL emulation)
  VARSTRINGCHECKS_ON     Compiling in the V+ state (type checking of shortstrings)
  STACKFRAMES_ON         Compiling in the W+ state (generation of stack frames)
  EXTENDEDSYNTAX_ON      Compiling in the X+ state (Delphi extended syntax enabled)

*)

//------------------------------------------------------------------------------
// Compiler settings
//------------------------------------------------------------------------------

{$IFOPT A+} {$DEFINE ALIGN_ON} {$ENDIF}
{$IFOPT B+} {$DEFINE BOOLEVAL_ON} {$ENDIF}
{$IFOPT C+} {$DEFINE ASSERTIONS_ON} {$ENDIF}
{$IFOPT D+} {$DEFINE DEBUGINFO_ON} {$ENDIF}
{$IFOPT G+} {$DEFINE IMPORTEDDATA_ON} {$ENDIF}
{$IFOPT H+} {$DEFINE LONGSTRINGS_ON} {$ENDIF}
//HINTS
{$IFOPT I+} {$DEFINE IOCHECKS_ON} {$ENDIF}
{$IFOPT J+} {$DEFINE WRITEABLECONST_ON} {$ENDIF}
{$IFOPT L+} {$DEFINE LOCALSYMBOLS} {$ENDIF}
{$IFOPT M+} {$DEFINE TYPEINFO_ON} {$ENDIF}
{$IFOPT O+} {$DEFINE OPTIMIZATION_ON} {$ENDIF}
{$IFOPT P+} {$DEFINE OPENSTRINGS_ON} {$ENDIF}
{$IFOPT Q+} {$DEFINE OVERFLOWCHECKS_ON} {$ENDIF}
{$IFOPT R+} {$DEFINE RANGECHECKS_ON} {$ENDIF}
//REALCOMPATIBILITY
{$IFOPT T+} {$DEFINE TYPEDADDRESS_ON} {$ENDIF}
{$IFOPT U+} {$DEFINE SAFEDIVIDE_ON} {$ENDIF}
{$IFOPT V+} {$DEFINE VARSTRINGCHECKS_ON} {$ENDIF}
{$IFOPT W+} {$DEFINE STACKFRAMES_ON} {$ENDIF}
//WARNINGS
{$IFOPT X+} {$DEFINE EXTENDEDSYNTAX_ON} {$ENDIF}

//------------------------------------------------------------------------------
// VERXXX to COMPILERX, DELPHIX and BCBX mappings
//------------------------------------------------------------------------------

{$IFDEF VER150}
  {$DEFINE COMPILER7}
  {$IFDEF BCB}
    {$DEFINE BCB7}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI7}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE COMPILER6}
  {$IFDEF BCB}
    {$DEFINE BCB6}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI6}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE COMPILER5}
  {$IFDEF BCB}
    {$DEFINE BCB5}
    {$DEFINE BCB}
  {$ELSE}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE COMPILER4}
  {$DEFINE BCB4}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE COMPILER4}
  {$DEFINE DELPHI4}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE COMPILER35}
  {$DEFINE BCB3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE COMPILER3}
  {$DEFINE DELPHI3}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE COMPILER2}
  {$DEFINE BCB1}
  {$DEFINE BCB}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE COMPILER2}
  {$DEFINE DELPHI2}
  {$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE COMPILER1}
  {$DEFINE DELPHI1}
  {$DEFINE DELPHI}
{$ENDIF}

//------------------------------------------------------------------------------
// DELPHIX_UP from DELPHIX mappings
//------------------------------------------------------------------------------

{$IFDEF DELPHI7}
  {$DEFINE DELPHI7_UP}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI6}
  {$DEFINE DELPHI6_UP}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI5}
  {$DEFINE DELPHI5_UP}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI4}
  {$DEFINE DELPHI4_UP}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI3}
  {$DEFINE DELPHI3_UP}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI2}
  {$DEFINE DELPHI2_UP}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

{$IFDEF DELPHI1}
  {$DEFINE DELPHI1_UP}
{$ENDIF}

//------------------------------------------------------------------------------
// BCBX_UP from BCBX mappings
//------------------------------------------------------------------------------

{$IFDEF BCB7}
  {$DEFINE BCB7_UP}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB6}
  {$DEFINE BCB6_UP}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB5}
  {$DEFINE BCB5_UP}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB4}
  {$DEFINE BCB4_UP}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB3}
  {$DEFINE BCB3_UP}
  {$DEFINE BCB1_UP}
{$ENDIF}

{$IFDEF BCB1}
  {$DEFINE BCB1_UP}
{$ENDIF}

//------------------------------------------------------------------------------
// COMPILERX_UP from COMPILERX mappings
//------------------------------------------------------------------------------


{$IFDEF COMPILER7}
  {$DEFINE COMPILER7_UP}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER6}
  {$DEFINE COMPILER6_UP}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER5}
  {$DEFINE COMPILER5_UP}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER4}
  {$DEFINE COMPILER4_UP}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER35}
  {$DEFINE COMPILER35_UP}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER3}
  {$DEFINE COMPILER3_UP}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER2}
  {$DEFINE COMPILER2_UP}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

{$IFDEF COMPILER1}
  {$DEFINE COMPILER1_UP}
{$ENDIF}

//------------------------------------------------------------------------------
// Map COMPILERX_UP to friendly feature names
//------------------------------------------------------------------------------

{$IFDEF COMPILER3_UP}
  {$DEFINE SUPPORTS_WIDESTRING}
  {$DEFINE SUPPORTS_INTERFACE}
{$ENDIF}

{$IFDEF COMPILER35_UP}
  {$DEFINE SUPPORTS_EXTSYM}
  {$DEFINE SUPPORTS_NODEFINE}
{$ENDIF}

{$IFDEF COMPILER4_UP}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_DYNAMICARRAYS}
  {$DEFINE SUPPORTS_DEFAULTPARAMS}
  {$DEFINE SUPPORTS_OVERLOAD}
{$ENDIF}

//------------------------------------------------------------------------------
// Cross-platform related defines
//------------------------------------------------------------------------------

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
  {$DEFINE COMPLIB_CLX}
{$ENDIF}

{$IFNDEF COMPLIB_CLX}
  {$DEFINE COMPLIB_VCL}
{$ENDIF}

