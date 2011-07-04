{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is jcl.inc                                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel.                       }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones                                                                                     }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This include file defines various JCL specific defines.                                          }
{ The more generic JCL defines are defined in jcl.inc and the generic defines in the jedi.inc file }
{ which is shared with the JEDI VCL.                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This file is filled by the JCL installer, all the changes made in its content will be lost the   }
{ next time the JCL is installed.                                                                  }
{                                                                                                  }
{**************************************************************************************************}

// $Id: jcl.template.inc 2328 2008-01-29 16:21:28Z outchy $

// Math precision selection, mutually exclusive
{.$DEFINE MATH_EXTENDED_PRECISION} // default
{.$DEFINE MATH_DOUBLE_PRECISION}
{.$DEFINE MATH_SINGLE_PRECISION}


// Math functions takes care of infinites and NaN
{.$DEFINE MATH_EXT_EXTREMEVALUES}


// JclHookExcept support for hooking exceptions from DLLs
{.$DEFINE HOOK_DLL_EXCEPTIONS}


//Threadsafe directive
{.$DEFINE THREADSAFE}


// To exclude obsolete code from compilation, remove the point from the line below
{.$DEFINE DROP_OBSOLETE_CODE}


//Support for JclUnitVersioning.pas)
{.$DEFINE UNITVERSIONING}


// debug sources
// defining these symbols will the debug source to be automatically registered
{.$DEFINE DEBUG_NO_BINARY}
{.$DEFINE DEBUG_NO_TD32}
{.$DEFINE DEBUG_NO_MAP}
{.$DEFINE DEBUG_NO_EXPORTS}
{.$DEFINE DEBUG_NO_SYMBOLS}


// mark EDI units as weak package units (to avoid conflicts with the EDI package)
{.$DEFINE EDI_WEAK_PACKAGE_UNITS}


// PCRE options, mutually exclusive
// IMPORTANT: The static link works only for Delphi 2005 and newer
//            (an internal error is raised on other compilers)
// Only one of the following defines can be defined at a time
//   static link: PCRE_STATICLINK
//   static dll import: PCRE_LINKDLL
//   dynamic dll import: PCRE_LINKONREQUEST

{.$DEFINE PCRE_STATICLINK}
{.$DEFINE PCRE_LINKDLL}
{.$DEFINE PCRE_LINKONREQUEST} // default


// BZIP2 options, mutually exclusive
// IMPORTANT: The static link is still experimental

{.$DEFINE BZIP2_STATICLINK}
{.$DEFINE BZIP2_LINKDLL}
{.$DEFINE BZIP2_LINKONREQUEST} // default


// Unicode options
// insert a replacement character if sequence is corrupted rather than raising an exception
{.$DEFINE UNICODE_SILENT_FAILURE}

// defines resource compression (uncompressed, compressed with ZLib, compressed with BZip2), mutually exclusive
{.$DEFINE UNICODE_RAW_DATA} // default
{.$DEFINE UNICODE_ZLIB_DATA}
{.$DEFINE UNICODE_BZIP2_DATA}


// container options
// define mapping of TJclStr* containers to TJclAnsiStr* or TJclWideStr* (mutually exclusive)
{.$DEFINE CONTAINER_ANSISTR} // default
{.$DEFINE CONTAINER_WIDESTR}
{.$DEFINE CONTAINER_NOSTR}


// 7Zip options, mutually exclusive
// IMPORTANT: The static link is not supported yet

{.$DEFINE 7ZIP_STATICLINK} // not supported yet
{.$DEFINE 7ZIP_LINKDLL}
{.$DEFINE 7ZIP_LINKONREQUEST} // default

