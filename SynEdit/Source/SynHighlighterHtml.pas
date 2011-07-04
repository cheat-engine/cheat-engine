{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHTML.pas, released 2000-04-10.
The Original Code is based on the hkHTMLSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Hideo Koiso.
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

$Id: SynHighlighterHtml.pas,v 1.25 2005/01/28 16:53:23 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides an HTML highlighter for SynEdit)
@author(Hideo Koiso, converted to SynEdit by Michael Hieke)
@created(1999-11-02, converted to SynEdit 2000-04-10)
@lastmod(2000-06-23)
The SynHighlighterHTML unit provides SynEdit with an HTML highlighter.
}

{$IFNDEF QSYNHIGHLIGHTERHTML}
unit SynHighlighterHtml;
{$ENDIF}

interface

{$I SynEdit.inc}

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

const
  MAX_ESCAPEAMPS = 249;

  EscapeAmps: array[0..MAX_ESCAPEAMPS - 1] of PChar = (
    ('&Alpha;'),         { ?        }  { greek capital alpha }
    ('&Beta;'),          { ?        }  { greek capital beta }
    ('&Gamma;'),         { G        }  { greek capital gamma }
    ('&Delta;'),         { ?        }  { greek capital delta }
    ('&Epsilon;'),       { ?        }  { greek capital epsilon }
    ('&Zeta;'),          { ?        }  { greek capital zeta }
    ('&Eta;'),           { ?        }  { greek capital eta }
    ('&Theta;'),         { T        }  { greek capital theta }
    ('&Iota;'),          { ?        }  { greek capital iota }
    ('&Kappa;'),         { ?        }  { greek capital kappa }
    ('&Lambda;'),        { ?        }  { greek capital lambda }
    ('&Mu;'),            { ?        }  { greek capital mu }
    ('&Nu;'),            { ?        }  { greek capital nu }
    ('&Xi;'),            { ?        }  { greek capital xi }
    ('&Omicron;'),       { ?        }  { greek capital omicron }
    ('&Pi;'),            { ?        }  { greek capital pi }
    ('&Rho;'),           { ?        }  { greek capital rho }
    ('&Sigma;'),         { S        }  { greek capital sigma }
    ('&Tau;'),           { ?        }  { greek capital tau }
    ('&Upsilon;'),       { ?        }  { greek capital upsilon }
    ('&Phi;'),           { F        }  { greek capital phi }
    ('&Chi;'),           { ?        }  { greek capital chi }
    ('&Psi;'),           { ?        }  { greek capital psi }
    ('&Omega;'),         { O        }  { greek capital omega }
    ('&alpha;'),         { a        }  { greek small alpha }
    ('&beta;'),          { ß        }  { greek small beta }
    ('&gamma;'),         { ?        }  { greek small gamma }
    ('&delta;'),         { d        }  { greek small delta }
    ('&epsilon;'),       { e        }  { greek small epsilon }
    ('&zeta;'),          { ?        }  { greek small zeta }
    ('&eta;'),           { ?        }  { greek small eta }
    ('&theta;'),         { ?        }  { greek small theta }
    ('&iota;'),          { ?        }  { greek small iota }
    ('&kappa;'),         { ?        }  { greek small kappa }
    ('&lambda;'),        { ?        }  { greek small lambda }
    ('&mu;'),            { µ        }  { greek small mu }
    ('&nu;'),            { ?        }  { greek small nu }
    ('&xi;'),            { ?        }  { greek small xi }
    ('&omicron;'),       { ?        }  { greek small omicron }
    ('&pi;'),            { p        }  { greek small pi }
    ('&rho;'),           { ?        }  { greek small rho }
    ('&sigmaf;'),        { ?        }  { greek small final sigma }
    ('&sigma;'),         { s        }  { greek small sigma }
    ('&tau;'),           { t        }  { greek small tau }
    ('&upsilon;'),       { ?        }  { greek small upsilon }
    ('&phi;'),           { f        }  { greek small phi }
    ('&chi;'),           { ?        }  { greek small chi }
    ('&psi;'),           { ?        }  { greek small psi }
    ('&omega;'),         { ?        }  { greek small omega }
    ('&thetasym;'),      { ?        }  { greek small theta symbol }
    ('&upsih;'),         { ?        }  { greek upsilon with hook symbol }
    ('&piv;'),           { ?        }  { greek pi symbol }
    ('&bull;'),          { •        }  { bullet }
    ('&hellip;'),        { …        }  { horizontal ellipsis }
    ('&prime;'),         { '        }  { prime }
    ('&Prime;'),         { "        }  { double prime }
    ('&oline;'),         { ?        }  { overline, = spacing overscore }
    ('&frasl;'),         { /        }  { fraction slash }
    ('&weierp;'),        { P        }  { script capital P }
    ('&image;'),         { I        }  { imaginary part }
    ('&real;'),          { R        }  { real part }
    ('&trade;'),         { ™        }  { trademark sign }
    ('&alefsym;'),       { ?        }  { first transfinite cardinal }
    ('&larr;'),          { ?        }  { leftwards arrow }
    ('&uarr;'),          { ?        }  { upwards arrow }
    ('&rarr;'),          { ?        }  { rightwards arrow }
    ('&darr;'),          { ?        }  { downwards arrow }
    ('&harr;'),          { ?        }  { left right arrow }
    ('&crarr;'),         { ?        }  { carriage return arrow }
    ('&lArr;'),          { ?        }  { leftwards double arrow }
    ('&uArr;'),          { ?        }  { upwards double arrow }
    ('&rArr;'),          { ?        }  { rightwards double arrow }
    ('&dArr;'),          { ?        }  { downwards double arrow }
    ('&hArr;'),          { ?        }  { left right double arrow }
    ('&forall;'),        { ?        }  { for all }
    ('&part;'),          { ?        }  { partial differential }
    ('&exist;'),         { ?        }  { there exists }
    ('&empty;'),         { Ø        }  { empty set }
    ('&nabla;'),         { ?        }  { backward difference }
    ('&isin;'),          { ?        }  { element of }
    ('&notin;'),         { ?        }  { not an element of }
    ('&ni;'),            { ?        }  { contains as member }
    ('&prod;'),          { ?        }  { n-ary product }
    ('&sum;'),           { ?        }  { n-ary sumation }
    ('&minus;'),         { -        }  { minus sign }
    ('&lowast;'),        { *        }  { asterisk operator }
    ('&radic;'),         { v        }  { square root }
    ('&prop;'),          { ?        }  { proportional to }
    ('&infin;'),         { 8        }  { infinity }
    ('&ang;'),           { ?        }  { angle }
    ('&and;'),           { ?        }  { logical and }
    ('&or;'),            { ?        }  { logical or }
    ('&cap;'),           { n        }  { intersection }
    ('&cup;'),           { ?        }  { union }
    ('&int;'),           { ?        }  { integral }
    ('&there4;'),        { ?        }  { therefore }
    ('&sim;'),           { ~        }  { similar to = tilde operator }
    ('&cong;'),          { ?        }  { approximately equal to }
    ('&asymp;'),         { ˜        }  { almost euqal to }
    ('&ne;'),            { ?        }  { not equal to }
    ('&equiv;'),         { =        }  { identical to }
    ('&le;'),            { =        }  { less-than or equal to }
    ('&ge;'),            { =        }  { greater-than or equal to }
    ('&sub;'),           { ?        }  { subset of }
    ('&sup;'),           { ?        }  { superset of }
    ('&nsub;'),          { ?        }  { not a subset of }
    ('&sube;'),          { ?        }  { subset of or equal to }
    ('&supe;'),          { ?        }  { superset of or equal to }
    ('&oplus;'),         { ?        }  { circled plus }
    ('&otimes;'),        { ?        }  { circled times }
    ('&perp;'),          { ?        }  { orthogonal to = perpendicular }
    ('&sdot;'),          { ·        }  { dot operator }
    ('&lceil;'),         { ?        }  { left ceiling }
    ('&rceil;'),         { ?        }  { right ceiling }
    ('&lfloor;'),        { ?        }  { left floor }
    ('&rfloor;'),        { ?        }  { right floor }
    ('&lang;'),          { <        }  { left-pointing angle bracket }
    ('&rang;'),          { >        }  { right-pointing angle bracket }
    ('&loz;'),           { ?        }  { lozenge }
    ('&spades;'),        { ?        }  { black spade suit }
    ('&clubs;'),         { ?        }  { black club suit }
    ('&hearts;'),        { ?        }  { black heart suit }
    ('&diams;'),         { ?        }  { black diamond suit }
    ('&lsquo;'),         { ‘        }  { left single quote  }
    ('&rsquo;'),         { ’        }  { right single quote }
    ('&sbquo;'),         { ‚        }  { single low-9 quote }
    ('&ldquo;'),         { “        }  { left double quote }
    ('&rdquo;'),         { ”        }  { right double quote }
    ('&bdquo;'),         { „        }  { double low-9 quote }
    ('&dagger;'),        { †        }  { dagger }
    ('&Dagger;'),        { ‡        }  { double dagger }
    ('&permil;'),        { ‰        }  { per mill sign }
    ('&lsaquo;'),        { ‹        }  { single left-pointing angle quote }
    ('&rsaquo;'),        { ›        }  { single right-pointing angle quote }
    ('&quot;'),          { &#034; " }  { double quotation mark }
    ('&amp;'),           { &#038; & }  { ampersand }
    ('&lt;'),            { &#060; < }  { less-than sign }
    ('&gt;'),            { >        }  { greater-than sign }
    ('&ndash;'),         { &#150; – }  { en dash }
    ('&mdash;'),         { &#151; — }  { em dash }
    ('&nbsp;'),          { &#160;   }  { nonbreaking space }
    ('&thinsp;'),        {          }  { thin space }
    ('&ensp;'),          {          }  { en space }
    ('&emsp;'),          {          }  { em space }
    ('&iexcl;'),         { &#161; ! }  { inverted exclamation }
    ('&cent;'),          { &#162; c }  { cent sign }
    ('&pound;'),         { &#163; L }  { pound sterling }
    ('&curren;'),        { &#164; ¤ }  { general currency sign }
    ('&yen;'),           { &#165; Y }  { yen sign }
    ('&brvbar;'),        { &#166; ¦ }  { broken vertical bar }
    ('&brkbar;'),        { &#166; ¦ }  { broken vertical bar }
    ('&sect;'),          { &#167; § }  { section sign }
    ('&uml;'),           { &#168; ¨ }  { umlaut }
    ('&die;'),           { &#168; ¨ }  { umlaut }
    ('&copy;'),          { &#169; © }  { copyright }
    ('&ordf;'),          { &#170; a }  { feminine ordinal }
    ('&laquo;'),         { &#171; « }  { left angle quote }
    ('&not;'),           { &#172; ¬ }  { not sign }
    ('&shy;'),           { &#173; ­ }  { soft hyphen }
    ('&reg;'),           { &#174; ® }  { registered trademark }
    ('&macr;'),          { &#175; — }  { macron accent }
    ('&hibar;'),         { &#175; — }  { macron accent }
    ('&deg;'),           { &#176; ° }  { degree sign }
    ('&plusmn;'),        { &#177; ± }  { plus or minus }
    ('&sup2;'),          { &#178; 2 }  { superscript two }
    ('&sup3;'),          { &#179; 3 }  { superscript three }
    ('&acute;'),         { &#180; ´ }  { acute accent }
    ('&micro;'),         { &#181; µ }  { micro sign }
    ('&para;'),          { &#182; ¶ }  { paragraph sign }
    ('&middot;'),        { &#183; · }  { middle dot }
    ('&cedil;'),         { &#184; ¸ }  { cedilla }
    ('&sup1;'),          { &#185; 1 }  { superscript one }
    ('&ordm;'),          { &#186; o }  { masculine ordinal }
    ('&raquo;'),         { &#187; » }  { right angle quote }
    ('&frac14;'),        { &#188; 1 }  { one-fourth }
    ('&frac12;'),        { &#189; 1 }  { one-half }
    ('&frac34;'),        { &#190; 3 }  { three-fourths }
    ('&iquest;'),        { &#191; ? }  { inverted question mark }
    ('&Agrave;'),        { &#192; A }  { uppercase A, grave accent }
    ('&Aacute;'),        { &#193; Á }  { uppercase A, acute accent }
    ('&Acirc;'),         { &#194; Â }  { uppercase A, circumflex accent }
    ('&Atilde;'),        { &#195; A }  { uppercase A, tilde }
    ('&Auml;'),          { &#196; Ä }  { uppercase A, umlaut }
    ('&Aring;'),         { &#197; A }  { uppercase A, ring }
    ('&AElig;'),         { &#198; A }  { uppercase AE }
    ('&Ccedil;'),        { &#199; Ç }  { uppercase C, cedilla }
    ('&Egrave;'),        { &#200; E }  { uppercase E, grave accent }
    ('&Eacute;'),        { &#201; É }  { uppercase E, acute accent }
    ('&Ecirc;'),         { &#202; E }  { uppercase E, circumflex accent }
    ('&Euml;'),          { &#203; Ë }  { uppercase E, umlaut }
    ('&Igrave;'),        { &#204; I }  { uppercase I, grave accent }
    ('&Iacute;'),        { &#205; Í }  { uppercase I, acute accent }
    ('&Icirc;'),         { &#206; Î }  { uppercase I, circumflex accent }
    ('&Iuml;'),          { &#207; I }  { uppercase I, umlaut }
    ('&ETH;'),           { &#208; ? }  { uppercase Eth, Icelandic }
    ('&Ntilde;'),        { &#209; N }  { uppercase N, tilde }
    ('&Ograve;'),        { &#210; O }  { uppercase O, grave accent }
    ('&Oacute;'),        { &#211; Ó }  { uppercase O, acute accent }
    ('&Ocirc;'),         { &#212; Ô }  { uppercase O, circumflex accent }
    ('&Otilde;'),        { &#213; O }  { uppercase O, tilde }
    ('&Ouml;'),          { &#214; Ö }  { uppercase O, umlaut }
    ('&times;'),         { &#215; × }  { multiplication sign }
    ('&Oslash;'),        { &#216; O }  { uppercase O, slash }
    ('&Ugrave;'),        { &#217; U }  { uppercase U, grave accent }
    ('&Uacute;'),        { &#218; Ú }  { uppercase U, acute accent }
    ('&Ucirc;'),         { &#219; U }  { uppercase U, circumflex accent }
    ('&Uuml;'),          { &#220; Ü }  { uppercase U, umlaut }
    ('&Yacute;'),        { &#221; Ý }  { uppercase Y, acute accent }
    ('&THORN;'),         { &#222; ? }  { uppercase THORN, Icelandic }
    ('&szlig;'),         { &#223; ß }  { lowercase sharps, German }
    ('&agrave;'),        { &#224; à }  { lowercase a, grave accent }
    ('&aacute;'),        { &#225; á }  { lowercase a, acute accent }
    ('&acirc;'),         { &#226; â }  { lowercase a, circumflex accent }
    ('&atilde;'),        { &#227; ã }  { lowercase a, tilde }
    ('&auml;'),          { &#228; ä }  { lowercase a, umlaut }
    ('&aring;'),         { &#229; å }  { lowercase a, ring }
    ('&aelig;'),         { &#230; a }  { lowercase ae }
    ('&ccedil;'),        { &#231; ç }  { lowercase c, cedilla }
    ('&egrave;'),        { &#232; e }  { lowercase e, grave accent }
    ('&eacute;'),        { &#233; é }  { lowercase e, acute accent }
    ('&ecirc;'),         { &#234; ê }  { lowercase e, circumflex accent }
    ('&euml;'),          { &#235; ë }  { lowercase e, umlaut }
    ('&igrave;'),        { &#236; i }  { lowercase i, grave accent }
    ('&iacute;'),        { &#237; í }  { lowercase i, acute accent }
    ('&icirc;'),         { &#238; î }  { lowercase i, circumflex accent }
    ('&iuml;'),          { &#239; i }  { lowercase i, umlaut }
    ('&eth;'),           { &#240; ? }  { lowercase eth, Icelandic }
    ('&ntilde;'),        { &#241; ñ }  { lowercase n, tilde }
    ('&ograve;'),        { &#242; o }  { lowercase o, grave accent }
    ('&oacute;'),        { &#243; ó }  { lowercase o, acute accent }
    ('&ocirc;'),         { &#244; ô }  { lowercase o, circumflex accent }
    ('&otilde;'),        { &#245; o }  { lowercase o, tilde }
    ('&ouml;'),          { &#246; ö }  { lowercase o, umlaut }
    ('&divide;'),        { &#247; ÷ }  { division sign }
    ('&oslash;'),        { &#248; o }  { lowercase o, slash }
    ('&ugrave;'),        { &#249; u }  { lowercase u, grave accent }
    ('&uacute;'),        { &#250; ú }  { lowercase u, acute accent }
    ('&ucirc;'),         { &#251; u }  { lowercase u, circumflex accent }
    ('&uuml;'),          { &#252; ü }  { lowercase u, umlaut }
    ('&yacute;'),        { &#253; ý }  { lowercase y, acute accent }
    ('&thorn;'),         { &#254; ? }  { lowercase thorn, Icelandic }
    ('&yuml;'),          { &#255; y }  { lowercase y, umlaut }
    ('&euro;'),          { €        }  { euro sign }
    ('&OElig;'),         { Œ        }  { capital ligature OE }
    ('&oelig;'),         { œ        }  { small ligature oe }
    ('&scaron;'),        { š        }  { small S with caron }
    ('&Scaron;'),        { Š        }  { capital S with caron }
    ('&fnof;'),          { ƒ        }  { function }
    ('&circ;')           { ˆ        }  { circumflex accent }
  );


type
  TtkTokenKind = (tkAmpersand, tkComment, tkIdentifier, tkKey, tkNull,
    tkSpace, tkSymbol, tkText, tkUndefKey, tkValue);

  TRangeState = (rsAmpersand, rsComment, rsKey, rsParam, rsText,
    rsUnKnown, rsValue, rsQuoteValue, rsDoubleQuoteValue);

  TProcTableProc = procedure of object;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

  TSynHTMLSyn = class(TSynCustomHighlighter)
  private
    fAndCode: Integer;
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: Longint;
    Temp: PChar;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..250] of TIdentFuncTableFunc;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fAndAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fUndefKeyAttri: TSynHighlighterAttributes;
    fValueAttri: TSynHighlighterAttributes;
    fLineNumber: Integer;

    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func1: TtkTokenKind;
    function Func2: TtkTokenKind;
    function Func8: TtkTokenKind;
    function Func9: TtkTokenKind;
    function Func10: TtkTokenKind;
    function Func11: TtkTokenKind;
    function Func12: TtkTokenKind;
    function Func13: TtkTokenKind;
    function Func14: TtkTokenKind;
    function Func16: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func123: TtkTokenKind;
    function Func124: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func132: TtkTokenKind;
    function Func133: TtkTokenKind;
    function Func134: TtkTokenKind;
    function Func135: TtkTokenKind;
    function Func136: TtkTokenKind;
    function Func138: TtkTokenKind;
    function Func139: TtkTokenKind;
    function Func140: TtkTokenKind;
    function Func141: TtkTokenKind;
    function Func143: TtkTokenKind;
    function Func145: TtkTokenKind;
    function Func146: TtkTokenKind;
    function Func149: TtkTokenKind;
    function Func150: TtkTokenKind;
    function Func151: TtkTokenKind;
    function Func152: TtkTokenKind;
    function Func153: TtkTokenKind;
    function Func154: TtkTokenKind;
    function Func155: TtkTokenKind;
    function Func157: TtkTokenKind;
    function Func159: TtkTokenKind;
    function Func160: TtkTokenKind;
    function Func161: TtkTokenKind;
    function Func162: TtkTokenKind;
    function Func163: TtkTokenKind;
    function Func164: TtkTokenKind;
    function Func168: TtkTokenKind;
    function Func169: TtkTokenKind;
    function Func170: TtkTokenKind;
    function Func171: TtkTokenKind;
    function Func172: TtkTokenKind;
    function Func174: TtkTokenKind;
    function Func175: TtkTokenKind;
    function Func177: TtkTokenKind;
    function Func178: TtkTokenKind;
    function Func179: TtkTokenKind;
    function Func180: TtkTokenKind;
    function Func183: TtkTokenKind;
    function Func186: TtkTokenKind;
    function Func187: TtkTokenKind;
    function Func188: TtkTokenKind;
    function Func192: TtkTokenKind;
    function Func198: TtkTokenKind;
    function Func200: TtkTokenKind;
    function Func202: TtkTokenKind;
    function Func203: TtkTokenKind;
    function Func204: TtkTokenKind;
    function Func205: TtkTokenKind;
    function Func207: TtkTokenKind;
    function Func209: TtkTokenKind;
    function Func211: TtkTokenKind;
    function Func212: TtkTokenKind;
    function Func213: TtkTokenKind;
    function Func214: TtkTokenKind;
    function Func215: TtkTokenKind;
    function Func216: TtkTokenKind;
    function Func227: TtkTokenKind;
    function Func229: TtkTokenKind;
    function Func236: TtkTokenKind;
    function Func243: TtkTokenKind;
    function Func250: TtkTokenKind;
    function AltFunc: TtkTokenKind;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure InitIdent;
    procedure MakeMethodTables;
    procedure TextProc;
    procedure CommentProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure EqualProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure AmpersandProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource : String; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: string; LineNumber:Integer); override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    property IdentChars;
  published
    property AndAttri: TSynHighlighterAttributes read fAndAttri write fAndAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri
      write fTextAttri;
    property UndefKeyAttri: TSynHighlighterAttributes read fUndefKeyAttri
      write fUndefKeyAttri;
    property ValueAttri: TSynHighlighterAttributes read fValueAttri
      write fValueAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

var
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  i: Char;
begin
  for i := #0 to #255 do
    case i of
      'a'..'z', 'A'..'Z':
        mHashTable[i] := (Ord(UpCase(i)) - 64);
      '!':
        mHashTable[i] := $7B;
      '/':
        mHashTable[i] := $7A;
      else
        mHashTable[Char(i)] := 0;
    end;
end;

procedure TSynHTMLSyn.InitIdent;
var
  i: Integer;
begin
  for i := 0 to 250 do
    case i of
      1:   fIdentFuncTable[i] := Func1;
      2:   fIdentFuncTable[i] := Func2;
      8:   fIdentFuncTable[i] := Func8;
      9:   fIdentFuncTable[i] := Func9;
      10:  fIdentFuncTable[i] := Func10;
      11:  fIdentFuncTable[i] := Func11;
      12:  fIdentFuncTable[i] := Func12;
      13:  fIdentFuncTable[i] := Func13;
      14:  fIdentFuncTable[i] := Func14;
      16:  fIdentFuncTable[i] := Func16;
      17:  fIdentFuncTable[i] := Func17;
      18:  fIdentFuncTable[i] := Func18;
      19:  fIdentFuncTable[i] := Func19;
      20:  fIdentFuncTable[i] := Func20;
      21:  fIdentFuncTable[i] := Func21;
      23:  fIdentFuncTable[i] := Func23;
      24:  fIdentFuncTable[i] := Func24;
      25:  fIdentFuncTable[i] := Func25;
      26:  fIdentFuncTable[i] := Func26;
      27:  fIdentFuncTable[i] := Func27;
      28:  fIdentFuncTable[i] := Func28;
      29:  fIdentFuncTable[i] := Func29;
      30:  fIdentFuncTable[i] := Func30;
      31:  fIdentFuncTable[i] := Func31;
      32:  fIdentFuncTable[i] := Func32;
      33:  fIdentFuncTable[i] := Func33;
      35:  fIdentFuncTable[i] := Func35;
      37:  fIdentFuncTable[i] := Func37;
      38:  fIdentFuncTable[i] := Func38;
      39:  fIdentFuncTable[i] := Func39;
      40:  fIdentFuncTable[i] := Func40;
      41:  fIdentFuncTable[i] := Func41;
      42:  fIdentFuncTable[i] := Func42;
      43:  fIdentFuncTable[i] := Func43;
      46:  fIdentFuncTable[i] := Func46;
      47:  fIdentFuncTable[i] := Func47;
      48:  fIdentFuncTable[i] := Func48;
      49:  fIdentFuncTable[i] := Func49;
      50:  fIdentFuncTable[i] := Func50;
      52:  fIdentFuncTable[i] := Func52;
      53:  fIdentFuncTable[i] := Func53;
      55:  fIdentFuncTable[i] := Func55;
      56:  fIdentFuncTable[i] := Func56;
      57:  fIdentFuncTable[i] := Func57;
      58:  fIdentFuncTable[i] := Func58;
      61:  fIdentFuncTable[i] := Func61;
      62:  fIdentFuncTable[i] := Func62;
      64:  fIdentFuncTable[i] := Func64;
      65:  fIdentFuncTable[i] := Func65;
      66:  fIdentFuncTable[i] := Func66;
      67:  fIdentFuncTable[i] := Func67;
      70:  fIdentFuncTable[i] := Func70;
      76:  fIdentFuncTable[i] := Func76;
      78:  fIdentFuncTable[i] := Func78;
      80:  fIdentFuncTable[i] := Func80;
      81:  fIdentFuncTable[i] := Func81;
      82:  fIdentFuncTable[i] := Func82;
      83:  fIdentFuncTable[i] := Func83;
      84:  fIdentFuncTable[i] := Func84;
      85:  fIdentFuncTable[i] := Func85;
      87:  fIdentFuncTable[i] := Func87;
      89:  fIdentFuncTable[i] := Func89;
      90:  fIdentFuncTable[i] := Func90;
      91:  fIdentFuncTable[i] := Func91;
      92:  fIdentFuncTable[i] := Func92;
      93:  fIdentFuncTable[i] := Func93;
      94:  fIdentFuncTable[i] := Func94;
      105: fIdentFuncTable[i] := Func105;
      107: fIdentFuncTable[i] := Func107;
      114: fIdentFuncTable[i] := Func114;
      121: fIdentFuncTable[i] := Func121;
      123: fIdentFuncTable[i] := Func123;
      124: fIdentFuncTable[i] := Func124;
      128: fIdentFuncTable[i] := Func128;
      130: fIdentFuncTable[i] := Func130;
      131: fIdentFuncTable[i] := Func131;
      132: fIdentFuncTable[i] := Func132;
      133: fIdentFuncTable[i] := Func133;
      134: fIdentFuncTable[i] := Func134;
      135: fIdentFuncTable[i] := Func135;
      136: fIdentFuncTable[i] := Func136;
      138: fIdentFuncTable[i] := Func138;
      139: fIdentFuncTable[i] := Func139;
      140: fIdentFuncTable[i] := Func140;
      141: fIdentFuncTable[i] := Func141;
      143: fIdentFuncTable[i] := Func143;
      145: fIdentFuncTable[i] := Func145;
      146: fIdentFuncTable[i] := Func146;
      149: fIdentFuncTable[i] := Func149;
      150: fIdentFuncTable[i] := Func150;
      151: fIdentFuncTable[i] := Func151;
      152: fIdentFuncTable[i] := Func152;
      153: fIdentFuncTable[i] := Func153;
      154: fIdentFuncTable[i] := Func154;
      155: fIdentFuncTable[i] := Func155;
      157: fIdentFuncTable[i] := Func157;
      159: fIdentFuncTable[i] := Func159;
      160: fIdentFuncTable[i] := Func160;
      161: fIdentFuncTable[i] := Func161;
      162: fIdentFuncTable[i] := Func162;
      163: fIdentFuncTable[i] := Func163;
      164: fIdentFuncTable[i] := Func164;
      168: fIdentFuncTable[i] := Func168;
      169: fIdentFuncTable[i] := Func169;
      170: fIdentFuncTable[i] := Func170;
      171: fIdentFuncTable[i] := Func171;
      172: fIdentFuncTable[i] := Func172;
      174: fIdentFuncTable[i] := Func174;
      175: fIdentFuncTable[i] := Func175;
      177: fIdentFuncTable[i] := Func177;
      178: fIdentFuncTable[i] := Func178;
      179: fIdentFuncTable[i] := Func179;
      180: fIdentFuncTable[i] := Func180;
      183: fIdentFuncTable[i] := Func183;
      186: fIdentFuncTable[i] := Func186;
      187: fIdentFuncTable[i] := Func187;
      188: fIdentFuncTable[i] := Func188;
      192: fIdentFuncTable[i] := Func192;
      198: fIdentFuncTable[i] := Func198;
      200: fIdentFuncTable[i] := Func200;
      202: fIdentFuncTable[i] := Func202;
      203: fIdentFuncTable[i] := Func203;
      204: fIdentFuncTable[i] := Func204;
      205: fIdentFuncTable[i] := Func205;
      207: fIdentFuncTable[i] := Func207;
      209: fIdentFuncTable[i] := Func209;
      211: fIdentFuncTable[i] := Func211;
      212: fIdentFuncTable[i] := Func212;
      213: fIdentFuncTable[i] := Func213;
      214: fIdentFuncTable[i] := Func214;
      215: fIdentFuncTable[i] := Func215;
      216: fIdentFuncTable[i] := Func216;
      227: fIdentFuncTable[i] := Func227;
      229: fIdentFuncTable[i] := Func229;
      236: fIdentFuncTable[i] := Func236;
      243: fIdentFuncTable[i] := Func243;
      250: fIdentFuncTable[i] := Func250;
      else fIdentFuncTable[i] := AltFunc;
    end;
end;

function TSynHTMLSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  While (ToHash^ In ['a'..'z', 'A'..'Z', '!', '/']) do begin
    Inc(Result, mHashTable[ToHash^]);
    Inc(ToHash);
  end;
  While (ToHash^ In ['0'..'9']) do begin
    Inc(Result, (Ord(ToHash^) - Ord('0')) );
    Inc(ToHash);
  end;
  fStringLen := (ToHash - fToIdent);
end;

function TSynHTMLSyn.KeyComp(const aKey: string): Boolean;
var
  i: Integer;
begin
  Temp := fToIdent;
  if (Length(aKey) = fStringLen) then begin
    Result := True;
    For i:=1 To fStringLen do begin
      if (mHashTable[Temp^] <> mHashTable[aKey[i]]) then begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end else begin
    Result := False;
  end;
end;

function TSynHTMLSyn.Func1: TtkTokenKind;
begin
  if KeyComp('A') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func2: TtkTokenKind;
begin
  if KeyComp('B') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func8: TtkTokenKind;
begin
  if KeyComp('DD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func9: TtkTokenKind;
begin
  if KeyComp('I') Or KeyComp('H1') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func10: TtkTokenKind;
begin
  if KeyComp('H2') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func11: TtkTokenKind;
begin
  if KeyComp('H3') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func12: TtkTokenKind;
begin
  if KeyComp('H4') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func13: TtkTokenKind;
begin
  if KeyComp('H5') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func14: TtkTokenKind;
begin
  if KeyComp('H6') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func16: TtkTokenKind;
begin
  if KeyComp('DL') Or KeyComp('P') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func17: TtkTokenKind;
begin
  if KeyComp('KBD') Or KeyComp('Q') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func18: TtkTokenKind;
begin
  if KeyComp('BIG') Or KeyComp('EM') Or KeyComp('HEAD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func19: TtkTokenKind;
begin
  if KeyComp('S') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func20: TtkTokenKind;
begin
  if KeyComp('BR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func21: TtkTokenKind;
begin
  if KeyComp('DEL') Or KeyComp('LI') Or KeyComp('U') Or KeyComp('BDO') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func23: TtkTokenKind;
begin
  if KeyComp('ABBR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func24: TtkTokenKind;
begin
  if KeyComp('DFN') Or KeyComp('DT') Or KeyComp('TD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func25: TtkTokenKind;
begin
  if KeyComp('AREA') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func26: TtkTokenKind;
begin
  if KeyComp('HR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func27: TtkTokenKind;
begin
  if KeyComp('BASE') Or KeyComp('CODE') Or KeyComp('OL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func28: TtkTokenKind;
begin
  if KeyComp('TH') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func29: TtkTokenKind;
begin
  if KeyComp('EMBED') Or KeyComp('IMG') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func30: TtkTokenKind;
begin
  if KeyComp('COL') Or KeyComp('MAP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func31: TtkTokenKind;
begin
  if KeyComp('DIR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func32: TtkTokenKind;
begin
  if KeyComp('LABEL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func33: TtkTokenKind;
begin
  if KeyComp('UL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func35: TtkTokenKind;
begin
  if KeyComp('DIV') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func37: TtkTokenKind;
begin
  if KeyComp('CITE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func38: TtkTokenKind;
begin
  if KeyComp('THEAD') Or KeyComp('TR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func39: TtkTokenKind;
begin
  if KeyComp('META') Or KeyComp('PRE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func40: TtkTokenKind;
begin
  if KeyComp('TABLE') Or KeyComp('TT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func41: TtkTokenKind;
begin
  if KeyComp('var') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func42: TtkTokenKind;
begin
  if KeyComp('INS') Or KeyComp('SUB') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func43: TtkTokenKind;
begin
  if KeyComp('FRAME') Or KeyComp('WBR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func46: TtkTokenKind;
begin
  if KeyComp('BODY') Or KeyComp('LINK') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func47: TtkTokenKind;
begin
  if KeyComp('LEGend') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func48: TtkTokenKind;
begin
  if KeyComp('BLINK') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func49: TtkTokenKind;
begin
  if KeyComp('NOBR') Or KeyComp('PARAM') Or KeyComp('SAMP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func50: TtkTokenKind;
begin
  if KeyComp('SPAN') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func52: TtkTokenKind;
begin
  if KeyComp('FORM') Or KeyComp('IFRAME') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func53: TtkTokenKind;
begin
  if KeyComp('HTML') Or KeyComp('MENU') Or KeyComp('XMP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func55: TtkTokenKind;
begin
  if KeyComp('FONT') Or KeyComp('object') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func56: TtkTokenKind;
begin
  if KeyComp('SUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func57: TtkTokenKind;
begin
  if KeyComp('SMALL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func58: TtkTokenKind;
begin
  if KeyComp('NOEMBED') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func61: TtkTokenKind;
begin
  if KeyComp('LAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func62: TtkTokenKind;
begin
  if KeyComp('SPACER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func64: TtkTokenKind;
begin
  if KeyComp('SELECT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func65: TtkTokenKind;
begin
  if KeyComp('CENTER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func66: TtkTokenKind;
begin
  if KeyComp('TBODY') Or KeyComp('TITLE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func67: TtkTokenKind;
begin
  if KeyComp('KEYGEN') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func70: TtkTokenKind;
begin
  if KeyComp('ADDRESS') Or KeyComp('APPLET') Or KeyComp('ILAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func76: TtkTokenKind;
begin
  if KeyComp('NEXTID') Or KeyComp('TFOOT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func78: TtkTokenKind;
begin
  if KeyComp('CAPTION') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func80: TtkTokenKind;
begin
  if KeyComp('FIELDSET') Or KeyComp('INPUT') Or KeyComp('MARQUEE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func81: TtkTokenKind;
begin
  if KeyComp('STYLE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func82: TtkTokenKind;
begin
  if KeyComp('BASEFONT') Or KeyComp('BGSOUND') Or KeyComp('STRIKE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func83: TtkTokenKind;
begin
  if KeyComp('COMMENT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func84: TtkTokenKind;
begin
  if KeyComp('ISINDEX') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func85: TtkTokenKind;
begin
  if KeyComp('SCRIPT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func87: TtkTokenKind;
begin
  if KeyComp('SERVER') Or KeyComp('FRAMESET') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func89: TtkTokenKind;
begin
  if KeyComp('ACRONYM') Or KeyComp('OPTION') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func90: TtkTokenKind;
begin
  if KeyComp('LISTING') Or KeyComp('NOLAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func91: TtkTokenKind;
begin
  if KeyComp('NOFRAMES') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func92: TtkTokenKind;
begin
  if KeyComp('BUTTON') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func93: TtkTokenKind;
begin
  if KeyComp('STRONG') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func94: TtkTokenKind;
begin
  if KeyComp('TEXTAREA') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func105: TtkTokenKind;
begin
  if KeyComp('MULTICOL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func107: TtkTokenKind;
begin
  if KeyComp('COLGROUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func114: TtkTokenKind;
begin
  if KeyComp('NOSCRIPT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func121: TtkTokenKind;
begin
  if KeyComp('BLOCKQUOTE') Or KeyComp('PLAINTEXT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func123: TtkTokenKind;
begin
  if KeyComp('/A') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func124: TtkTokenKind;
begin
  if KeyComp('/B') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func128: TtkTokenKind;
begin
  if KeyComp('OPTGROUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func130: TtkTokenKind;
begin
  if KeyComp('/DD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func131: TtkTokenKind;
begin
  if KeyComp('/I') Or KeyComp('/H1') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func132: TtkTokenKind;
begin
  if KeyComp('/H2') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func133: TtkTokenKind;
begin
  if KeyComp('/H3') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func134: TtkTokenKind;
begin
  if KeyComp('/H4') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func135: TtkTokenKind;
begin
  if KeyComp('/H5') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func136: TtkTokenKind;
begin
  if KeyComp('/H6') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func138: TtkTokenKind;
begin
  if KeyComp('/DL') Or KeyComp('/P') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func139: TtkTokenKind;
begin
  if KeyComp('/KBD') Or KeyComp('/Q') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func140: TtkTokenKind;
begin
  if KeyComp('/BIG') Or KeyComp('/EM') Or KeyComp('/HEAD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func141: TtkTokenKind;
begin
  if KeyComp('/S') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func143: TtkTokenKind;
begin
  if KeyComp('/DEL') Or KeyComp('/LI') Or KeyComp('/U') Or KeyComp('/BDO') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func145: TtkTokenKind;
begin
  if KeyComp('/ABBR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func146: TtkTokenKind;
begin
  if KeyComp('/DFN') Or KeyComp('/DT') Or KeyComp('/TD') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func149: TtkTokenKind;
begin
  if KeyComp('/CODE') Or KeyComp('/OL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func150: TtkTokenKind;
begin
  if KeyComp('/TH') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func151: TtkTokenKind;
begin
  if KeyComp('/EMBED') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func152: TtkTokenKind;
begin
  if KeyComp('/MAP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func153: TtkTokenKind;
begin
  if KeyComp('/DIR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func154: TtkTokenKind;
begin
  if KeyComp('/LABEL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func155: TtkTokenKind;
begin
  if KeyComp('/UL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func157: TtkTokenKind;
begin
  if KeyComp('/DIV') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func159: TtkTokenKind;
begin
  if KeyComp('/CITE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func160: TtkTokenKind;
begin
  if KeyComp('/THEAD') Or KeyComp('/TR') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func161: TtkTokenKind;
begin
  if KeyComp('/PRE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func162: TtkTokenKind;
begin
  if KeyComp('/TABLE') Or KeyComp('/TT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func163: TtkTokenKind;
begin
  if KeyComp('/var') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func164: TtkTokenKind;
begin
  if KeyComp('/INS') Or KeyComp('/SUB') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func168: TtkTokenKind;
begin
  if KeyComp('/BODY') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func169: TtkTokenKind;
begin
  if KeyComp('/LEGend') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func170: TtkTokenKind;
begin
  if KeyComp('/BLINK') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func171: TtkTokenKind;
begin
  if KeyComp('/NOBR') Or KeyComp('/SAMP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func172: TtkTokenKind;
begin
  if KeyComp('/SPAN') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func174: TtkTokenKind;
begin
  if KeyComp('/FORM') Or KeyComp('/IFRAME') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func175: TtkTokenKind;
begin
  if KeyComp('/HTML') Or KeyComp('/MENU') Or KeyComp('/XMP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func177: TtkTokenKind;
begin
  if KeyComp('/FONT') Or KeyComp('/object') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func178: TtkTokenKind;
begin
  if KeyComp('/SUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func179: TtkTokenKind;
begin
  if KeyComp('/SMALL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func180: TtkTokenKind;
begin
  if KeyComp('/NOEMBED') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func183: TtkTokenKind;
begin
  if KeyComp('/LAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func186: TtkTokenKind;
begin
  if KeyComp('/SELECT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func187: TtkTokenKind;
begin
  if KeyComp('/CENTER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func188: TtkTokenKind;
begin
  if KeyComp('/TBODY') Or KeyComp('/TITLE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func192: TtkTokenKind;
begin
  if KeyComp('/ADDRESS') Or KeyComp('/APPLET') Or KeyComp('/ILAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func198: TtkTokenKind;
begin
  if KeyComp('/TFOOT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func200: TtkTokenKind;
begin
  if KeyComp('/CAPTION') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func202: TtkTokenKind;
begin
  if KeyComp('/FIELDSET') Or KeyComp('/MARQUEE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func203: TtkTokenKind;
begin
  if KeyComp('/STYLE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func204: TtkTokenKind;
begin
  if KeyComp('/STRIKE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func205: TtkTokenKind;
begin
  if KeyComp('/COMMENT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func207: TtkTokenKind;
begin
  if KeyComp('/SCRIPT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func209: TtkTokenKind;
begin
  if KeyComp('/FRAMESET') Or KeyComp('/SERVER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func211: TtkTokenKind;
begin
  if KeyComp('/ACRONYM') Or KeyComp('/OPTION') Or KeyComp('!DOCTYPE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func212: TtkTokenKind;
begin
  if KeyComp('/LISTING') Or KeyComp('/NOLAYER') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func213: TtkTokenKind;
begin
  if KeyComp('/NOFRAMES') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func214: TtkTokenKind;
begin
  if KeyComp('/BUTTON') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func215: TtkTokenKind;
begin
  if KeyComp('/STRONG') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func216: TtkTokenKind;
begin
  if KeyComp('/TEXTAREA') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func227: TtkTokenKind;
begin
  if KeyComp('/MULTICOL') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func229: TtkTokenKind;
begin
  if KeyComp('/COLGROUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func236: TtkTokenKind;
begin
  if KeyComp('/NOSCRIPT') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func243: TtkTokenKind;
begin
  if KeyComp('/BLOCKQUOTE') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.Func250: TtkTokenKind;
begin
  if KeyComp('/OPTGROUP') then begin
    Result := tkKey;
  end else begin
    Result := tkUndefKey;
  end;
end;

function TSynHTMLSyn.AltFunc: TtkTokenKind;
begin
  Result := tkUndefKey;
end;

procedure TSynHTMLSyn.MakeMethodTables;
var
  i: Char;
begin
  For i:=#0 To #255 do begin
    case i of
    #0:
      begin
        fProcTable[i] := NullProc;
      end;
    #10:
      begin
        fProcTable[i] := LFProc;
      end;
    #13:
      begin
        fProcTable[i] := CRProc;
      end;
    #1..#9, #11, #12, #14..#32:
      begin
        fProcTable[i] := SpaceProc;
      end;
    '&':
      begin
        fProcTable[i] := AmpersandProc;
      end;
    '"', #39:
      begin
        fProcTable[i] := StringProc;
      end;
    '<':
      begin
        fProcTable[i] := BraceOpenProc;
      end;
    '>':
      begin
        fProcTable[i] := BraceCloseProc;
      end;
    '=':
      begin
        fProcTable[i] := EqualProc;
      end;
    else
    fProcTable[i] := IdentProc;
  end;
  end;
end;

constructor TSynHTMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Style := [fsBold];
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := $00ff0080;
  AddAttribute(fKeyAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Style := [fsBold];
  AddAttribute(fSymbolAttri);

  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText);
  AddAttribute(fTextAttri);

  fUndefKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrUnknownWord);
  fUndefKeyAttri.Style := [fsBold];
  fUndefKeyAttri.Foreground := clRed;
  AddAttribute(fUndefKeyAttri);

  fValueAttri := TSynHighlighterAttributes.Create(SYNS_AttrValue);
  fValueAttri.Foreground := $00ff8000;
  AddAttribute(fValueAttri);

  fAndAttri := TSynHighlighterAttributes.Create(SYNS_AttrEscapeAmpersand);
  fAndAttri.Style := [fsBold];
  fAndAttri.Foreground := $0000ff00;
  AddAttribute(fAndAttri);
  SetAttributesOnChange(DefHighlightChange);

  InitIdent;
  MakeMethodTables;
  fRange := rsText;
  fDefaultFilter := SYNS_FilterHTML;
  fAndCode := -1;
end;

procedure TSynHTMLSyn.SetLine(NewValue: string; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynHTMLSyn.BraceCloseProc;
begin
  fRange := rsText;
  fTokenId := tkSymbol;
  Inc(Run);
end;

procedure TSynHTMLSyn.CommentProc;
begin
  fTokenID := tkComment;

  if (fLine[Run] In [#0, #10, #13]) then begin
    fProcTable[fLine[Run]];
    Exit;
  end;

  while not (fLine[Run] in [#0, #10, #13]) do begin
    if (fLine[Run] = '>') and (fLine[Run - 1] = '-') and (fLine[Run - 2] = '-')
    then begin
      fRange := rsText;
      Inc(Run);
      break;
    end;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.BraceOpenProc;
begin
  Inc(Run);
  if (fLine[Run] = '!') and (fLine[Run + 1] = '-') and (fLine[Run + 2] = '-')
  then begin
    fRange := rsComment;
    fTokenID := tkComment;
    Inc(Run, 3);
  end else begin
    fRange := rsKey;
    fTokenID := tkSymbol;
  end;
end;

procedure TSynHTMLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynHTMLSyn.EqualProc;
begin
  fRange := rsValue;
  fTokenID := tkSymbol;
  Inc(Run);
end;

function TSynHTMLSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  hashKey: Integer;
begin
  fToIdent := MayBe;
  hashKey := KeyHash(MayBe);
  if (hashKey < 251) then begin
    Result := fIdentFuncTable[hashKey];
  end else begin
    Result := tkIdentifier;
  end;
end;

procedure TSynHTMLSyn.IdentProc;
begin
  case fRange of
  rsKey:
    begin
      fRange := rsParam;
      fTokenID := IdentKind((fLine + Run));
      Inc(Run, fStringLen);
    end;
  rsValue:
    begin
      fRange := rsParam;
      fTokenID := tkValue;
      repeat
        Inc(Run);
      until (fLine[Run] In [#0..#32, '>']);
    end;
  else
    fTokenID := tkIdentifier;
    repeat
      Inc(Run);
    until (fLine[Run] In [#0..#32, '=', '"', '>']);
  end;
end;

procedure TSynHTMLSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynHTMLSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynHTMLSyn.TextProc;
const StopSet = [#0..#31, '<', '&'];
var
  i: Integer;
begin
  if fLine[Run] in (StopSet - ['&']) then begin
    fProcTable[fLine[Run]];
    exit;
  end;

  fTokenID := tkText;
  While True do begin
    while not (fLine[Run] in StopSet) do Inc(Run);

    if (fLine[Run] = '&') then begin
      if (fLine[Run+1]='#') then begin
        fAndCode:=-1;
        i:=Run;
        inc(Run, 2);
        if fLine[Run] in ['X', 'x'] then
        begin
          inc(Run);
          while (fLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f']) do
            inc(Run);
        end
        else
          while (fLine[Run] in ['0'..'9']) do
            inc(Run);
        if (fLine[Run]=';') then begin
          inc(Run);
          Run:=i;
          fRange := rsAmpersand;
        end;
        BREAK;
      end else begin
      For i:=Low(EscapeAmps) To High(EscapeAmps) do begin
        if (StrLComp((fLine + Run), PChar(EscapeAmps[i]), StrLen(EscapeAmps[i])) = 0) then begin
          fAndCode := i;
          fRange := rsAmpersand;
          Exit;
        end;
      end;
      end;

      Inc(Run);
    end else begin
      Break;
    end;
  end;

end;

procedure TSynHTMLSyn.AmpersandProc;
begin
  if fRange <> rsAmpersand then
  begin
    if fRange = rsKey then
    begin
      Inc( Run );
      fRange := rsText;
      fTokenID := tkText;
    end
    else
      IdentProc;
    Exit;
  end;
  
  case fAndCode of
  Low(EscapeAmps)..High(EscapeAmps):
    begin
      fTokenID := tkAmpersand;
      Inc(Run, StrLen(EscapeAmps[fAndCode]));
    end;
    else begin
      if (fLine[Run+1]='#') then begin
        fAndCode:=-1;
        inc(Run, 2);
        if fLine[Run] in ['X', 'x'] then
        begin
          inc(Run);
          while (fLine[Run] in ['0'..'9', 'A'..'F', 'a'..'f']) do
            inc(Run);
        end
        else
          while (fLine[Run] in ['0'..'9']) do
            inc(Run);
        if (fLine[Run]=';') then begin
          inc(Run);
          fTokenID := tkAmpersand;
        end else
          fTokenID := tkText;
      end;
    end;
  end;
  fAndCode := -1;
  fRange := rsText;
end;

procedure TSynHTMLSyn.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while fLine[Run] <= #32 do begin
    if fLine[Run] in [#0, #9, #10, #13] then break;
    Inc(Run);
  end;
end;

procedure TSynHTMLSyn.StringProc;
var
  iOpenChar: Char;
begin
  case fRange of
    rsQuoteValue: begin
      iOpenChar := #39;
      fTokenID := tkValue;
    end;
    rsDoubleQuoteValue: begin
      iOpenChar := '"';
      fTokenID := tkValue;
    end;
    else begin
      iOpenChar := fLine[Run];
      if fRange = rsValue then begin
        if iOpenChar = '"' then
          fRange := rsDoubleQuoteValue
        else
          fRange := rsQuoteValue;
        fTokenID := tkValue;
      end else
      begin
        IdentProc;
        Exit;
      end;
      Inc( Run ); { jumps over the opening char }
    end;
  end;

  while not( fLine[ Run ] in [#0, #10, #13] ) do begin
    if fLine[ Run ] = iOpenChar then begin
      Inc( Run );  { jumps over the closing char }
      if fRange in [rsDoubleQuoteValue, rsQuoteValue] then
        fRange := rsParam
      else
        fRange := rsText;
      break;
    end;
    Inc( Run );
  end;
end;

procedure TSynHTMLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsText:
      TextProc;
    rsComment:
      CommentProc;
    rsQuoteValue, rsDoubleQuoteValue:
      if fLine[ Run ] in [#0, #10, #13] then
        fProcTable[ fLine[Run] ]
      else
        StringProc;
    else
      fProcTable[fLine[Run]];
  end;
end;

function TSynHTMLSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynHTMLSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynHTMLSyn.GetToken: string;
var
  len: Longint;
begin
  Len := (Run - fTokenPos);
  SetString(Result, (FLine + fTokenPos), len);
end;

function TSynHTMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynHTMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkAmpersand: Result := fAndAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSpace: Result := fSpaceAttri;
    tkSymbol: Result := fSymbolAttri;
    tkText: Result := fTextAttri;
    tkUndefKey: Result := fUndefKeyAttri;
    tkValue: Result := fValueAttri;
    else Result := nil;
  end;
end;

function TSynHTMLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynHTMLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynHTMLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

procedure TSynHTMLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

procedure TSynHTMLSyn.ResetRange;
begin
  fRange:= rsText;
end;

function TSynHTMLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

function TSynHTMLSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterHTML;
end;

class function TSynHTMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangHTML;
end;

function TSynHTMLSyn.GetSampleSource: String;
begin
  Result := '<!-- Syntax highlighting -->'#13#10 +
            #13#10 +
            '<html>'#13#10 +
            '<body bgcolor="red">'#13#10 +
            '  <form name="frmLogin" action="doSomething.asp">'#13#10 +
            '    <input name="user" value=''any'#13#10 +
            '      value''>'#13#10 +
            '  </form>'#13#10 +
            '  <invalid>Sample HTML code &copy; 2001</invalid>'#13#10 +
            '</body>'#13#10 +
            '</html>';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynHTMLSyn);
{$ENDIF}
end.
