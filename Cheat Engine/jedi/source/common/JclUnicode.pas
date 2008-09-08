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
{ The Original Code is JclUnicode.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Mike Lischke (public att lischke-online dott de).  }
{ Portions created by Mike Lischke are Copyright (C) 1999-2000 Mike Lischke. All Rights Reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Andreas Hausladen (ahuser)                                                                     }
{   Mike Lischke                                                                                   }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Peter Schraut (http://www.console-dev.de)                                                      }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various Unicode related routines                                                                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-02-09 23:39:21 +0100 (sam., 09 févr. 2008)                        $ }
{ Revision:      $Rev:: 2352                                                                     $ }
{ Author:        $Author:: marcovtje                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclUnicode;

{$I jcl.inc}

// Copyright (c) 1999-2000 Mike Lischke (public att lischke-online dott de)
//

// 10-JUL-2005: (changes by Peter Schraut)
//   - added CodeBlockName, returns the blockname as string
//   - added CodeBlockRange, returns the range of the specified codeblock
//   - updated TUnicodeBlock to reflect changes in unicode 4.1
//   - updated CodeBlockFromChar to reflect changes in unicode 4.1
//   - Notes:
//      Here are a few suggestions to reflect latest namechanges in unicode 4.1,
//      but they were not done due to compatibility with old code:
//      ubGreek should be renamed to ubGreekandCoptic
//      ubCombiningMarksforSymbols should be renamed  to ubCombiningDiacriticalMarksforSymbols
//      ubPrivateUse should be renamed to ubPrivateUseArea
//
//
// 19-SEP-2003: (changes by Andreas Hausladen)
//   - added OWN_WIDESTRING_MEMMGR for faster memory managment in TWideStringList
//     under Windows
//   - fixed: TWideStringList.Destroy does not set OnChange and OnChanging to nil before calling Clear
//
//
// 29-MAR-2002: MT
//   - WideNormalize now returns strings with normalization mode nfNone unchanged.
//   - Bug fix in WideCompose: Raised exception when Result of WideComposeHangul was an
//     empty string. (#0000044)
//   - Bug fix in WideAdjustLineBreaks
//   - Added Asserts were needed.
//   - TWideStrings.IndexOfName now takes care of NormalizeForm as well.
//   - TWideStrings.IndexOf now takes care of NormalizeForm as well.
//   - TWideString.List Find now uses the same NormalizationForm for the search string as it uses
//     within the list itself.
//
// 29-NOV-2001:
//   - bug fix
// 06-JUN-2001:
//   - small changes
// 28-APR-2001:
//   - bug fixes
// 05-APR-2001:
//   - bug fixes
// 23-MAR-2001:
//   - WideSameText
//   - small changes
// 10-FEB-2001:
//   - bug fix in StringToWideStringEx and WideStringToStringEx
// 05-FEB-2001:
//   - TWideStrings.GetSeparatedText changed (no separator anymore after the last line)
// 29-JAN-2001:
//   - PrepareUnicodeData
//   - LoadInProgress critical section is now created at init time to avoid critical thread races
//   - bug fixes
// 26-JAN-2001:
//   - ExpandANSIString
//   - TWideStrings.SaveUnicode is by default True now
// 20..21-JAN-2001:
//   - StrUpperW, StrLowerW and StrTitleW removed because they potentially would need
//     a reallocation to work correctly (use the WideString versions instead)
//   - further improvements related to internal data
//   - introduced TUnicodeBlock
//   - CodeBlockFromChar improved
// 07-JAN-2001:
//   optimized access to character properties, combining class etc.
// 06-JAN-2001:
//   TWideStrings and TWideStringList improved
// APR-DEC 2000: versions 2.1 - 2.6
//   - preparation for public rlease
//   - additional conversion routines
//   - JCL compliance
//   - character properties unified
//   - character properties data and lookup improvements
//   - reworked Unicode data resource file
//   - improved simple string comparation routines (StrCompW, StrLCompW etc., include surrogate fix)
//   - special case folding data for language neutral case insensitive comparations included
//   - optimized decomposition
//   - composition and normalization support
//   - normalization conformance tests applied
//   - bug fixes
// FEB-MAR 2000: version 2.0
//   - Unicode regular expressions (URE) search class (TURESearch)
//   - generic search engine base class for both the Boyer-Moore and the RE search class
//   - whole word only search in UTBM, bug fixes in UTBM
//   - string decompositon (including hangul)
// OCT/99 - JAN/2000: version 1.0
//   - basic Unicode implementation, more than 100 WideString/UCS2 and UCS4 core functions
//   - TWideStrings and TWideStringList classes
//   - Unicode Tuned Boyer-Moore search class (TUTBMSearch)
//   - low and high level Unicode/Wide* functions
//   - low level Unicode UCS4 data import and functions
//   - helper functions
//
//  Version 2.9
// This unit contains routines and classes to manage and work with Unicode/WideString strings.
// You need Delphi 4 or higher to compile this code.
//
// Publicly available low level functions are all preceded by "Unicode..." (e.g.
// in UnicodeToUpper) while the high level functions use the Str... or Wide...
// naming scheme (e.g. StrLICompW and WideUpperCase).
//
// The normalization implementation in this unit has successfully and completely passed the
// official normative conformance testing as of Annex 9 in Technical Report #15
// (Unicode Standard Annex #15, http://www.unicode.org/unicode/reports/tr15, from 2000-08-31).
//
// Open issues:
//   - Yet to do things in the URE class are:
//     - check all character classes if they match correctly
//     - optimize rebuild of DFA (build only when pattern changes)
//     - set flag parameter of ExecuteURE
//     - add \d     any decimal digit
//           \D     any character that is not a decimal digit
//           \s     any whitespace character
//           \S     any character that is not a whitespace character
//           \w     any "word" character
//           \W     any "non-word" character
//   - The wide string classes still compare text with functions provided by the
//     particular system. This works usually fine under WinNT/W2K (although also
//     there are limitations like maximum text lengths). Under Win9x conversions
//     from and to MBCS are necessary which are bound to a particular locale and
//     so very limited in general use. These comparisons should be changed so that
//     the code in this unit is used.

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes,
  JclBase;

{$IFNDEF FPC}
 {$IFDEF MSWINDOWS}
  {$DEFINE OWN_WIDESTRING_MEMMGR}
 {$ENDIF MSWINDOWS}
{$ENDIF ~FPC}

const
  // definitions of often used characters:
  // Note: Use them only for tests of a certain character not to determine character
  //       classes (like white spaces) as in Unicode are often many code points defined
  //       being in a certain class. Hence your best option is to use the various
  //       UnicodeIs* functions.
  WideNull = WideChar(#0);
  WideTabulator = WideChar(#9);
  WideSpace = WideChar(#32);

  // logical line breaks
  WideLF = WideChar(#10);
  WideLineFeed = WideChar(#10);
  WideVerticalTab = WideChar(#11);
  WideFormFeed = WideChar(#12);
  WideCR = WideChar(#13);
  WideCarriageReturn = WideChar(#13);
  WideCRLF: WideString = #13#10;
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

  // byte order marks for Unicode files
  // Unicode text files (in UTF-16 format) should contain $FFFE as first character to
  // identify such a file clearly. Depending on the system where the file was created
  // on this appears either in big endian or little endian style.
  BOM_LSB_FIRST = WideChar($FEFF);
  BOM_MSB_FIRST = WideChar($FFFE);

type
  TSaveFormat = ( sfUTF16LSB, sfUTF16MSB, sfUTF8, sfAnsi );

const
  sfUnicodeLSB = sfUTF16LSB;
  sfUnicodeMSB = sfUTF16MSB;

  BOM_UTF16_LSB: array [0..1] of Byte = ($FF,$FE);
  BOM_UTF16_MSB: array [0..1] of Byte = ($FE,$FF);
  BOM_UTF8: array [0..2] of Byte = ($EF,$BB,$BF);
  BOM_UTF32_LSB: array [0..3] of Byte = ($FF,$FE,$00,$00);
  BOM_UTF32_MSB: array [0..3] of Byte = ($00,$00,$FE,$FF);
//  BOM_UTF7_1: array [0..3] of Byte = ($2B,$2F,$76,$38);
//  BOM_UTF7_2: array [0..3] of Byte = ($2B,$2F,$76,$39);
//  BOM_UTF7_3: array [0..3] of Byte = ($2B,$2F,$76,$2B);
//  BOM_UTF7_4: array [0..3] of Byte = ($2B,$2F,$76,$2F);
//  BOM_UTF7_5: array [0..3] of Byte = ($2B,$2F,$76,$38,$2D);

type
  // Unicode transformation formats (UTF) data types
  PUTF7 = ^UTF7;
  UTF7 = Char;
  PUTF8 = ^UTF8;
  UTF8 = Char;
  PUTF16 = ^UTF16;
  UTF16 = WideChar;
  PUTF32 = ^UTF32;
  UTF32 = Cardinal;

  // UTF conversion schemes (UCS) data types
  PUCS4 = ^UCS4;
  UCS4 = Cardinal;
  PUCS2 = PWideChar;
  UCS2 = WideChar;

  TUCS2Array = array of UCS2;
  TUCS4Array = array of UCS4;

  // various predefined or otherwise useful character property categories
  TCharacterCategory = (
    // normative categories
    ccLetterUppercase,
    ccLetterLowercase,
    ccLetterTitlecase,
    ccMarkNonSpacing,
    ccMarkSpacingCombining,
    ccMarkEnclosing,
    ccNumberDecimalDigit,
    ccNumberLetter,
    ccNumberOther,
    ccSeparatorSpace,
    ccSeparatorLine,
    ccSeparatorParagraph,
    ccOtherControl,
    ccOtherFormat,
    ccOtherSurrogate,
    ccOtherPrivate,
    ccOtherUnassigned,
    // informative categories
    ccLetterModifier,
    ccLetterOther,
    ccPunctuationConnector,
    ccPunctuationDash,
    ccPunctuationOpen,
    ccPunctuationClose,
    ccPunctuationInitialQuote,
    ccPunctuationFinalQuote,
    ccPunctuationOther,
    ccSymbolMath,
    ccSymbolCurrency,
    ccSymbolModifier,
    ccSymbolOther,
    // bidirectional categories
    ccLeftToRight,
    ccLeftToRightEmbedding,
    ccLeftToRightOverride,
    ccRightToLeft,
    ccRightToLeftArabic,
    ccRightToLeftEmbedding,
    ccRightToLeftoverride,
    ccPopDirectionalFormat,
    ccEuropeanNumber,
    ccEuropeanNumberSeparator,
    ccEuropeanNumberTerminator,
    ccArabicNumber,
    ccCommonNumberSeparator,
    ccBoundaryNeutral,
    ccSegmentSeparator,      // this includes tab and vertical tab
    ccWhiteSpace,
    ccOtherNeutrals,
    // self defined categories, they do not appear in the Unicode data file
    ccComposed,              // can be decomposed
    ccNonBreaking,
    ccSymmetric,             // has left and right forms
    ccHexDigit,
    ccQuotationMark,
    ccMirroring,
    ccSpaceOther,
    ccAssigned               // means there is a definition in the Unicode standard
  );
  TCharacterCategories = set of TCharacterCategory;

  // four forms of normalization are defined:
  TNormalizationForm = (
    nfNone, // do not normalize
    nfC,    // canonical decomposition followed by canonical composition (this is most often used)
    nfD,    // canonical decomposition
    nfKC,   // compatibility decomposition followed by a canonical composition
    nfKD    // compatibility decomposition
  );

  // used to hold information about the start and end
  // position of a unicodeblock.
  TUnicodeBlockRange = record
    RangeStart,
    RangeEnd: Cardinal;
  end;

  // An Unicode block usually corresponds to a particular language script but
  // can also represent special characters, musical symbols and the like.
  // http://www.unicode.org/Public/5.0.0/ucd/Blocks.txt
  TUnicodeBlock = (
    ubUndefined,
    ubBasicLatin,
    ubLatin1Supplement,
    ubLatinExtendedA,
    ubLatinExtendedB,
    ubIPAExtensions,
    ubSpacingModifierLetters,
    ubCombiningDiacriticalMarks,
    ubGreekandCoptic,
    ubCyrillic,
    ubCyrillicSupplement,
    ubArmenian,
    ubHebrew,
    ubArabic,
    ubSyriac,
    ubArabicSupplement,
    ubThaana,
    ubNKo,
    ubDevanagari,
    ubBengali,
    ubGurmukhi,
    ubGujarati,
    ubOriya,
    ubTamil,
    ubTelugu,
    ubKannada,
    ubMalayalam,
    ubSinhala,
    ubThai,
    ubLao,
    ubTibetan,
    ubMyanmar,
    ubGeorgian,
    ubHangulJamo,
    ubEthiopic,
    ubEthiopicSupplement,
    ubCherokee,
    ubUnifiedCanadianAboriginalSyllabics,
    ubOgham,
    ubRunic,
    ubTagalog,
    ubHanunoo,
    ubBuhid,
    ubTagbanwa,
    ubKhmer,
    ubMongolian,
    ubLimbu,
    ubTaiLe,
    ubNewTaiLue,
    ubKhmerSymbols,
    ubBuginese,
    ubBalinese,
    ubPhoneticExtensions,
    ubPhoneticExtensionsSupplement,
    ubCombiningDiacriticalMarksSupplement,
    ubLatinExtendedAdditional,
    ubGreekExtended,
    ubGeneralPunctuation,
    ubSuperscriptsandSubscripts,
    ubCurrencySymbols,
    ubCombiningDiacriticalMarksforSymbols,
    ubLetterlikeSymbols,
    ubNumberForms,
    ubArrows,
    ubMathematicalOperators,
    ubMiscellaneousTechnical,
    ubControlPictures,
    ubOpticalCharacterRecognition,
    ubEnclosedAlphanumerics,
    ubBoxDrawing,
    ubBlockElements,
    ubGeometricShapes,
    ubMiscellaneousSymbols,
    ubDingbats,
    ubMiscellaneousMathematicalSymbolsA,
    ubSupplementalArrowsA,
    ubBraillePatterns,
    ubSupplementalArrowsB,
    ubMiscellaneousMathematicalSymbolsB,
    ubSupplementalMathematicalOperators,
    ubMiscellaneousSymbolsandArrows,
    ubGlagolitic,
    ubLatinExtendedC,
    ubCoptic,
    ubGeorgianSupplement,
    ubTifinagh,
    ubEthiopicExtended,
    ubSupplementalPunctuation,
    ubCJKRadicalsSupplement,
    ubKangxiRadicals,
    ubIdeographicDescriptionCharacters,
    ubCJKSymbolsandPunctuation,
    ubHiragana,
    ubKatakana,
    ubBopomofo,
    ubHangulCompatibilityJamo,
    ubKanbun,
    ubBopomofoExtended,
    ubCJKStrokes,
    ubKatakanaPhoneticExtensions,
    ubEnclosedCJKLettersandMonths,
    ubCJKCompatibility,
    ubCJKUnifiedIdeographsExtensionA,
    ubYijingHexagramSymbols,
    ubCJKUnifiedIdeographs,
    ubYiSyllables,
    ubYiRadicals,
    ubModifierToneLetters,
    ubLatinExtendedD,
    ubSylotiNagri,
    ubPhagsPa,
    ubHangulSyllables,
    ubHighSurrogates,
    ubHighPrivateUseSurrogates,
    ubLowSurrogates,
    ubPrivateUseArea,
    ubCJKCompatibilityIdeographs,
    ubAlphabeticPresentationForms,
    ubArabicPresentationFormsA,
    ubVariationSelectors,
    ubVerticalForms,
    ubCombiningHalfMarks,
    ubCJKCompatibilityForms,
    ubSmallFormVariants,
    ubArabicPresentationFormsB,
    ubHalfwidthandFullwidthForms,
    ubSpecials,
    ubLinearBSyllabary,
    ubLinearBIdeograms,
    ubAegeanNumbers,
    ubAncientGreekNumbers,
    ubOldItalic,
    ubGothic,
    ubUgaritic,
    ubOldPersian,
    ubDeseret,
    ubShavian,
    ubOsmanya,
    ubCypriotSyllabary,
    ubPhoenician,
    ubKharoshthi,
    ubCuneiform,
    ubCuneiformNumbersAndPunctuation,
    ubByzantineMusicalSymbols,
    ubMusicalSymbols,
    ubAncientGreekMusicalNotation,
    ubTaiXuanJingSymbols,
    ubCountingRodNumerals,
    ubMathematicalAlphanumericSymbols,
    ubCJKUnifiedIdeographsExtensionB,
    ubCJKCompatibilityIdeographsSupplement,
    ubTags,
    ubVariationSelectorsSupplement,
    ubSupplementaryPrivateUseAreaA,
    ubSupplementaryPrivateUseAreaB
  );

  TUnicodeBlockData = record
    Range: TUnicodeBlockRange;
    Name: string;
  end;
  PUnicodeBlockData = ^TUnicodeBlockData;

const
  UnicodeBlockData: array [TUnicodeBlock] of TUnicodeBlockData =
    ((Range:(RangeStart: $FFFFFFFF; RangeEnd: $0000); Name: 'No-block'),
    (Range:(RangeStart: $0000; RangeEnd: $007F); Name: 'Basic Latin'),
    (Range:(RangeStart: $0080; RangeEnd: $00FF); Name: 'Latin-1 Supplement'),
    (Range:(RangeStart: $0100; RangeEnd: $017F); Name: 'Latin Extended-A'),
    (Range:(RangeStart: $0180; RangeEnd: $024F); Name: 'Latin Extended-B'),
    (Range:(RangeStart: $0250; RangeEnd: $02AF); Name: 'IPA Extensions'),
    (Range:(RangeStart: $02B0; RangeEnd: $02FF); Name: 'Spacing Modifier Letters'),
    (Range:(RangeStart: $0300; RangeEnd: $036F); Name: 'Combining Diacritical Marks'),
    (Range:(RangeStart: $0370; RangeEnd: $03FF); Name: 'Greek and Coptic'),
    (Range:(RangeStart: $0400; RangeEnd: $04FF); Name: 'Cyrillic'),
    (Range:(RangeStart: $0500; RangeEnd: $052F); Name: 'Cyrillic Supplement'),
    (Range:(RangeStart: $0530; RangeEnd: $058F); Name: 'Armenian'),
    (Range:(RangeStart: $0590; RangeEnd: $05FF); Name: 'Hebrew'),
    (Range:(RangeStart: $0600; RangeEnd: $06FF); Name: 'Arabic'),
    (Range:(RangeStart: $0700; RangeEnd: $074F); Name: 'Syriac'),
    (Range:(RangeStart: $0750; RangeEnd: $077F); Name: 'Arabic Supplement'),
    (Range:(RangeStart: $0780; RangeEnd: $07BF); Name: 'Thaana'),
    (Range:(RangeStart: $07C0; RangeEnd: $07FF); Name: 'NKo'),
    (Range:(RangeStart: $0900; RangeEnd: $097F); Name: 'Devanagari'),
    (Range:(RangeStart: $0980; RangeEnd: $09FF); Name: 'Bengali'),
    (Range:(RangeStart: $0A00; RangeEnd: $0A7F); Name: 'Gurmukhi'),
    (Range:(RangeStart: $0A80; RangeEnd: $0AFF); Name: 'Gujarati'),
    (Range:(RangeStart: $0B00; RangeEnd: $0B7F); Name: 'Oriya'),
    (Range:(RangeStart: $0B80; RangeEnd: $0BFF); Name: 'Tamil'),
    (Range:(RangeStart: $0C00; RangeEnd: $0C7F); Name: 'Telugu'),
    (Range:(RangeStart: $0C80; RangeEnd: $0CFF); Name: 'Kannada'),
    (Range:(RangeStart: $0D00; RangeEnd: $0D7F); Name: 'Malayalam'),
    (Range:(RangeStart: $0D80; RangeEnd: $0DFF); Name: 'Sinhala'),
    (Range:(RangeStart: $0E00; RangeEnd: $0E7F); Name: 'Thai'),
    (Range:(RangeStart: $0E80; RangeEnd: $0EFF); Name: 'Lao'),
    (Range:(RangeStart: $0F00; RangeEnd: $0FFF); Name: 'Tibetan'),
    (Range:(RangeStart: $1000; RangeEnd: $109F); Name: 'Myanmar'),
    (Range:(RangeStart: $10A0; RangeEnd: $10FF); Name: 'Georgian'),
    (Range:(RangeStart: $1100; RangeEnd: $11FF); Name: 'Hangul Jamo'),
    (Range:(RangeStart: $1200; RangeEnd: $137F); Name: 'Ethiopic'),
    (Range:(RangeStart: $1380; RangeEnd: $139F); Name: 'Ethiopic Supplement'),
    (Range:(RangeStart: $13A0; RangeEnd: $13FF); Name: 'Cherokee'),
    (Range:(RangeStart: $1400; RangeEnd: $167F); Name: 'Unified Canadian Aboriginal Syllabics'),
    (Range:(RangeStart: $1680; RangeEnd: $169F); Name: 'Ogham'),
    (Range:(RangeStart: $16A0; RangeEnd: $16FF); Name: 'Runic'),
    (Range:(RangeStart: $1700; RangeEnd: $171F); Name: 'Tagalog'),
    (Range:(RangeStart: $1720; RangeEnd: $173F); Name: 'Hanunoo'),
    (Range:(RangeStart: $1740; RangeEnd: $175F); Name: 'Buhid'),
    (Range:(RangeStart: $1760; RangeEnd: $177F); Name: 'Tagbanwa'),
    (Range:(RangeStart: $1780; RangeEnd: $17FF); Name: 'Khmer'),
    (Range:(RangeStart: $1800; RangeEnd: $18AF); Name: 'Mongolian'),
    (Range:(RangeStart: $1900; RangeEnd: $194F); Name: 'Limbu'),
    (Range:(RangeStart: $1950; RangeEnd: $197F); Name: 'Tai Le'),
    (Range:(RangeStart: $1980; RangeEnd: $19DF); Name: 'New Tai Lue'),
    (Range:(RangeStart: $19E0; RangeEnd: $19FF); Name: 'Khmer Symbols'),
    (Range:(RangeStart: $1A00; RangeEnd: $1A1F); Name: 'Buginese'),
    (Range:(RangeStart: $1B00; RangeEnd: $1B7F); Name: 'Balinese'),
    (Range:(RangeStart: $1D00; RangeEnd: $1D7F); Name: 'Phonetic Extensions'),
    (Range:(RangeStart: $1D80; RangeEnd: $1DBF); Name: 'Phonetic Extensions Supplement'),
    (Range:(RangeStart: $1DC0; RangeEnd: $1DFF); Name: 'Combining Diacritical Marks Supplement'),
    (Range:(RangeStart: $1E00; RangeEnd: $1EFF); Name: 'Latin Extended Additional'),
    (Range:(RangeStart: $1F00; RangeEnd: $1FFF); Name: 'Greek Extended'),
    (Range:(RangeStart: $2000; RangeEnd: $206F); Name: 'General Punctuation'),
    (Range:(RangeStart: $2070; RangeEnd: $209F); Name: 'Superscripts and Subscripts'),
    (Range:(RangeStart: $20A0; RangeEnd: $20CF); Name: 'Currency Symbols'),
    (Range:(RangeStart: $20D0; RangeEnd: $20FF); Name: 'Combining Diacritical Marks for Symbols'),
    (Range:(RangeStart: $2100; RangeEnd: $214F); Name: 'Letterlike Symbols'),
    (Range:(RangeStart: $2150; RangeEnd: $218F); Name: 'Number Forms'),
    (Range:(RangeStart: $2190; RangeEnd: $21FF); Name: 'Arrows'),
    (Range:(RangeStart: $2200; RangeEnd: $22FF); Name: 'Mathematical Operators'),
    (Range:(RangeStart: $2300; RangeEnd: $23FF); Name: 'Miscellaneous Technical'),
    (Range:(RangeStart: $2400; RangeEnd: $243F); Name: 'Control Pictures'),
    (Range:(RangeStart: $2440; RangeEnd: $245F); Name: 'Optical Character Recognition'),
    (Range:(RangeStart: $2460; RangeEnd: $24FF); Name: 'Enclosed Alphanumerics'),
    (Range:(RangeStart: $2500; RangeEnd: $257F); Name: 'Box Drawing'),
    (Range:(RangeStart: $2580; RangeEnd: $259F); Name: 'Block Elements'),
    (Range:(RangeStart: $25A0; RangeEnd: $25FF); Name: 'Geometric Shapes'),
    (Range:(RangeStart: $2600; RangeEnd: $26FF); Name: 'Miscellaneous Symbols'),
    (Range:(RangeStart: $2700; RangeEnd: $27BF); Name: 'Dingbats'),
    (Range:(RangeStart: $27C0; RangeEnd: $27EF); Name: 'Miscellaneous Mathematical Symbols-A'),
    (Range:(RangeStart: $27F0; RangeEnd: $27FF); Name: 'Supplemental Arrows-A'),
    (Range:(RangeStart: $2800; RangeEnd: $28FF); Name: 'Braille Patterns'),
    (Range:(RangeStart: $2900; RangeEnd: $297F); Name: 'Supplemental Arrows-B'),
    (Range:(RangeStart: $2980; RangeEnd: $29FF); Name: 'Miscellaneous Mathematical Symbols-B'),
    (Range:(RangeStart: $2A00; RangeEnd: $2AFF); Name: 'Supplemental Mathematical Operators'),
    (Range:(RangeStart: $2B00; RangeEnd: $2BFF); Name: 'Miscellaneous Symbols and Arrows'),
    (Range:(RangeStart: $2C00; RangeEnd: $2C5F); Name: 'Glagolitic'),
    (Range:(RangeStart: $2C60; RangeEnd: $2C7F); Name: 'Latin Extended-C'),
    (Range:(RangeStart: $2C80; RangeEnd: $2CFF); Name: 'Coptic'),
    (Range:(RangeStart: $2D00; RangeEnd: $2D2F); Name: 'Georgian Supplement'),
    (Range:(RangeStart: $2D30; RangeEnd: $2D7F); Name: 'Tifinagh'),
    (Range:(RangeStart: $2D80; RangeEnd: $2DDF); Name: 'Ethiopic Extended'),
    (Range:(RangeStart: $2E00; RangeEnd: $2E7F); Name: 'Supplemental Punctuation'),
    (Range:(RangeStart: $2E80; RangeEnd: $2EFF); Name: 'CJK Radicals Supplement'),
    (Range:(RangeStart: $2F00; RangeEnd: $2FDF); Name: 'Kangxi Radicals'),
    (Range:(RangeStart: $2FF0; RangeEnd: $2FFF); Name: 'Ideographic Description Characters'),
    (Range:(RangeStart: $3000; RangeEnd: $303F); Name: 'CJK Symbols and Punctuation'),
    (Range:(RangeStart: $3040; RangeEnd: $309F); Name: 'Hiragana'),
    (Range:(RangeStart: $30A0; RangeEnd: $30FF); Name: 'Katakana'),
    (Range:(RangeStart: $3100; RangeEnd: $312F); Name: 'Bopomofo'),
    (Range:(RangeStart: $3130; RangeEnd: $318F); Name: 'Hangul Compatibility Jamo'),
    (Range:(RangeStart: $3190; RangeEnd: $319F); Name: 'Kanbun'),
    (Range:(RangeStart: $31A0; RangeEnd: $31BF); Name: 'Bopomofo Extended'),
    (Range:(RangeStart: $31C0; RangeEnd: $31EF); Name: 'CJK Strokes'),
    (Range:(RangeStart: $31F0; RangeEnd: $31FF); Name: 'Katakana Phonetic Extensions'),
    (Range:(RangeStart: $3200; RangeEnd: $32FF); Name: 'Enclosed CJK Letters and Months'),
    (Range:(RangeStart: $3300; RangeEnd: $33FF); Name: 'CJK Compatibility'),
    (Range:(RangeStart: $3400; RangeEnd: $4DBF); Name: 'CJK Unified Ideographs Extension A'),
    (Range:(RangeStart: $4DC0; RangeEnd: $4DFF); Name: 'Yijing Hexagram Symbols'),
    (Range:(RangeStart: $4E00; RangeEnd: $9FFF); Name: 'CJK Unified Ideographs'),
    (Range:(RangeStart: $A000; RangeEnd: $A48F); Name: 'Yi Syllables'),
    (Range:(RangeStart: $A490; RangeEnd: $A4CF); Name: 'Yi Radicals'),
    (Range:(RangeStart: $A700; RangeEnd: $A71F); Name: 'Modifier Tone Letters'),
    (Range:(RangeStart: $A720; RangeEnd: $A7FF); Name: 'Latin Extended-D'),
    (Range:(RangeStart: $A800; RangeEnd: $A82F); Name: 'Syloti Nagri'),
    (Range:(RangeStart: $A840; RangeEnd: $A87F); Name: 'Phags-pa'),
    (Range:(RangeStart: $AC00; RangeEnd: $D7AF); Name: 'Hangul Syllables'),
    (Range:(RangeStart: $D800; RangeEnd: $DB7F); Name: 'High Surrogates'),
    (Range:(RangeStart: $DB80; RangeEnd: $DBFF); Name: 'High Private Use Surrogates'),
    (Range:(RangeStart: $DC00; RangeEnd: $DFFF); Name: 'Low Surrogates'),
    (Range:(RangeStart: $E000; RangeEnd: $F8FF); Name: 'Private Use Area'),
    (Range:(RangeStart: $F900; RangeEnd: $FAFF); Name: 'CJK Compatibility Ideographs'),
    (Range:(RangeStart: $FB00; RangeEnd: $FB4F); Name: 'Alphabetic Presentation Forms'),
    (Range:(RangeStart: $FB50; RangeEnd: $FDFF); Name: 'Arabic Presentation Forms-A'),
    (Range:(RangeStart: $FE00; RangeEnd: $FE0F); Name: 'Variation Selectors'),
    (Range:(RangeStart: $FE10; RangeEnd: $FE1F); Name: 'Vertical Forms'),
    (Range:(RangeStart: $FE20; RangeEnd: $FE2F); Name: 'Combining Half Marks'),
    (Range:(RangeStart: $FE30; RangeEnd: $FE4F); Name: 'CJK Compatibility Forms'),
    (Range:(RangeStart: $FE50; RangeEnd: $FE6F); Name: 'Small Form Variants'),
    (Range:(RangeStart: $FE70; RangeEnd: $FEFF); Name: 'Arabic Presentation Forms-B'),
    (Range:(RangeStart: $FF00; RangeEnd: $FFEF); Name: 'Halfwidth and Fullwidth Forms'),
    (Range:(RangeStart: $FFF0; RangeEnd: $FFFF); Name: 'Specials'),
    (Range:(RangeStart: $10000; RangeEnd: $1007F); Name: 'Linear B Syllabary'),
    (Range:(RangeStart: $10080; RangeEnd: $100FF); Name: 'Linear B Ideograms'),
    (Range:(RangeStart: $10100; RangeEnd: $1013F); Name: 'Aegean Numbers'),
    (Range:(RangeStart: $10140; RangeEnd: $1018F); Name: 'Ancient Greek Numbers'),
    (Range:(RangeStart: $10300; RangeEnd: $1032F); Name: 'Old Italic'),
    (Range:(RangeStart: $10330; RangeEnd: $1034F); Name: 'Gothic'),
    (Range:(RangeStart: $10380; RangeEnd: $1039F); Name: 'Ugaritic'),
    (Range:(RangeStart: $103A0; RangeEnd: $103DF); Name: 'Old Persian'),
    (Range:(RangeStart: $10400; RangeEnd: $1044F); Name: 'Deseret'),
    (Range:(RangeStart: $10450; RangeEnd: $1047F); Name: 'Shavian'),
    (Range:(RangeStart: $10480; RangeEnd: $104AF); Name: 'Osmanya'),
    (Range:(RangeStart: $10800; RangeEnd: $1083F); Name: 'Cypriot Syllabary'),
    (Range:(RangeStart: $10900; RangeEnd: $1091F); Name: 'Phoenician'),
    (Range:(RangeStart: $10A00; RangeEnd: $10A5F); Name: 'Kharoshthi'),
    (Range:(RangeStart: $12000; RangeEnd: $123FF); Name: 'Cuneiform'),
    (Range:(RangeStart: $12400; RangeEnd: $1247F); Name: 'Cuneiform Numbers and Punctuation'),
    (Range:(RangeStart: $1D000; RangeEnd: $1D0FF); Name: 'Byzantine Musical Symbols'),
    (Range:(RangeStart: $1D100; RangeEnd: $1D1FF); Name: 'Musical Symbols'),
    (Range:(RangeStart: $1D200; RangeEnd: $1D24F); Name: 'Ancient Greek Musical Notation'),
    (Range:(RangeStart: $1D300; RangeEnd: $1D35F); Name: 'Tai Xuan Jing Symbols'),
    (Range:(RangeStart: $1D360; RangeEnd: $1D37F); Name: 'Counting Rod Numerals'),
    (Range:(RangeStart: $1D400; RangeEnd: $1D7FF); Name: 'Mathematical Alphanumeric Symbols'),
    (Range:(RangeStart: $20000; RangeEnd: $2A6DF); Name: 'CJK Unified Ideographs Extension B'),
    (Range:(RangeStart: $2F800; RangeEnd: $2FA1F); Name: 'CJK Compatibility Ideographs Supplement'),
    (Range:(RangeStart: $E0000; RangeEnd: $E007F); Name: 'Tags'),
    (Range:(RangeStart: $E0100; RangeEnd: $E01EF); Name: 'Variation Selectors Supplement'),
    (Range:(RangeStart: $F0000; RangeEnd: $FFFFF); Name: 'Supplementary Private Use Area-A'),
    (Range:(RangeStart: $100000; RangeEnd: $10FFFF); Name: 'Supplementary Private Use Area-B'));

type
  TWideStrings = class;

  TSearchFlag = (
    sfCaseSensitive,    // match letter case
    sfIgnoreNonSpacing, // ignore non-spacing characters in search
    sfSpaceCompress,    // handle several consecutive white spaces as one white space
                        // (this applies to the pattern as well as the search text)
    sfWholeWordOnly     // match only text at end/start and/or surrounded by white spaces
  );

  TSearchFlags = set of TSearchFlag;

  // a generic search class defininition used for tuned Boyer-Moore and Unicode
  // regular expression searches
  TSearchEngine = class(TObject)
  private
    FResults: TList;      // 2 entries for each result (start and stop position)
    FOwner: TWideStrings; // at the moment unused, perhaps later to access strings faster
  protected
    function GetCount: Integer; virtual;
  public
    constructor Create(AOwner: TWideStrings); virtual;
    destructor Destroy; override;

    procedure AddResult(Start, Stop: Cardinal); virtual;
    procedure Clear; virtual;
    procedure ClearResults; virtual;
    procedure DeleteResult(Index: Cardinal); virtual;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; virtual; abstract;
    procedure FindPrepare(Pattern: PWideChar; PatternLength: Cardinal; Options: TSearchFlags); overload; virtual; abstract;
    function FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean; overload; virtual; abstract;
    function FindFirst(Text: PWideChar; TextLen: Cardinal; var Start, Stop: Cardinal): Boolean; overload; virtual; abstract;
    function FindAll(const Text: WideString): Boolean; overload; virtual; abstract;
    function FindAll(Text: PWideChar; TextLen: Cardinal): Boolean; overload; virtual; abstract;
    procedure GetResult(Index: Cardinal; var Start, Stop: Integer); virtual;

    property Count: Integer read GetCount;
  end;

  // The Unicode Tuned Boyer-Moore (UTBM) search implementation is an extended
  // translation created from a free package written by Mark Leisher (mleisher att crl dott nmsu dott edu).
  //
  // The code handles high and low surrogates as well as case (in)dependency,
  // can ignore non-spacing characters and allows optionally to return whole
  // words only.

  // single pattern character
  PUTBMChar = ^TUTBMChar;
  TUTBMChar = record
    LoCase,
    UpCase,
    TitleCase: UCS4;
  end;

  PUTBMSkip = ^TUTBMSkip;
  TUTBMSkip = record
    BMChar: PUTBMChar;
    SkipValues: Integer;
  end;

  TUTBMSearch = class(TSearchEngine)
  private
    FFlags: TSearchFlags;
    FPattern: PUTBMChar;
    FPatternUsed: Cardinal;
    FPatternSize: Cardinal;
    FPatternLength: Cardinal;
    FSkipValues: PUTBMSkip;
    FSkipsUsed: Integer;
    FMD4: Cardinal;
  protected
    procedure ClearPattern;
    procedure Compile(Pattern: PUCS2; PatternLength: Integer; Flags: TSearchFlags);
    function Find(Text: PUCS2; TextLen: Cardinal; var MatchStart, MatchEnd: Cardinal): Boolean;
    function GetSkipValue(TextStart, TextEnd: PUCS2): Cardinal;
    function Match(Text, Start, Stop: PUCS2; var MatchStart, MatchEnd: Cardinal): Boolean;
  public
    procedure Clear; override;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; override;
    procedure FindPrepare(Pattern: PWideChar; PatternLength: Cardinal; Options: TSearchFlags); overload; override;
    function FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean; overload; override;
    function FindFirst(Text: PWideChar; TextLen: Cardinal; var Start, Stop: Cardinal): Boolean; overload; override;
    function FindAll(const Text: WideString): Boolean; overload; override;
    function FindAll(Text: PWideChar; TextLen: Cardinal): Boolean; overload; override;
  end;

  // Regular expression search engine for text in UCS2 form taking surrogates
  // into account. This implementation is an improved translation from the URE
  // package written by Mark Leisher (mleisher att crl dott nmsu dott edu) who used a variation
  // of the RE->DFA algorithm done by Mark Hopkins (markh att csd4 dott csd dott uwm dott edu).
  // Assumptions:
  //   o  Regular expression and text already normalized.
  //   o  Conversion to lower case assumes a 1-1 mapping.
  //
  // Definitions:
  //   Separator - any one of U+2028, U+2029, NL, CR.
  //
  // Operators:
  //   .      - match any character
  //   *      - match zero or more of the last subexpression
  //   +      - match one or more of the last subexpression
  //   ?      - match zero or one of the last subexpression
  //   ()     - subexpression grouping
  //   {m, n} - match at least m occurences and up to n occurences
  //            Note: both values can be 0 or ommitted which denotes then a unlimiting bound
  //            {,} and {0,} and {0, 0} correspond to *
  //            {, 1} and {0, 1} correspond to ?
  //            {1,} and {1, 0} correspond to +
  //   {m}    - match exactly m occurences
  //
  //   Notes:
  //     o  The "." operator normally does not match separators, but a flag is
  //        available that will allow this operator to match a separator.
  //
  // Literals and Constants:
  //   c       - literal UCS2 character
  //   \x....  - hexadecimal number of up to 4 digits
  //   \X....  - hexadecimal number of up to 4 digits
  //   \u....  - hexadecimal number of up to 4 digits
  //   \U....  - hexadecimal number of up to 4 digits
  //
  // Character classes:
  //   [...]           - Character class
  //   [^...]          - Negated character class
  //   \pN1,N2,...,Nn  - Character properties class
  //   \PN1,N2,...,Nn  - Negated character properties class
  //
  //   POSIX character classes recognized:
  //     :alnum:
  //     :alpha:
  //     :cntrl:
  //     :digit:
  //     :graph:
  //     :lower:
  //     :print:
  //     :punct:
  //     :space:
  //     :upper:
  //     :xdigit:
  //
  //   Notes:
  //     o  Character property classes are \p or \P followed by a comma separated
  //        list of integers between 0 and the maximum entry index in TCharacterCategory.
  //        These integers directly correspond to the TCharacterCategory enumeration entries.
  //        Note: upper, lower and title case classes need to have case sensitive search
  //              be enabled to match correctly!
  //
  //     o  Character classes can contain literals, constants and character
  //        property classes. Example:
  //
  //        [abc\U10A\p0,13,4]

  // structure used to handle a compacted range of characters
  PUcRange = ^TUcRange;
  TUcRange = record
    MinCode,
    MaxCode: UCS4;
  end;

  TUcCClass = record
    Ranges: array of TUcRange;
    RangesUsed: Integer;
  end;

  // either a single character or a list of character classes
  TUcSymbol = record
    Chr: UCS4;
    CCL: TUcCClass;
  end;

  // this is a general element structure used for expressions and stack elements
  TUcElement = record
    OnStack: Boolean;
    AType,
    LHS,
    RHS: Cardinal;
  end;

  // this is a structure used to track a list or a stack of states
  PUcStateList = ^TUcStateList;
  TUcStateList = record
    List: array of Cardinal;
    ListUsed: Integer;
  end;

  // structure to track the list of unique states for a symbol during reduction
  PUcSymbolTableEntry = ^TUcSymbolTableEntry;
  TUcSymbolTableEntry = record
    ID,
    AType: Cardinal;
    Mods,
    Categories: TCharacterCategories;
    Symbol: TUcSymbol;
    States: TUcStateList;
  end;

  // structure to hold a single State
  PUcState = ^TUcState;
  TUcState = record
    ID: Cardinal;
    Accepting: Boolean;
    StateList: TUcStateList;
    Transitions: array of TUcElement;
    TransitionsUsed: Integer;
  end;

  // structure used for keeping lists of states
  TUcStateTable = record
    States: array of TUcState;
    StatesUsed: Integer;
  end;

  // structure to track pairs of DFA states when equivalent states are merged
  TUcEquivalent = record
    Left,
    Right: Cardinal;
  end;

  TUcExpressionList = record
    Expressions: array of TUcElement;
    ExpressionsUsed: Integer;
  end;

  TUcSymbolTable = record
    Symbols: array of TUcSymbolTableEntry;
    SymbolsUsed: Integer;
  end;

  TUcEquivalentList = record
    Equivalents: array of TUcEquivalent;
    EquivalentsUsed: Integer;
  end;

  // structure used for constructing the NFA and reducing to a minimal DFA
  PUREBuffer = ^TUREBuffer;
  TUREBuffer = record
    Reducing: Boolean;
    Error: Integer;
    Flags: Cardinal;
    Stack: TUcStateList;
    SymbolTable: TUcSymbolTable;       // table of unique symbols encountered
    ExpressionList: TUcExpressionList; // tracks the unique expressions generated
                                       // for the NFA and when the NFA is reduced
    States: TUcStateTable;             // the reduced table of unique groups of NFA states
    EquivalentList: TUcEquivalentList; // tracks states when equivalent states are merged
  end;

  TUcTransition = record
    Symbol,
    NextState: Cardinal;
  end;

  PDFAState = ^TDFAState;
  TDFAState = record
    Accepting: Boolean;
    NumberTransitions: Integer;
    StartTransition: Integer;
  end;

  TDFAStates = record
    States: array of TDFAState;
    StatesUsed: Integer;
  end;

  TUcTransitions = record
    Transitions: array of TUcTransition;
    TransitionsUsed: Integer;
  end;

  TDFA = record
    Flags: Cardinal;
    SymbolTable: TUcSymbolTable;
    StateList: TDFAStates;
    TransitionList: TUcTransitions;
  end;

  TURESearch = class(TSearchEngine)
  private
    FUREBuffer: TUREBuffer;
    FDFA: TDFA;
  protected
    procedure AddEquivalentPair(L, R: Cardinal);
    procedure AddRange(var CCL: TUcCClass; Range: TUcRange);
    function AddState(NewStates: array of Cardinal): Cardinal;
    procedure AddSymbolState(Symbol, State: Cardinal);
    function BuildCharacterClass(CP: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
    procedure ClearUREBuffer;
    function CompileSymbol(S: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
    procedure CompileURE(RE: PWideChar; RELength: Cardinal; Casefold: Boolean);
    procedure CollectPendingOperations(var State: Cardinal);
    function ConvertRegExpToNFA(RE: PWideChar; RELength: Cardinal): Cardinal;
    function ExecuteURE(Flags: Cardinal; Text: PUCS2; TextLen: Cardinal; var MatchStart, MatchEnd: Cardinal): Boolean;
    procedure ClearDFA;
    procedure HexDigitSetup(Symbol: PUcSymbolTableEntry);
    function MakeExpression(AType, LHS, RHS: Cardinal): Cardinal;
    function MakeHexNumber(NP: PUCS2; Limit: Cardinal; var Number: Cardinal): Cardinal;
    function MakeSymbol(S: PUCS2; Limit: Cardinal; var Consumed: Cardinal): Cardinal;
    procedure MergeEquivalents;
    function ParsePropertyList(Properties: PUCS2; Limit: Cardinal; var Categories: TCharacterCategories): Cardinal;
    function Peek: Cardinal;
    function Pop: Cardinal;
    function PosixCCL(CP: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
    function ProbeLowSurrogate(LeftState: PUCS2; Limit: Cardinal; var Code: UCS4): Cardinal;
    procedure Push(V: Cardinal);
    procedure Reduce(Start: Cardinal);
    procedure SpaceSetup(Symbol: PUcSymbolTableEntry; Categories: TCharacterCategories);
    function SymbolsAreDifferent(A, B: PUcSymbolTableEntry): Boolean;
  public
    procedure Clear; override;
    procedure FindPrepare(const Pattern: WideString; Options: TSearchFlags); overload; override;
    procedure FindPrepare(Pattern: PWideChar; PatternLength: Cardinal; Options: TSearchFlags); overload; override;
    function FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean; overload; override;
    function FindFirst(Text: PWideChar; TextLen: Cardinal; var Start, Stop: Cardinal): Boolean; overload; override;
    function FindAll(const Text: WideString): Boolean; overload; override;
    function FindAll(Text: PWideChar; TextLen: Cardinal): Boolean; overload; override;
  end;

  // Event used to give the application a chance to switch the way of how to save
  // the text in TWideStrings if the text contains characters not only from the
  // ANSI block but the save type is ANSI. On triggering the event the application
  // can change the property SaveUnicode as needed. This property is again checked
  // after the callback returns.
  TConfirmConversionEvent = procedure (Sender: TWideStrings; var Allowed: Boolean) of object;

  TWideStrings = class(TPersistent)
  private
    FUpdateCount: Integer;
    FLanguage: LCID;        // language can usually left alone, the system's default is used
    FSaved: Boolean;        // set in SaveToStream, True in case saving was successfull otherwise False
    FNormalizationForm: TNormalizationForm; // determines in which form Unicode strings should be stored
    FOnConfirmConversion: TConfirmConversionEvent;
    FSaveFormat: TSaveFormat;  // overrides the FSaveUnicode flag, initialized when a file is loaded,
                               // expect losses if it is set to sfAnsi before saving
    function GetCommaText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetNormalizationForm(const Value: TNormalizationForm);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
    function GetSaveUnicode: Boolean;
    procedure SetSaveUnicode(const Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoConfirmConversion(var Allowed: Boolean); virtual;
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): WideString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual; abstract;
    procedure PutObject(Index: Integer; AObject: TObject); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    procedure SetLanguage(Value: LCID); virtual;
  public
    constructor Create;

    function Add(const S: WideString): Integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure AddStrings(Strings: TWideStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TWideStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetSeparatedText(Separators: WideString): WideString; virtual;
    function GetText: PWideChar; virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: WideString; AObject: TObject);
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream; WithBOM: Boolean = True); virtual;
    procedure SetText(const Value: WideString); virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Language: LCID read FLanguage write SetLanguage;
    property Names[Index: Integer]: WideString read GetName;
    property NormalizationForm: TNormalizationForm read FNormalizationForm write SetNormalizationForm default nfC;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property Saved: Boolean read FSaved;
    property SaveUnicode: Boolean read GetSaveUnicode write SetSaveUnicode default True;
    property SaveFormat: TSaveFormat read FSaveFormat write FSaveFormat default sfUnicodeLSB;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetText;

    property OnConfirmConversion: TConfirmConversionEvent read FOnConfirmConversion write FOnConfirmConversion;
  end;

  //----- TWideStringList class
  TDynWideCharArray = array of WideChar;
  TWideStringItem = record
    {$IFDEF OWN_WIDESTRING_MEMMGR}
    FString: PWideChar; // "array of WideChar";
    {$ELSE}
    FString: WideString;
    {$ENDIF OWN_WIDESTRING_MEMMGR}
    FObject: TObject;
  end;

  TWideStringItemList = array of TWideStringItem;

  TWideStringList = class(TWideStrings)
  private
    FList: TWideStringItemList;
    FCount: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: WideString);
    procedure SetSorted(Value: Boolean);
    {$IFDEF OWN_WIDESTRING_MEMMGR}
    procedure SetListString(Index: Integer; const S: WideString);
    {$ENDIF OWN_WIDESTRING_MEMMGR}
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): WideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetLanguage(Value: LCID); override;
  public
    destructor Destroy; override;

    function Add(const S: WideString): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure Sort; virtual;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  // result type for number retrieval functions
  TUcNumber = record
    Numerator,
    Denominator: Integer;
  end;

const
  ReplacementCharacter: UCS4 = $0000FFFD;
  MaximumUCS2: UCS4 = $0000FFFF;
  MaximumUTF16: UCS4 = $0010FFFF;
  MaximumUCS4: UCS4 = $7FFFFFFF;

  SurrogateHighStart = UCS4($D800);
  SurrogateHighEnd = UCS4($DBFF);
  SurrogateLowStart = UCS4($DC00);
  SurrogateLowEnd = UCS4($DFFF);

// functions involving null-terminated strings
// NOTE: PWideChars as well as WideStrings are NOT managed by reference counting under Win32.
//       In Kylix this is different. WideStrings are reference counted there, just like ANSI strings.
function StrLenW(Str: PWideChar): Cardinal;
function StrEndW(Str: PWideChar): PWideChar;
function StrMoveW(Dest, Source: PWideChar; Count: Cardinal): PWideChar;
function StrCopyW(Dest, Source: PWideChar): PWideChar;
function StrECopyW(Dest, Source: PWideChar): PWideChar;
function StrLCopyW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
function StrPCopyWW(Dest: PWideChar; const Source: WideString): PWideChar; overload;
function StrPCopyW(Dest: PWideChar; const Source: string): PWideChar;
function StrPLCopyWW(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
function StrPLCopyW(Dest: PWideChar; const Source: string; MaxLen: Cardinal): PWideChar;
function StrCatW(Dest: PWideChar; const Source: PWideChar): PWideChar;
function StrLCatW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
function StrCompW(const Str1, Str2: PWideChar): Integer;
function StrICompW(const Str1, Str2: PWideChar): Integer;
function StrLCompW(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function StrLICompW(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function StrNScanW(const Str1, Str2: PWideChar): Integer;
function StrRNScanW(const Str1, Str2: PWideChar): Integer;
function StrScanW(Str: PWideChar; Chr: WideChar): PWideChar; overload;
function StrScanW(Str: PWideChar; Chr: WideChar; StrLen: Cardinal): PWideChar; overload;
function StrRScanW(Str: PWideChar; Chr: WideChar): PWideChar;
function StrPosW(Str, SubStr: PWideChar): PWideChar;
function StrAllocW(WideSize: Cardinal): PWideChar;
function StrBufSizeW(const Str: PWideChar): Cardinal;
function StrNewW(const Str: PWideChar): PWideChar; overload;
function StrNewW(const Str: WideString): PWideChar; overload;
procedure StrDisposeW(Str: PWideChar);
procedure StrDisposeAndNilW(var Str: PWideChar);
procedure StrSwapByteOrder(Str: PWideChar);

// functions involving Delphi wide strings
function WideAdjustLineBreaks(const S: WideString): WideString;
function WideCharPos(const S: WideString; const Ch: WideChar; const Index: Integer): Integer;  //az
function WideCompose(const S: WideString): WideString;
function WideDecompose(const S: WideString; Compatible: Boolean): WideString;
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
function WideStringOfChar(C: WideChar; Count: Cardinal): WideString;
function WideCaseFolding(C: WideChar): WideString; overload;
function WideCaseFolding(const S: WideString): WideString; overload;
function WideLowerCase(C: WideChar): WideString; overload;
function WideLowerCase(const S: WideString): WideString; overload;
function WideNormalize(const S: WideString; Form: TNormalizationForm): WideString;
function WideSameText(const Str1, Str2: WideString): Boolean;
function WideTitleCase(C: WideChar): WideString; overload;
function WideTitleCase(const S: WideString): WideString; overload;
function WideTrim(const S: WideString): WideString;
function WideTrimLeft(const S: WideString): WideString;
function WideTrimRight(const S: WideString): WideString;
function WideUpperCase(C: WideChar): WideString; overload;
function WideUpperCase(const S: WideString): WideString; overload;

// Low level character routines
function UnicodeNumberLookup(Code: UCS4; var Number: TUcNumber): Boolean;
function UnicodeCompose(const Codes: array of UCS4; var Composite: UCS4): Integer;
function UnicodeCaseFold(Code: UCS4): TUCS4Array;
function UnicodeToUpper(Code: UCS4): TUCS4Array;
function UnicodeToLower(Code: UCS4): TUCS4Array;
function UnicodeToTitle(Code: UCS4): TUCS4Array;

// Character test routines
function UnicodeIsAlpha(C: UCS4): Boolean;
function UnicodeIsDigit(C: UCS4): Boolean;
function UnicodeIsAlphaNum(C: UCS4): Boolean;
function UnicodeIsCased(C: UCS4): Boolean;
function UnicodeIsControl(C: UCS4): Boolean;
function UnicodeIsSpace(C: UCS4): Boolean;
function UnicodeIsWhiteSpace(C: UCS4): Boolean;
function UnicodeIsBlank(C: UCS4): Boolean;
function UnicodeIsPunctuation(C: UCS4): Boolean;
function UnicodeIsGraph(C: UCS4): Boolean;
function UnicodeIsPrintable(C: UCS4): Boolean;
function UnicodeIsUpper(C: UCS4): Boolean;
function UnicodeIsLower(C: UCS4): Boolean;
function UnicodeIsTitle(C: UCS4): Boolean;
function UnicodeIsHexDigit(C: UCS4): Boolean;
function UnicodeIsIsoControl(C: UCS4): Boolean;
function UnicodeIsFormatControl(C: UCS4): Boolean;
function UnicodeIsSymbol(C: UCS4): Boolean;
function UnicodeIsNumber(C: UCS4): Boolean;
function UnicodeIsNonSpacing(C: UCS4): Boolean;
function UnicodeIsOpenPunctuation(C: UCS4): Boolean;
function UnicodeIsClosePunctuation(C: UCS4): Boolean;
function UnicodeIsInitialPunctuation(C: UCS4): Boolean;
function UnicodeIsFinalPunctuation(C: UCS4): Boolean;
function UnicodeIsComposed(C: UCS4): Boolean;
function UnicodeIsQuotationMark(C: UCS4): Boolean;
function UnicodeIsSymmetric(C: UCS4): Boolean;
function UnicodeIsMirroring(C: UCS4): Boolean;
function UnicodeIsNonBreaking(C: UCS4): Boolean;

// Directionality functions
function UnicodeIsRightToLeft(C: UCS4): Boolean;
function UnicodeIsLeftToRight(C: UCS4): Boolean;
function UnicodeIsStrong(C: UCS4): Boolean;
function UnicodeIsWeak(C: UCS4): Boolean;
function UnicodeIsNeutral(C: UCS4): Boolean;
function UnicodeIsSeparator(C: UCS4): Boolean;

// Other character test functions
function UnicodeIsMark(C: UCS4): Boolean;
function UnicodeIsModifier(C: UCS4): Boolean;
function UnicodeIsLetterNumber(C: UCS4): Boolean;
function UnicodeIsConnectionPunctuation(C: UCS4): Boolean;
function UnicodeIsDash(C: UCS4): Boolean;
function UnicodeIsMath(C: UCS4): Boolean;
function UnicodeIsCurrency(C: UCS4): Boolean;
function UnicodeIsModifierSymbol(C: UCS4): Boolean;
function UnicodeIsNonSpacingMark(C: UCS4): Boolean;
function UnicodeIsSpacingMark(C: UCS4): Boolean;
function UnicodeIsEnclosing(C: UCS4): Boolean;
function UnicodeIsPrivate(C: UCS4): Boolean;
function UnicodeIsSurrogate(C: UCS4): Boolean;
function UnicodeIsLineSeparator(C: UCS4): Boolean;
function UnicodeIsParagraphSeparator(C: UCS4): Boolean;
function UnicodeIsIdentifierStart(C: UCS4): Boolean;
function UnicodeIsIdentifierPart(C: UCS4): Boolean;
function UnicodeIsDefined(C: UCS4): Boolean;
function UnicodeIsUndefined(C: UCS4): Boolean;
function UnicodeIsHan(C: UCS4): Boolean;
function UnicodeIsHangul(C: UCS4): Boolean;

// Utility functions
function CharSetFromLocale(Language: LCID): Byte;
function GetCharSetFromLocale(Language: LCID; out FontCharSet: Byte): Boolean;
function CodePageFromLocale(Language: LCID): Integer;
function CodeBlockName(const CB: TUnicodeBlock): string;
function CodeBlockRange(const CB: TUnicodeBlock): TUnicodeBlockRange;
function CodeBlockFromChar(const C: UCS4): TUnicodeBlock;
function KeyboardCodePage: Word;
function KeyUnicode(C: Char): WideChar;
function StringToWideStringEx(const S: string; CodePage: Word): WideString;
function TranslateString(const S: string; CP1, CP2: Word): string;
function WideStringToStringEx(const WS: WideString; CodePage: Word): string;

// conversion routines between WideString (handled as UCS-2), UTF-16, UCS-4 and UTF8

// iterative conversions

// UTF8GetNextChar = read next UTF8 sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF8GetNextChar(const S: AnsiString; var StrPos: Integer): UCS4;

// UTF8SkipChars = skip NbSeq UTF8 sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF8 sequences that were skipped
function UTF8SkipChars(const S: AnsiString; var StrPos: Integer; var NbSeq: Integer): Boolean;

// UTF8SetNextChar = append an UTF8 sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: AnsiString; var StrPos: Integer; Ch: UCS4): Boolean;

// UTF16GetNextChar = read next UTF16 sequence at StrPos
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF16GetNextChar(const S: WideString; var StrPos: Integer): UCS4;

// UTF16GetPreviousChar = read previous UTF16 sequence starting at StrPos-1
// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
function UTF16GetPreviousChar(const S: WideString; var StrPos: Integer): UCS4;

// UTF16SkipChars = skip NbSeq UTF16 sequences starting from StrPos
// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbChar contains the number of UTF16 sequences that were skipped
function UTF16SkipChars(const S: WideString; var StrPos: Integer; var NbSeq: Integer): Boolean;

// UTF16SetNextChar = append an UTF16 sequence at StrPos
// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, callee did nothing and caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF16SetNextChar(var S: WideString; var StrPos: Integer; Ch: UCS4): Boolean;

// one shot conversions
procedure ExpandANSIString(const Source: PChar; Target: PWideChar; Count: Cardinal);
function WideStringToUTF8(const S: WideString): AnsiString; // WideString = UCS2
function UTF8ToWideString(const S: AnsiString): WideString; // WideString = UCS2
function WideStringToUTF16(const S: WideString): WideString; // WideString = UCS2
function UTF16ToWideString(const S: WideString): WideString; // WideString = UCS2
function WideStringToUCS4(const S: WideString): TUCS4Array; // WideString = UCS2
function UCS4ToWideString(const S: TUCS4Array): WideString; // WideString = UCS2
function UTF8ToUTF16(const S: AnsiString): WideString;
function UTF16ToUTF8(const S: WideString): AnsiString;
function UTF8ToUCS4(const S: AnsiString): TUCS4Array;
function UCS4ToUTF8(const S: TUCS4Array): AnsiString;
function UTF16ToUCS4(const S: WideString): TUCS4Array;
function UCS4ToUTF16(const S: TUCS4Array): WideString;

// indexed conversions
function UTF8CharCount(const S: AnsiString): Integer;
function UTF16CharCount(const S: WideString): Integer;
function UCS2CharCount(const S: WideString): Integer;
function UCS4CharCount(const S: TUCS4Array): Integer;
// returns False if string is too small
// if UNICODE_SILENT_FAILURE is not defined and an invalid UTFX sequence is detected, an exception is raised
// returns True on success and Value contains UCS4 character that was read
function GetUCS4CharAt(const UTF8Str: AnsiString; Index: Integer; out Value: UCS4): Boolean; overload;
function GetUCS4CharAt(const WideStr: WideString; Index: Integer; out Value: UCS4; IsUTF16: Boolean = False): Boolean; overload;
function GetUCS4CharAt(const UCS4Str: TUCS4Array; Index: Integer; out Value: UCS4): Boolean; overload;

type
  TCompareFunc = function (const W1, W2: WideString; Locale: LCID): Integer;

var
  WideCompareText: TCompareFunc;

type
  EJclUnicodeError = class(EJclError);

// functions to load Unicode data from resource
procedure LoadCharacterCategories;
procedure LoadCaseMappingData;
procedure LoadDecompositionData;
procedure LoadCombiningClassData;
procedure LoadNumberData;
procedure LoadCompositionData;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclUnicode.pas $';
    Revision: '$Revision: 2352 $';
    Date: '$Date: 2008-02-09 23:39:21 +0100 (sam., 09 févr. 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

// Unicode data for case mapping, decomposition, numbers etc. This data is
// loaded on demand which means only those parts will be put in memory which are
// needed by one of the lookup functions.
// Note: There is a little tool called UDExtract which creates a resouce script from
//       the Unicode database file which can be compiled to the needed res file.
//       This tool, including its source code, can be downloaded from www.lischke-online.de/Unicode.html.

{$IFDEF UNICODE_RAW_DATA}
{$R JclUnicode.res}
{$ENDIF UNICODE_RAW_DATA}
{$IFDEF UNICODE_BZIP2_DATA}
{$R JclUnicodeBZip2.res}
{$ENDIF UNICODE_BZIP2_DATA}
{$IFDEF UNICODE_ZLIB_DATA}
{$R JclUnicodeZLib.res}
{$ENDIF UNICODE_ZLIB_DATA}

uses
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RtlConsts,
  {$ELSE}
  {$IFNDEF FPC}
  Consts,
  {$ENDIF ~FPC}
  {$ENDIF HAS_UNIT_RTLCONSTS}
  SysUtils,
  {$IFDEF UNICODE_BZIP2_DATA}
  BZip2,
  {$ENDIF UNICODE_BZIP2_DATA}
  {$IFNDEF UNICODE_RAW_DATA}
  JclStreams,
  JclCompression,
  {$ENDIF ~UNICODE_RAW_DATA}
  JclResources, JclSynch, JclSysUtils;

const
  {$IFDEF FPC} // declarations from unit [Rtl]Consts
  SDuplicateString = 'String list does not allow duplicates';
  SListIndexError = 'List index out of bounds (%d)';
  SSortedListError = 'Operation not allowed on sorted string list';
  {$ENDIF FPC}
  // some predefined sets to shorten parameter lists below and ease repeative usage
  ClassLetter = [ccLetterUppercase, ccLetterLowercase, ccLetterTitlecase, ccLetterModifier, ccLetterOther];
  ClassSpace = [ccSeparatorSpace, ccSpaceOther];
  ClassPunctuation = [ccPunctuationConnector, ccPunctuationDash, ccPunctuationOpen, ccPunctuationClose,
    ccPunctuationOther, ccPunctuationInitialQuote, ccPunctuationFinalQuote];
  ClassMark = [ccMarkNonSpacing, ccMarkSpacingCombining, ccMarkEnclosing];
  ClassNumber = [ccNumberDecimalDigit, ccNumberLetter, ccNumberOther];
  ClassSymbol = [ccSymbolMath, ccSymbolCurrency, ccSymbolModifier, ccSymbolOther];
  ClassEuropeanNumber = [ccEuropeanNumber, ccEuropeanNumberSeparator, ccEuropeanNumberTerminator];

  // used to negate a set of categories
  ClassAll = [Low(TCharacterCategory)..High(TCharacterCategory)];

var
  // As the global data can be accessed by several threads it should be guarded
  // while the data is loaded.
  LoadInProgress: TJclCriticalSection;

function OpenResourceStream(const ResName: string): TStream;
var
  ResourceStream: TStream;
  {$IFNDEF UNICODE_RAW_DATA}
  DecompressionStream: TStream;
  {$ENDIF ~UNICODE_RAW_DATA}
begin
  ResourceStream := TResourceStream.Create(HInstance, ResName, 'UNICODEDATA');
  {$IFDEF UNICODE_RAW_DATA}
  Result := ResourceStream;
  {$ENDIF UNICODE_RAW_DATA}
  {$IFDEF UNICODE_BZIP2_DATA}
  try
    LoadBZip2;
    DecompressionStream := TJclBZIP2DecompressionStream.Create(ResourceStream);
    try
      Result := TMemoryStream.Create;
      StreamCopy(DecompressionStream, Result);
      StreamSeek(Result, 0, soBeginning);
    finally
      DecompressionStream.Free;
    end;
  finally
    ResourceStream.Free;
  end;
  {$ENDIF UNICODE_BZIP2_DATA}
  {$IFDEF UNICODE_ZLIB_DATA}
  try
    DecompressionStream := TJclZLibDecompressStream.Create(ResourceStream);
    try
      Result := TMemoryStream.Create;
      StreamCopy(DecompressionStream, Result);
      StreamSeek(Result, 0, soBeginning);
    finally
      DecompressionStream.Free;
    end;
  finally
    ResourceStream.Free;
  end;
  {$ENDIF UNICODE_ZLIB_DATA}
end;

//----------------- support for character categories -----------------------------------------------

// Character category data is quite a large block since every defined character in Unicode is assigned at least
// one category. Because of this we cannot use a sparse matrix to provide quick access as implemented for
// e.g. composition data.
// The approach used here is based on the fact that an application seldomly uses all characters defined in Unicode
// simultanously. In fact the opposite is true. Most application will use either Western Europe or Arabic or
// Far East character data, but very rarely all together. Based on this fact is the implementation of virtual
// memory using the systems paging file (aka file mapping) to load only into virtual memory what is used currently.
// The implementation is not yet finished and needs a lot of improvements yet.

type
  // start and stop of a range of code points
  TRange = record
    Start,
    Stop: Cardinal;
  end;

  TRangeArray = array of TRange;
  TCategoriesArray = array of array of TCharacterCategories;

var
  // character categories, stored in the system's swap file and mapped on demand
  CategoriesLoaded: Boolean;
  Categories: array [Byte] of TCategoriesArray;

procedure LoadCharacterCategories;
// Loads the character categories data (as saved by the Unicode database extractor, see also
// the comments about JclUnicode.res above).
var
  Size: Integer;
  Stream: TStream;
  Category: TCharacterCategory;
  Buffer: TRangeArray;
  First, Second, Third: Byte;
  J, K: Integer;
begin
  // Data already loaded?
  if not CategoriesLoaded then
  begin
    // make sure no other code is currently modifying the global data area
    LoadInProgress.Enter;
    try
      CategoriesLoaded := True;
      Stream := OpenResourceStream('CATEGORIES');
      try
        while Stream.Position < Stream.Size do
        begin
          // a) read which category is current in the stream
          Stream.ReadBuffer(Category, 1);
          // b) read the size of the ranges and the ranges themself
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(Buffer, Size);
            Stream.ReadBuffer(Buffer[0], Size * SizeOf(TRange));

            // c) go through every range and add the current category to each code point
            for J := 0 to Size - 1 do
              for K := Buffer[J].Start to Buffer[J].Stop do
              begin
                Assert(K < $1000000, LoadResString(@RsCategoryUnicodeChar));

                First := (K shr 16) and $FF;
                Second := (K shr 8) and $FF;
                Third := K and $FF;
                // add second step array if not yet done
                if Categories[First] = nil then
                  SetLength(Categories[First], 256);
                if Categories[First, Second] = nil then
                  SetLength(Categories[First, Second], 256);
                Include(Categories[First, Second, Third], Category);
              end;
          end;
        end;
      finally
        Stream.Free;
      end;
    finally
      LoadInProgress.Leave;
    end;
  end;
end;

function CategoryLookup(Code: Cardinal; Cats: TCharacterCategories): Boolean; overload;
// determines whether the Code is in the given category
var
  First, Second, Third: Byte;
begin
  Assert(Code < $1000000, LoadResString(@RsCategoryUnicodeChar));

  // load property data if not already done
  if not CategoriesLoaded then
    LoadCharacterCategories;

  First := (Code shr 16) and $FF;
  Second := (Code shr 8) and $FF;
  Third := Code and $FF;
  if (Categories[First] <> nil) and (Categories[First, Second] <> nil) then
    Result := Categories[First, Second, Third] * Cats <> []
  else
    Result := False;
end;

//----------------- support for case mapping -------------------------------------------------------

type
  TCase = array [0..3] of TUCS4Array; // mapping for case fold, lower, title and upper in this order
  TCaseArray = array of array of TCase;

var
  // An array for all case mappings (including 1 to many casing if saved by the extraction program).
  // The organization is a sparse, two stage matrix.
  // SingletonMapping is to quickly return a single default mapping.
  CaseDataLoaded: Boolean;
  CaseMapping: array [Byte] of TCaseArray;
  SingletonMapping: TUCS4Array;

procedure LoadCaseMappingData;
var
  Stream: TStream;
  I, Code,
  Size: Cardinal;
  First, Second, Third: Byte;
begin
  if not CaseDataLoaded then
  begin
    // make sure no other code is currently modifying the global data area
    LoadInProgress.Enter;

    try
      SetLength(SingletonMapping, 1);
      CaseDataLoaded := True;
      Stream := OpenResourceStream('CASE');
      try
        // the first entry in the stream is the number of entries in the case mapping table
        Stream.ReadBuffer(Size, 4);
        for I := 0 to Size - 1 do
        begin
          // a) read actual code point
          Stream.ReadBuffer(Code, 4);

          Assert(Code < $1000000, LoadResString(@RsCasedUnicodeChar));
          // if there is no high byte entry in the first stage table then create one

          First := (Code shr 16) and $FF;
          Second := (Code shr 8) and $FF;
          Third := Code and $FF;
          if CaseMapping[First] = nil then
            SetLength(CaseMapping[First], 256);
          if CaseMapping[First, Second] = nil then
            SetLength(CaseMapping[First, Second], 256);

          // b) read fold case array
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, Third, 0], Size);
            Stream.ReadBuffer(CaseMapping[First, Second, Third, 0, 0], Size * SizeOf(UCS4));
          end;
          // c) read lower case array
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, Third, 1], Size);
            Stream.ReadBuffer(CaseMapping[First, Second, Third, 1, 0], Size * SizeOf(UCS4));
          end;
          // d) read title case array
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, Third, 2], Size);
            Stream.ReadBuffer(CaseMapping[First, Second, Third, 2, 0], Size * SizeOf(UCS4));
          end;
          // e) read upper case array
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, Third, 3], Size);
            Stream.ReadBuffer(CaseMapping[First, Second, Third, 3, 0], Size * SizeOf(UCS4));
          end;
        end;

      finally
        Stream.Free;
      end;
    finally
      LoadInProgress.Leave;
    end;
  end;
end;

function CaseLookup(Code: Cardinal; CaseType: Cardinal): TUCS4Array;
// Performs a lookup of the given code and returns its case mapping if found.
// CaseType must be 0 for case folding, 1 for lower case, 2 for title case and 3 for upper case, respectively.
// If Code could not be found (or there is no case mapping) then the result is a mapping of length 1 with the
// code itself. Otherwise an array of code points is returned which represent the mapping.
var
  First, Second, Third: Byte;
begin
  Assert(Code < $1000000, LoadResString(@RsCasedUnicodeChar));

  // load case mapping data if not already done
  if not CaseDataLoaded then
    LoadCaseMappingData;

  First := (Code shr 16) and $FF;
  Second := (Code shr 8) and $FF;
  Third := Code and $FF;
  // Check first stage table whether there is a mapping for a particular block and
  // (if so) then whether there is a mapping or not.
  if (CaseMapping[First] = nil) or (CaseMapping[First, Second] = nil)
    or (CaseMapping[First, Second, Third, CaseType] = nil) then
  begin
    SingletonMapping[0] := Code;
    Result := SingletonMapping;
  end
  else
    Result := CaseMapping[First, Second, Third, CaseType];
end;

function UnicodeCaseFold(Code: UCS4): TUCS4Array;
// This function returnes an array of special case fold mappings if there is one defined for the given
// code, otherwise the lower case will be returned. This all applies only to cased code points.
// Uncased code points are returned unchanged.
begin
  Result := CaseLookup(Code, 0);
end;

function UnicodeToUpper(Code: UCS4): TUCS4Array;
begin
  Result := CaseLookup(Code, 3);
end;

function UnicodeToLower(Code: UCS4): TUCS4Array;
begin
  Result := CaseLookup(Code, 1);
end;

function UnicodeToTitle(Code: UCS4): TUCS4Array;
begin
  Result := CaseLookup(Code, 2);
end;

//----------------- support for decomposition ------------------------------------------------------

const
  // constants for hangul composition and hangul-to-jamo decomposition
  SBase = $AC00;             // hangul syllables start code point
  LBase = $1100;             // leading syllable
  VBase = $1161;
  TBase = $11A7;             // trailing syllable
  LCount = 19;
  VCount = 21;
  TCount = 28;
  NCount = VCount * TCount;   // 588
  SCount = LCount * NCount;   // 11172

type
  TDecompositions = array of array of TUCS4Array;
  TDecompositionsArray = array [Byte] of TDecompositions;
  
var
  // list of decompositions, organized (again) as three stage matrix
  // Note: there are two tables, one for canonical decompositions and the other one
  //       for compatibility decompositions.
  DecompositionsLoaded: Boolean;
  CanonicalDecompositions,
  CompatibleDecompositions: TDecompositionsArray;

procedure LoadDecompositionData;
var
  Stream: TStream;
  I, Code,
  Size: Cardinal;
  First, Second, Third: Byte;
begin
  if not DecompositionsLoaded then
  begin
    // make sure no other code is currently modifying the global data area
    LoadInProgress.Enter;

    try
      DecompositionsLoaded := True;
      Stream := OpenResourceStream('DECOMPOSITION');
      try
        // determine how many decomposition entries we have
        Stream.ReadBuffer(Size, 4);
        for I := 0 to Size - 1 do
        begin
          Stream.ReadBuffer(Code, 4);

          Assert((Code and not $40000000) < $1000000, LoadResString(@RsDecomposedUnicodeChar));

          // if there is no high byte entry in the first stage table then create one
          First := (Code shr 16) and $FF;
          Second := (Code shr 8) and $FF;
          Third := Code and $FF;

          // insert into the correct table depending on bit 30
          // (if set then it is a compatibility decomposition)
          if Code and $40000000 <> 0 then
          begin
            if CompatibleDecompositions[First] = nil then
              SetLength(CompatibleDecompositions[First], 256);
            if CompatibleDecompositions[First, Second] = nil then
              SetLength(CompatibleDecompositions[First, Second], 256);

            Stream.ReadBuffer(Size, 4);
            if Size > 0 then
            begin
              SetLength(CompatibleDecompositions[First, Second, Third], Size);
              Stream.ReadBuffer(CompatibleDecompositions[First, Second, Third, 0], Size * SizeOf(UCS4));
            end;
          end
          else
          begin
            if CanonicalDecompositions[First] = nil then
              SetLength(CanonicalDecompositions[First], 256);
            if CanonicalDecompositions[First, Second] = nil then
              SetLength(CanonicalDecompositions[First, Second], 256);

            Stream.ReadBuffer(Size, 4);
            if Size > 0 then
            begin
              SetLength(CanonicalDecompositions[First, Second, Third], Size);
              Stream.ReadBuffer(CanonicalDecompositions[First, Second, Third, 0], Size * SizeOf(UCS4));
            end;
          end;
        end;
      finally
        Stream.Free;
      end;
    finally
      LoadInProgress.Leave;
    end;
  end;
end;

function UnicodeDecomposeHangul(Code: UCS4): TUCS4Array;
// algorithmically decomposes hangul character
var
  Rest: Integer;
begin
  Dec(Code, SBase);
  Rest := Code mod TCount;
  if Rest = 0 then
    SetLength(Result, 2)
  else
    SetLength(Result, 3);
  Result[0] := LBase + (Code div NCount);
  Result[1] := VBase + ((Code mod NCount) div TCount);
  if Rest <> 0 then
    Result[2] := TBase + Rest;
end;

function UnicodeDecompose(Code: UCS4; Compatible: Boolean): TUCS4Array;
var
  First, Second, Third: Byte;
begin
  Assert((Code and not $40000000) < $1000000, LoadResString(@RsDecomposedUnicodeChar));

  // load decomposition data if not already done
  if not DecompositionsLoaded then
    LoadDecompositionData;

  Result := nil;

  // if the code is hangul then decomposition is algorithmically
  if UnicodeIsHangul(Code) then
    Result := UnicodeDecomposeHangul(Code)
  else
  begin
    First := (Code shr 16) and $FF;
    Second := (Code shr 8) and $FF;
    Third := Code and $FF;
    if Compatible then
    begin
      // Check first stage table whether there is a particular block and
      // (if so) then whether there is a decomposition or not.
      if (CompatibleDecompositions[First] = nil) or (CompatibleDecompositions[First, Second] = nil)
        or (CompatibleDecompositions[First, Second, Third] = nil) then
      begin
        // if there is no compatibility decompositions try canonical
        if (CanonicalDecompositions[First] = nil) or (CanonicalDecompositions[First, Second] = nil)
          or (CanonicalDecompositions[First, Second, Third] = nil) then
          Result := nil
        else
          Result := CanonicalDecompositions[First, Second, Third];
      end
      else
        Result := CompatibleDecompositions[First, Second, Third];
    end
    else
    begin
      if (CanonicalDecompositions[First] = nil) or (CanonicalDecompositions[First, Second] = nil)
        or (CanonicalDecompositions[First, Second, Third] = nil) then
        Result := nil
      else
        Result := CanonicalDecompositions[First, Second, Third];
    end;
  end;
end;

//----------------- support for combining classes --------------------------------------------------

type
  TClassArray = array of array of Byte;

var
  // canonical combining classes, again as two stage matrix
  CCCsLoaded: Boolean;
  CCCs: array [Byte] of TClassArray;

procedure LoadCombiningClassData;
var
  Stream: TStream;
  I, J, K,
  Size: Cardinal;
  Buffer: TRangeArray;
  First, Second, Third: Byte;
begin
  // make sure no other code is currently modifying the global data area
  LoadInProgress.Enter;

  try
    if not CCCsLoaded then
    begin
      CCCsLoaded := True;
      Stream := OpenResourceStream('COMBINING');
      try
        while Stream.Position < Stream.Size do
        begin
          // a) determine which class is stored here
          Stream.ReadBuffer(I, 4);
          // b) determine how many ranges are assigned to this class
          Stream.ReadBuffer(Size, 4);
          // c) read start and stop code of each range
          if Size > 0 then
          begin
            SetLength(Buffer, Size);
            Stream.ReadBuffer(Buffer[0], Size * SizeOf(TRange));

            // d) put this class in every of the code points just loaded
            for J := 0 to Size - 1 do
              for K := Buffer[J].Start to Buffer[J].Stop do
              begin
                // (outchy) TODO: handle in a cleaner way
                Assert(K < $1000000, LoadResString(@RsCombiningClassUnicodeChar));
                First := (K shr 16) and $FF;
                Second := (K shr 8) and $FF;
                Third := K and $FF;
                // add second step array if not yet done
                if CCCs[First] = nil then
                  SetLength(CCCs[First], 256);
                if CCCs[First, Second] = nil then
                  SetLength(CCCs[First, Second], 256);
                CCCs[First, Second, Third] := I;
              end;
          end;
        end;
      finally
        Stream.Free;
      end;
    end;
  finally
    LoadInProgress.Leave;
  end;
end;

function CanonicalCombiningClass(Code: Cardinal): Cardinal;
var
  First, Second, Third: Byte;
begin
  Assert(Code < $1000000, LoadResString(@RsCombiningClassUnicodeChar));

  // load combining class data if not already done
  if not CCCsLoaded then
    LoadCombiningClassData;

  First := (Code shr 16) and $FF;
  Second := (Code shr 8) and $FF;
  Third := Code and $FF;
  if (CCCs[First] <> nil) and (CCCs[First, Second] <> nil) then
    Result := CCCs[First, Second, Third]
  else
    Result := 0;
end;

//----------------- support for numeric values -----------------------------------------------------

type
  // structures for handling numbers
  TCodeIndex = record
    Code,
    Index: Cardinal;
  end;

var
  // array to hold the number equivalents for specific codes
  NumberCodes: array of TCodeIndex;
  // array of numbers used in NumberCodes
  Numbers: array of TUcNumber;

procedure LoadNumberData;
var
  Stream: TStream;
  Size: Cardinal;
begin
  // make sure no other code is currently modifying the global data area
  LoadInProgress.Enter;

  try
    if NumberCodes = nil then
    begin
      Stream := OpenResourceStream('NUMBERS');
      // Numbers are special (compared to other Unicode data) as they utilize two
      // arrays, one containing all used numbers (in nominator-denominator format) and
      // another one which maps a code point to one of the numbers in the first array.

      // a) determine size of numbers array
      Stream.ReadBuffer(Size, 4);
      SetLength(Numbers, Size);
      // b) read numbers data
      Stream.ReadBuffer(Numbers[0], Size * SizeOf(TUcNumber));
      // c) determine size of index array
      Stream.ReadBuffer(Size, 4);
      SetLength(NumberCodes, Size);
      // d) read index data
      Stream.ReadBuffer(NumberCodes[0], Size * SizeOf(TCodeIndex));
      Stream.Free;
    end;
  finally
    LoadInProgress.Leave;
  end;
end;

function UnicodeNumberLookup(Code: UCS4; var Number: TUcNumber): Boolean;
// Searches for the given code and returns its number equivalent (if there is one).
// Typical cases are: '1/6' (U+2159), '3/8' (U+215C), 'XII' (U+216B) etc.
// Result is set to True if the code could be found.
var
  L, R, M: Integer;
begin
  // load number data if not already done
  if NumberCodes = nil then
    LoadNumberData;

  Result := False;
  L := 0;
  R := High(NumberCodes);
  while L <= R do
  begin
    M := (L + R) shr 1;
    if Code > NumberCodes[M].Code then
      L := M + 1
    else
    begin
      if Code < NumberCodes[M].Code then
        R := M - 1
      else
      begin
        Number := Numbers[NumberCodes[M].Index];
        Result := True;
        Break;
      end;
    end;
  end;
end;

//----------------- support for composition --------------------------------------------------------

type
  // maps between a pair of code points to a composite code point
  // Note: the source pair is packed into one 4 byte value to speed up search. 
  TComposition = record
    Code: Cardinal;
    First: Cardinal;
    Next: array of Cardinal;
  end;

var
  // list of composition mappings
  Compositions: array of TComposition;
  MaxCompositionSize: Integer;

procedure LoadCompositionData;
var
  Stream: TStream;
  I, Size: Integer;
begin
  // make sure no other code is currently modifying the global data area
  LoadInProgress.Enter;

  try
    if Compositions = nil then
    begin
      Stream := OpenResourceStream('COMPOSITION');
      try
        // a) determine size of compositions array
        Stream.ReadBuffer(Size, 4);
        SetLength(Compositions, Size);
        // b) read data
        for I := 0 to Size - 1 do
        begin
          Stream.ReadBuffer(Compositions[I].Code, 4);
          Stream.ReadBuffer(Size, 4);
          if Size > MaxCompositionSize then
            MaxCompositionSize := Size;
          SetLength(Compositions[I].Next, Size - 1);
          Stream.ReadBuffer(Compositions[I].First, 4);
          Stream.ReadBuffer(Compositions[I].Next[0], 4 * (Size - 1));
        end;
      finally
        Stream.Free;
      end;
    end;
  finally
    LoadInProgress.Leave;
  end;
end;

function UnicodeCompose(const Codes: array of UCS4; var Composite: UCS4): Integer;
// Maps the sequence of Codes (up to MaxCompositionSize codes) to a composite
// Result is the number of Codes that were composed (at least 1 if Codes is not empty)
var
  L, R, M, I, HighCodes, HighNext: Integer;
begin
  if Compositions = nil then
    LoadCompositionData;

  Result := 0;
  HighCodes := High(Codes);

  if HighCodes = -1 then
    Exit;

  if HighCodes = 0 then
  begin
    Result := 1;
    Composite := Codes[0];
    Exit;
  end;

  L := 0;
  R := High(Compositions);

  while L <= R do
  begin
    M := (L + R) shr 1;
    if Compositions[M].First > Codes[0] then
      R := M - 1
    else
    if Compositions[M].First < Codes[0] then
      L := M + 1
    else
    begin
      // back to the first element where Codes[0] = First
      while (M > 0) and (Compositions[M-1].First = Codes[0]) do
        Dec(M);

      while (M <= High(Compositions)) and (Compositions[M].First = Codes[0]) do
      begin
        HighNext := High(Compositions[M].Next);
        Result := 0;

        if HighNext < HighCodes then // enough characters in buffer to be tested
        begin
          for I := 0 to HighNext do
            if Compositions[M].Next[I] = Codes[I + 1] then
              Result := I + 2 { +1 for first, +1 because of 0-based array }
            else
              Break;

          if Result = HighNext + 2 then // all codes matched
          begin
            Composite := Compositions[M].Code;
            Exit;
          end;
        end;

        Inc(M);
      end;
      Break;
    end;
  end;
  Result := 1;
  Composite := Codes[0];
end;

//=== { TSearchEngine } ======================================================

constructor TSearchEngine.Create(AOwner: TWideStrings);
begin
  FOwner := AOwner;
  FResults := TList.Create;
end;

destructor TSearchEngine.Destroy;
begin
  Clear;
  FResults.Free;
  inherited Destroy;
end;

procedure TSearchEngine.AddResult(Start, Stop: Cardinal);
begin
  FResults.Add(Pointer(Start));
  FResults.Add(Pointer(Stop));
end;

procedure TSearchEngine.Clear;
begin
  ClearResults;
end;

procedure TSearchEngine.ClearResults;
begin
  FResults.Clear;
end;

procedure TSearchEngine.DeleteResult(Index: Cardinal);
// explicitly deletes a search result
begin
  with FResults do
  begin
    // start index
    Delete(2 * Index);
    // stop index
    Delete(2 * Index);
  end;
end;

function TSearchEngine.GetCount: Integer;
// returns the number of matches found
begin
  Result := FResults.Count div 2;
end;

procedure TSearchEngine.GetResult(Index: Cardinal; var Start, Stop: Integer);
// returns the start position of a match (end position can be determined by
// adding the length of the pattern to the start position)
begin
  Start := Cardinal(FResults[2 * Index]);
  Stop := Cardinal(FResults[2 * Index + 1]);
end;

//----------------- TUTBSearch ---------------------------------------------------------------------

procedure TUTBMSearch.ClearPattern;
begin
  FreeMem(FPattern);
  FPattern := nil;
  FFlags := [];
  FPatternUsed := 0;
  FPatternSize := 0;
  FPatternLength := 0;
  FreeMem(FSkipValues);
  FSkipValues := nil;
  FSkipsUsed := 0;
  FMD4 := 0;
end;

function TUTBMSearch.GetSkipValue(TextStart, TextEnd: PUCS2): Cardinal;
// looks up the SkipValues value for a character
var
  I: Integer;
  C1,
  C2: UCS4;
  Sp: PUTBMSkip;
begin
  Result := 0;
  if Cardinal(TextStart) < Cardinal(TextEnd) then
  begin
    C1 := UCS4(TextStart^);
    if (TextStart + 1) < TextEnd then
      C2 := UCS4((TextStart + 1)^)
    else
      C2 := $FFFFFFFF;
    if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
       (SurrogateLowStart <= C2) and (C2 <= $DDDD) then
      C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));

    Sp := FSkipValues;
    for I := 0 to FSkipsUsed - 1 do
    begin
      if not (Boolean(C1 xor Sp.BMChar.UpCase) and
              Boolean(C1 xor Sp.BMChar.LoCase) and
              Boolean(C1 xor Sp.BMChar.TitleCase)) then
      begin
        if (TextEnd - TextStart) < Sp.SkipValues then
          Result := TextEnd - TextStart
        else
          Result := Sp.SkipValues;
        Exit;
      end;
      Inc(Sp);
    end;
    Result := FPatternLength;
  end;
end;

function TUTBMSearch.Match(Text, Start, Stop: PUCS2; var MatchStart, MatchEnd: Cardinal): Boolean;
// Checks once whether the text at position Start (which points to the end of the
// current text part to be matched) matches.
// Note: If whole words only are allowed then the left and right border tests are
//       done here too. The keypoint for the right border is that the next character
//       after the search string is either the text end or a space character.
//       For the left side this is similar, but there is nothing like a string
//       start marker (like the string end marker #0).
//
//       It seems not obvious, but we still can use the passed Text pointer to do
//       the left check. Although this pointer might not point to the real string
//       start (e.g. in TUTBMSearch.FindAll Text is incremented as needed) it is
//       still a valid check mark. The reason is that Text either points to the
//       real string start or a previous match (happend already, keep in mind the
//       search options do not change in the FindAll loop) and the character just
//       before Text is a space character.
//       This fact implies, though, that strings passed to Find (or FindFirst,
//       FindAll in TUTBMSearch) always really start at the given address. Although
//       this might not be the case in some circumstances (e.g. if you pass only
//       the selection from an editor) it is still assumed that a pattern matching
//       from the first position on (from the search string start) also matches
//       when whole words only are allowed.
var
  CheckSpace: Boolean;
  C1, C2: UCS4;
  Count: Integer;
  Cp: PUTBMChar;
begin
  // be pessimistic
  Result := False;

  // set the potential match endpoint first
  MatchEnd := (Start - Text) + 1;

  C1 := UCS4(Start^);
  if (Start + 1) < Stop then
    C2 := UCS4((Start + 1)^)
  else
    C2 := $FFFFFFFF;
  if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
     (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) then
  begin
    C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
    // Adjust the match end point to occur after the UTF-16 character.
    Inc(MatchEnd);
  end;

  // check special cases
  if FPatternUsed = 1 then
  begin
    MatchStart := Start - Text;
    Result := True;
    Exit;
  end;

  // Early out if entire words need to be matched and the next character
  // in the search string is neither the string end nor a space character.
  if (sfWholeWordOnly in FFlags) and
     not ((Start + 1)^ = WideNull) and
     not UnicodeIsWhiteSpace(UCS4((Start + 1)^)) then
    Exit;

  // compare backward
  Cp := FPattern;
  Inc(Cp, FPatternUsed - 1);

  Count := FPatternLength;
  while (Start >= Text) and (Count > 0) do
  begin
    // ignore non-spacing characters if indicated
    if sfIgnoreNonSpacing in FFlags then
    begin
      while (Start > Text) and UnicodeIsNonSpacing(C1) do
      begin
        Dec(Start);
        C2 := UCS4(Start^);
        if (Start - 1) > Text then
          C1 := UCS4((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
    end;

    // handle space compression if indicated
    if sfSpaceCompress in FFlags then
    begin
      CheckSpace := False;
      while (Start > Text) and (UnicodeIsWhiteSpace(C1) or UnicodeIsControl(C1)) do
      begin
        CheckSpace := UnicodeIsWhiteSpace(C1);
        Dec(Start);
        C2 := UCS4(Start^);
        if (Start - 1) > Text then
          C1 := UCS4((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
      // Handle things if space compression was indicated and one or
      // more member characters were found.
      if CheckSpace then
      begin
        if Cp.UpCase <> $20 then
          Exit;
        Dec(Cp);
        Dec(Count);
        // If Count is 0 at this place then the space character(s) was the first
        // in the pattern and we need to correct the start position.
        if Count = 0 then
          Inc(Start);
      end;
    end;

    // handle the normal comparison cases
    if (Count > 0) and
       (Boolean(C1 xor Cp.UpCase) and
        Boolean(C1 xor Cp.LoCase) and
        Boolean(C1 xor Cp.TitleCase)) then
      Exit;

    if C1 >= $10000 then
      Dec(Count, 2)
    else
      Dec(Count, 1);
    if Count > 0 then
    begin
      Dec(Cp);
      // get the next preceding character
      if Start > Text then
      begin
        Dec(Start);
        C2 := UCS4(Start^);
        if (Start - 1) > Text then
          C1 := UCS4((Start - 1)^)
        else
          C1 := $FFFFFFFF;
        if (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) and
           (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) then
        begin
          C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));
          Dec(Start);
        end
        else
          C1 := C2;
      end;
    end;
  end;

  // So far the string matched. Now check its left border for a space character
  // if whole word only are allowed.
  if not (sfWholeWordOnly in FFlags) or
     (Start <= Text) or
     UnicodeIsWhiteSpace(UCS4((Start - 1)^)) then
  begin
    // set the match start position
    MatchStart := Start - Text;
    Result := True;
  end;
end;

procedure TUTBMSearch.Compile(Pattern: PUCS2; PatternLength: Integer; Flags: TSearchFlags);
var
  HaveSpace: Boolean;
  I, J, K,
  SLen: Integer;
  Cp: PUTBMChar;
  Sp: PUTBMSkip;
  C1, C2,
  Sentinel: UCS4;
begin
  if (Pattern <> nil) and (Pattern^ <> #0) and (PatternLength > 0) then
  begin
    // do some initialization
    FFlags := Flags;
    // extra skip flag
    FMD4 := 1;

    Sentinel := 0;

    // allocate more storage if necessary
    FPattern := AllocMem(SizeOf(TUTBMChar) * PatternLength);
    FSkipValues := AllocMem(SizeOf(TUTBMSkip) * PatternLength);
    FPatternSize := PatternLength;

    // Preprocess the pattern to remove controls (if specified) and determine case.
    Cp := FPattern;
    I := 0;
    HaveSpace := False;
    while I < PatternLength do
    begin
      C1 := UCS4(Pattern[I]);
      if (I + 1) < PatternLength then
        C2 := UCS4(Pattern[I + 1])
      else
        C2 := $FFFFFFFF;
      if (SurrogateHighStart <= C1) and (C1 <= SurrogateHighEnd) and
         (SurrogateLowStart <= C2) and (C2 <= SurrogateLowEnd) then
        C1 := $10000 + (((C1 and $03FF) shl 10) or (C2 and $03FF));

      // Make sure the HaveSpace flag is turned off if the character is not an
      // appropriate one.
      if not UnicodeIsWhiteSpace(C1) then
        HaveSpace := False;

      // If non-spacing characters should be ignored, do it here.
      if (sfIgnoreNonSpacing in Flags) and UnicodeIsNonSpacing(C1) then
      begin
        Inc(I);
        Continue;
      end;

      // check if spaces and controls need to be compressed
      if sfSpaceCompress in Flags then
      begin
        if UnicodeIsWhiteSpace(C1) then
        begin
          if not HaveSpace then
          begin
            // Add a space and set the flag.
            Cp.UpCase := $20;
            Cp.LoCase := $20;
            Cp.TitleCase := $20;
            Inc(Cp);

            // increase the real pattern length
            Inc(FPatternLength);
            Sentinel := $20;
            HaveSpace := True;
          end;
          Inc(I);
          Continue;
        end;

        // ignore all control characters
        if UnicodeIsControl(C1) then
        begin
          Inc(I);
          Continue;
        end;
      end;

      // add the character
      if not (sfCaseSensitive in Flags) then
      begin
        { TODO : use the entire mapping, not only the first character }
        Cp.UpCase := UnicodeToUpper(C1)[0];
        Cp.LoCase := UnicodeToLower(C1)[0];
        Cp.TitleCase := UnicodeToTitle(C1)[0];
      end
      else
      begin
        Cp.UpCase := C1;
        Cp.LoCase := C1;
        Cp.TitleCase := C1;
      end;

      Sentinel := Cp.UpCase;

      // move to the next character
      Inc(Cp);

      // increase the real pattern length appropriately
      if C1 >= $10000 then
        Inc(FPatternLength, 2)
      else
        Inc(FPatternLength);

      // increment the loop index for UTF-16 characters
      if C1 > $10000 then
        Inc(I, 2)
      else
        Inc(I);
    end;

    // set the number of characters actually used
    FPatternUsed := (PChar(Cp) - PChar(FPattern)) div SizeOf(TUTBMChar);

    // Go through and construct the skip array and determine the actual length
    // of the pattern in UCS2 terms.
    SLen := FPatternLength - 1;
    Cp := FPattern;
    K := 0;
    for I := 0 to FPatternUsed - 1 do
    begin
      // locate the character in the FSkipValues array
      Sp := FSkipValues;
      J := 0;
      while (J < FSkipsUsed) and (Sp.BMChar.UpCase <> Cp.UpCase) do
      begin
        Inc(J);
        Inc(Sp);
      end;

      // If the character is not found, set the new FSkipValues element and
      // increase the number of FSkipValues elements.
      if J = FSkipsUsed then
      begin
        Sp.BMChar := Cp;
        Inc(FSkipsUsed);
      end;

      // Set the updated FSkipValues value.  If the character is UTF-16 and is
      // not the last one in the pattern, add one to its FSkipValues value.
      Sp.SkipValues := SLen - K;
      if (Cp.UpCase >= $10000) and ((K + 2) < SLen) then
        Inc(Sp.SkipValues);

      // set the new extra FSkipValues for the sentinel character
      if ((Cp.UpCase >= $10000) and
          ((K + 2) <= SLen) or ((K + 1) <= SLen) and
          (Cp.UpCase = Sentinel)) then
        FMD4 := SLen - K;

      // increase the actual index
      if Cp.UpCase >= $10000 then
        Inc(K, 2)
      else
        Inc(K);
      Inc(Cp);
    end;
  end;
end;

function TUTBMSearch.Find(Text: PUCS2; TextLen: Cardinal; var MatchStart, MatchEnd: Cardinal): Boolean;
// this is the main matching routine using a tuned Boyer-Moore algorithm
var
  K: Cardinal;
  Start,
  Stop: PUCS2;
begin
  Result := False;
  if (FPattern <> nil) and (FPatternUsed > 0) and (Text <> nil) and
     (TextLen > 0) and (TextLen >= FPatternLength) then
  begin
    Start := Text + FPatternLength - 1;
    Stop := Text + TextLen;

    // adjust the start point if it points to a low surrogate
    if (SurrogateLowStart <= UCS4(Start^)) and
       (UCS4(Start^) <= SurrogateLowEnd) and
       (SurrogateHighStart <= UCS4((Start - 1)^)) and
       (UCS4((Start - 1)^) <= SurrogateHighEnd) then
      Dec(Start);

    while Start < Stop do
    begin
      repeat
        K := GetSkipValue(Start, Stop);
        if K = 0 then
          Break;
        Inc(Start, K);
        if (Start < Stop) and
           (SurrogateLowStart <= UCS4(Start^)) and
           (UCS4(Start^) <= SurrogateLowEnd) and
           (SurrogateHighStart <= UCS4((Start - 1)^)) and
           (UCS4((Start - 1)^) <= SurrogateHighEnd) then
          Dec(Start);
      until False;

      if (Start < Stop) and Match(Text, Start, Stop, MatchStart, MatchEnd) then
      begin
        Result := True;
        Break;
      end;
      Inc(Start, FMD4);
      if (Start < Stop) and
         (SurrogateLowStart <= UCS4(Start^)) and
         (UCS4(Start^) <= SurrogateLowEnd) and
         (SurrogateHighStart <= UCS4((Start - 1)^)) and
         (UCS4((Start - 1)^) <= SurrogateHighEnd) then
        Dec(Start);
    end;
  end;
end;

procedure TUTBMSearch.Clear;
begin
  ClearPattern;
  inherited Clear;
end;

function TUTBMSearch.FindAll(const Text: WideString): Boolean;
begin
  Result := FindAll(PWideChar(Text), Length(Text));
end;

function TUTBMSearch.FindAll(Text: PWideChar; TextLen: Cardinal): Boolean;
// Looks for all occurences of the pattern passed to FindPrepare and creates an
// internal list of their positions.
var
  Start, Stop: Cardinal;
  Run: PWideChar;
  RunLen: Cardinal;
begin
  ClearResults;
  Run := Text;
  RunLen := TextLen;
  // repeat to find all occurences of the pattern
  while Find(Run, RunLen, Start, Stop) do
  begin
    // store this result (consider text pointer movement)...
    AddResult(Start + Run - Text, Stop + Run - Text);
    // ... and advance text position and length
    Inc(Run, Stop);
    Dec(RunLen, Stop);
  end;
  Result := Count > 0;
end;

function TUTBMSearch.FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean;
// Looks for the first occurence of the pattern passed to FindPrepare in Text and
// returns True if one could be found (in which case Start and Stop are set to
// the according indices) otherwise False. This function is in particular of
// interest if only one occurence needs to be found.
begin
  ClearResults;
  Result := Find(PWideChar(Text), Length(Text), Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

function TUTBMSearch.FindFirst(Text: PWideChar; TextLen: Cardinal; var Start, Stop: Cardinal): Boolean;
// Same as the WideString version of this method.
begin
  ClearResults;
  Result := Find(Text, TextLen, Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

procedure TUTBMSearch.FindPrepare(const Pattern: WideString; Options: TSearchFlags);
begin
  FindPrepare(PWideChar(Pattern), Length(Pattern), Options);
end;

procedure TUTBMSearch.FindPrepare(Pattern: PWideChar; PatternLength: Cardinal; Options: TSearchFlags);
// prepares following search by compiling the given pattern into an internal structure
begin
  Compile(Pattern, PatternLength, Options);
end;

//----------------- Unicode RE search core ---------------------------------------------------------

const
  // error codes
  _URE_OK = 0;
  _URE_UNEXPECTED_EOS = -1;
  _URE_CCLASS_OPEN = -2;
  _URE_UNBALANCED_GROUP = -3;
  _URE_INVALID_PROPERTY = -4;
  _URE_INVALID_RANGE = -5;
  _URE_RANGE_OPEN = -6;

  // options that can be combined for searching
  URE_IGNORE_NONSPACING = $01;
  URE_DONT_MATCHES_SEPARATORS = $02;

const
  // Flags used internally in the DFA
  _URE_DFA_CASEFOLD = $01;
  _URE_DFA_BLANKLINE = $02;

  // symbol types for the DFA
  _URE_ANY_CHAR = 1;
  _URE_CHAR = 2;
  _URE_CCLASS = 3;
  _URE_NCCLASS = 4;
  _URE_BOL_ANCHOR = 5;
  _URE_EOL_ANCHOR = 6;

  // op codes for converting the NFA to a DFA
  _URE_SYMBOL = 10;
  _URE_PAREN = 11;
  _URE_QUEST = 12;
  _URE_STAR = 13;
  _URE_PLUS = 14;
  _URE_ONE = 15;
  _URE_AND = 16;
  _URE_OR = 17;

  _URE_NOOP = $FFFF;

//----------------- TURESearch ---------------------------------------------------------------------

procedure TURESearch.Clear;
begin
  inherited Clear;
  ClearUREBuffer;
  ClearDFA;
end;

procedure TURESearch.Push(V: Cardinal);
begin
  with FUREBuffer do
  begin
    // If the 'Reducing' parameter is True, check to see if the value passed is
    // already on the stack.
    if Reducing and ExpressionList.Expressions[Word(V)].OnStack then
      Exit;

    if Stack.ListUsed = Length(Stack.List) then
      SetLength(Stack.List, Length(Stack.List) + 8);
    Stack.List[Stack.ListUsed] := V;
    Inc(Stack.ListUsed);

    // If the 'reducing' parameter is True, flag the element as being on the Stack.
    if Reducing then
      ExpressionList.Expressions[Word(V)].OnStack := True;
  end;
end;

function TURESearch.Peek: Cardinal;
begin
  if FUREBuffer.Stack.ListUsed = 0 then
    Result := _URE_NOOP
  else
    Result := FUREBuffer.Stack.List[FUREBuffer.Stack.ListUsed - 1];
end;

function TURESearch.Pop: Cardinal;
begin
  if FUREBuffer.Stack.ListUsed = 0 then
    Result := _URE_NOOP
  else
  begin
    Dec(FUREBuffer.Stack.ListUsed);
    Result := FUREBuffer.Stack.List[FUREBuffer.Stack.ListUsed];
    if FUREBuffer.Reducing then
      FUREBuffer.ExpressionList.Expressions[Word(Result)].OnStack := False;
  end;
end;

function TURESearch.ParsePropertyList(Properties: PUCS2; Limit: Cardinal;
  var Categories: TCharacterCategories): Cardinal;
// Parse a comma-separated list of integers that represent character properties.
// Combine them into a set of categories and return the number of characters consumed.
var
  N: Cardinal;
  Run,
  ListEnd: PUCS2;
begin
  Run := Properties;
  ListEnd := Run + Limit;

  N := 0;
  Categories := [];
  while (FUREBuffer.Error = _URE_OK) and (Run < ListEnd) do
  begin
    if Run^ = ',' then
    begin
      // Encountered a comma, so take the number parsed so far as category and
      // reset the number.
      Include(Categories, TCharacterCategory(N));
      N := 0;
    end
    else
    begin
      if (Run^ >= '0') and (Run^ <= '9') then
      begin
        // Encountered a digit, so start or continue building the cardinal that
        // represents the character category.
        N := (N * 10) + Cardinal(Word(Run^) - Ord('0'));
      end
      else
      begin
        // Encountered something that is not part of the property list.
        // Indicate that we are done.
        Break;
      end;
    end;

    // If the number is to large then there is a problem.
    // Most likely a missing comma separator.
    if Integer(N) > Ord(High(TCharacterCategory)) then
      FUREBuffer.Error := _URE_INVALID_PROPERTY;
    Inc(Run);
  end;

  // Return the number of characters consumed.
  Result := Run - Properties;
end;

function TURESearch.MakeHexNumber(NP: PUCS2; Limit: Cardinal; var Number: Cardinal): Cardinal;
// Collect a hex number with 1 to 4 digits and return the number of characters used.
var
  I: Integer;
  Run,
  ListEnd: PUCS2;
begin
  Run := np;
  ListEnd := Run + Limit;

  Number := 0;
  I := 0;
  while (I < 4) and (Run < ListEnd) do
  begin
    if (Run^ >= '0') and (Run^ <= '9') then
      Number := (Number shl 4) + Cardinal(Word(Run^) - Ord('0'))
    else
    begin
      if (Run^ >= 'A') and (Run^ <= 'F') then
        Number := (Number shl 4) + Cardinal(Word(Run^) - Ord('A')) + 10
      else
      begin
        if (Run^ >= 'a') and (Run^ <= 'f') then
          Number := (Number shl 4) + Cardinal(Word(Run^) - Ord('a')) + 10
        else
          Break;
      end;
    end;
    Inc(I);
    Inc(Run);
  end;

  Result := Run - NP;
end;

procedure TURESearch.AddRange(var CCL: TUcCClass; Range: TUcRange);
// Insert a Range into a character class, removing duplicates and ordering them
// in increasing Range-start order.
var
  I: Integer;
  Temp: UCS4;
begin
  // If the `Casefold' flag is set, then make sure both endpoints of the Range
  // are converted to lower.
  if (FUREBuffer.Flags and _URE_DFA_CASEFOLD) <> 0 then
  begin
    { TODO : use the entire mapping, not only the first character }
    Range.MinCode := UnicodeToLower(Range.MinCode)[0];
    Range.MaxCode := UnicodeToLower(Range.MaxCode)[0];
  end;

  // Swap the Range endpoints if they are not in increasing order.
  if Range.MinCode > Range.MaxCode then
  begin
    Temp := Range.MinCode;
    Range.MinCode := Range.MaxCode;
    Range.MaxCode := Temp;
  end;

  I := 0;
  while (I < CCL.RangesUsed) and (Range.MinCode < CCL.Ranges[I].MinCode) do
    Inc(I);

  // check for a duplicate
  if (I < CCL.RangesUsed) and (Range.MinCode = CCL.Ranges[I].MinCode) and
    (Range.MaxCode = CCL.Ranges[I].MaxCode) then
    Exit;

  if CCL.RangesUsed = Length(CCL.Ranges) then
    SetLength(CCL.Ranges, Length(CCL.Ranges) + 8);

  if I < CCL.RangesUsed then
    Move(CCL.Ranges[I], CCL.Ranges[I + 1], SizeOf(TUcRange) * (CCL.RangesUsed - I));

  CCL.Ranges[I].MinCode := Range.MinCode;
  CCL.Ranges[I].MaxCode := Range.MaxCode;
  Inc(CCL.RangesUsed);
end;

type
  PTrie = ^TTrie;
  TTrie = record
    Key: UCS2;
    Len,
    Next: Cardinal;
    Setup: Integer;
    Categories: TCharacterCategories;
  end;

procedure TURESearch.SpaceSetup(Symbol: PUcSymbolTableEntry; Categories: TCharacterCategories);
var
  Range: TUcRange;
begin
  Symbol.Categories := Symbol.Categories + Categories;

  Range.MinCode := UCS4(WideTabulator);
  Range.MaxCode := UCS4(WideTabulator);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4(WideCarriageReturn);
  Range.MaxCode := UCS4(WideCarriageReturn);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4(WideLineFeed);
  Range.MaxCode := UCS4(WideLineFeed);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4(WideFormFeed);
  Range.MaxCode := UCS4(WideFormFeed);
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := $FEFF;
  Range.MaxCode := $FEFF;
  AddRange(Symbol.Symbol.CCL, Range);
end;

procedure TURESearch.HexDigitSetup(Symbol: PUcSymbolTableEntry);
var
  Range: TUcRange;
begin
  Range.MinCode := UCS4('0');
  Range.MaxCode := UCS4('9');
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4('A');
  Range.MaxCode := UCS4('F');
  AddRange(Symbol.Symbol.CCL, Range);
  Range.MinCode := UCS4('a');
  Range.MaxCode := UCS4('f');
  AddRange(Symbol.Symbol.CCL, Range);
end;

const
  CClassTrie: array [0..64] of TTrie = (
    (Key: #$003A; Len: 1; Next:  1; Setup: 0; Categories: []),
    (Key: #$0061; Len: 9; Next: 10; Setup: 0; Categories: []),
    (Key: #$0063; Len: 8; Next: 19; Setup: 0; Categories: []),
    (Key: #$0064; Len: 7; Next: 24; Setup: 0; Categories: []),
    (Key: #$0067; Len: 6; Next: 29; Setup: 0; Categories: []),
    (Key: #$006C; Len: 5; Next: 34; Setup: 0; Categories: []),
    (Key: #$0070; Len: 4; Next: 39; Setup: 0; Categories: []),
    (Key: #$0073; Len: 3; Next: 49; Setup: 0; Categories: []),
    (Key: #$0075; Len: 2; Next: 54; Setup: 0; Categories: []),
    (Key: #$0078; Len: 1; Next: 59; Setup: 0; Categories: []),
    (Key: #$006C; Len: 1; Next: 11; Setup: 0; Categories: []),
    (Key: #$006E; Len: 2; Next: 13; Setup: 0; Categories: []),
    (Key: #$0070; Len: 1; Next: 16; Setup: 0; Categories: []),
    (Key: #$0075; Len: 1; Next: 14; Setup: 0; Categories: []),
    (Key: #$006D; Len: 1; Next: 15; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 16; Setup: 1; Categories: ClassLetter + ClassNumber),
    (Key: #$0068; Len: 1; Next: 17; Setup: 0; Categories: []),
    (Key: #$0061; Len: 1; Next: 18; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 19; Setup: 1; Categories: ClassLetter),
    (Key: #$006E; Len: 1; Next: 20; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 21; Setup: 0; Categories: []),
    (Key: #$0072; Len: 1; Next: 22; Setup: 0; Categories: []),
    (Key: #$006C; Len: 1; Next: 23; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 24; Setup: 1; Categories: [ccOtherControl, ccOtherFormat]),
    (Key: #$0069; Len: 1; Next: 25; Setup: 0; Categories: []),
    (Key: #$0067; Len: 1; Next: 26; Setup: 0; Categories: []),
    (Key: #$0069; Len: 1; Next: 27; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 28; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 29; Setup: 1; Categories: ClassNumber),
    (Key: #$0072; Len: 1; Next: 30; Setup: 0; Categories: []),
    (Key: #$0061; Len: 1; Next: 31; Setup: 0; Categories: []),
    (Key: #$0070; Len: 1; Next: 32; Setup: 0; Categories: []),
    (Key: #$0068; Len: 1; Next: 33; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 34; Setup: 1; Categories: ClassMark + ClassNumber + ClassLetter + ClassPunctuation +
      ClassSymbol),
    (Key: #$006F; Len: 1; Next: 35; Setup: 0; Categories: []),
    (Key: #$0077; Len: 1; Next: 36; Setup: 0; Categories: []),
    (Key: #$0065; Len: 1; Next: 37; Setup: 0; Categories: []),
    (Key: #$0072; Len: 1; Next: 38; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 39; Setup: 1; Categories: [ccLetterLowercase]),
    (Key: #$0072; Len: 2; Next: 41; Setup: 0; Categories: []),
    (Key: #$0075; Len: 1; Next: 45; Setup: 0; Categories: []),
    (Key: #$0069; Len: 1; Next: 42; Setup: 0; Categories: []),
    (Key: #$006E; Len: 1; Next: 43; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 44; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 45; Setup: 1; Categories: ClassMark + ClassNumber + ClassLetter + ClassPunctuation +
      ClassSymbol + [ccSeparatorSpace]),
    (Key: #$006E; Len: 1; Next: 46; Setup: 0; Categories: []),
    (Key: #$0063; Len: 1; Next: 47; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 48; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 49; Setup: 1; Categories: ClassPunctuation),
    (Key: #$0070; Len: 1; Next: 50; Setup: 0; Categories: []),
    (Key: #$0061; Len: 1; Next: 51; Setup: 0; Categories: []),
    (Key: #$0063; Len: 1; Next: 52; Setup: 0; Categories: []),
    (Key: #$0065; Len: 1; Next: 53; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 54; Setup: 2; Categories: ClassSpace),
    (Key: #$0070; Len: 1; Next: 55; Setup: 0; Categories: []),
    (Key: #$0070; Len: 1; Next: 56; Setup: 0; Categories: []),
    (Key: #$0065; Len: 1; Next: 57; Setup: 0; Categories: []),
    (Key: #$0072; Len: 1; Next: 58; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 59; Setup: 1; Categories: [ccLetterUppercase]),
    (Key: #$0064; Len: 1; Next: 60; Setup: 0; Categories: []),
    (Key: #$0069; Len: 1; Next: 61; Setup: 0; Categories: []),
    (Key: #$0067; Len: 1; Next: 62; Setup: 0; Categories: []),
    (Key: #$0069; Len: 1; Next: 63; Setup: 0; Categories: []),
    (Key: #$0074; Len: 1; Next: 64; Setup: 0; Categories: []),
    (Key: #$003A; Len: 1; Next: 65; Setup: 3; Categories: [])
  );

function TURESearch.PosixCCL(CP: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
// Probe for one of the POSIX colon delimited character classes in the static trie.
var
  I: Integer;
  N: Cardinal;
  TP: PTrie;
  Run,
  ListEnd: PUCS2;
begin
  Result := 0;
  // If the number of characters left is less than 7,
  // then this cannot be interpreted as one of the colon delimited classes.
  if Limit >= 7 then
  begin
    Run := cp;
    ListEnd := Run + Limit;
    TP := @CClassTrie[0];
    I := 0;
    while (Run < ListEnd) and (I < 8) do
    begin
      N := TP.Len;
      while (N > 0) and (TP.Key <> Run^) do
      begin
        Inc(TP);
        Dec(N);
      end;

      if N = 0 then
      begin
        Result := 0;
        Exit;
      end;

      if (Run^ = ':') and ((I = 6) or (I = 7)) then
      begin
        Inc(Run);
        Break;
      end;
      if (Run + 1) < ListEnd then
        TP := @CClassTrie[TP.Next];
      Inc(I);
      Inc(Run);
    end;

    Result := Run - CP;
    case TP.Setup of
      1:
        Symbol.Categories := Symbol.Categories + TP.Categories;
      2:
        SpaceSetup(Symbol, TP.Categories);
      3:
        HexDigitSetup(Symbol);
    else
      Result := 0;
    end;
  end;
end;

function TURESearch.BuildCharacterClass(CP: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
// Construct a list of ranges and return the number of characters consumed.
var
  RangeEnd: Integer;
  N: Cardinal;
  Run,
  ListEnd: PUCS2;
  C, Last: UCS4;
  Range: TUcRange;
begin
  Run := cp;
  ListEnd := Run + Limit;

  if Run^ = '^' then
  begin
    Symbol.AType := _URE_NCCLASS;
    Inc(Run);
  end
  else
    Symbol.AType := _URE_CCLASS;

  Last := 0;
  RangeEnd := 0;
  while (FUREBuffer.Error = _URE_OK) and (Run < ListEnd) do
  begin
    // Allow for the special case []abc], where the first closing bracket would end an empty
    // character class, which makes no sense. Hence this bracket is treaded literally.
    if (Run^ = ']') and (Symbol.Symbol.CCL.RangesUsed > 0) then
      Break;

    C := UCS4(Run^);
    Inc(Run);

    // escape character
    if C = Ord('\') then
    begin
      if Run = ListEnd then
      begin
        // The EOS was encountered when expecting the reverse solidus to be followed by the character it is escaping.
        // Set an Error code and return the number of characters consumed up to this point.
        FUREBuffer.Error := _URE_UNEXPECTED_EOS;
        Result := Run - CP;
        Exit;
      end;

      C := UCS4(Run^);
      Inc(Run);
      case UCS2(C) of
        'a':
          C := $07;
        'b':
          C := $08;
        'f':
          C := $0C;
        'n':
          C := $0A;
        'R':
          C := $0D;
        't':
          C := $09;
        'v':
          C := $0B;
        'p', 'P':
          begin
            Inc(Run, ParsePropertyList(Run, ListEnd - Run, Symbol.Categories));
            // Invert the bit mask of the properties if this is a negated character class or if 'P' is used to specify
            // a list of character properties that should *not* match in a character class.
            if C = Ord('P') then
              Symbol.Categories := ClassAll - Symbol.Categories;
            Continue;
          end;
        'x', 'X', 'u', 'U':
          begin
            if (Run < ListEnd) and
               ((Run^ >= '0') and (Run^ <= '9') or
                (Run^ >= 'A') and (Run^ <= 'F') or
                (Run^ >= 'a') and (Run^ <= 'f')) then
              Inc(Run, MakeHexNumber(Run, ListEnd - Run, C));
          end;
      end;
    end
    else
    begin
      if C = Ord(':') then
      begin
        // Probe for a POSIX colon delimited character class.
        Dec(Run);
        N := PosixCCL(Run, ListEnd - Run, Symbol);
        if N = 0 then
          Inc(Run)
        else
        begin
          Inc(Run, N);
          Continue;
        end;
      end;
    end;

    // Check to see if the current character is a low surrogate that needs
    // to be combined with a preceding high surrogate.
    if Last <> 0 then
    begin
      if (C >= SurrogateLowStart) and (C <= SurrogateLowEnd) then
      begin
        // Construct the UTF16 character code.
        C := $10000 + (((Last and $03FF) shl 10) or (C and $03FF))
      end
      else
      begin
        // Add the isolated high surrogate to the range.
        if RangeEnd = 1 then
          Range.MaxCode := Last and $FFFF
        else
        begin
          Range.MinCode := Last and $FFFF;
          Range.MaxCode := Last and $FFFF;
        end;

        AddRange(Symbol.Symbol.CCL, Range);
        RangeEnd := 0;
      end;
    end;

    // Clear the Last character code.
    Last := 0;

    // This slightly awkward code handles the different cases needed to construct a range.
    if (C >= SurrogateHighStart) and (C <= SurrogateHighEnd) then
    begin
      // If the high surrogate is followed by a Range indicator, simply add it as the Range start.  Otherwise,
      // save it in  the next character is a low surrogate.
      if Run^ = '-' then
      begin
        Inc(Run);
        Range.MinCode := C;
        RangeEnd := 1;
      end
      else
        Last := C;
    end
    else
    begin
      if RangeEnd = 1 then
      begin
        Range.MaxCode := C;
        AddRange(Symbol.Symbol.CCL, Range);
        RangeEnd := 0;
      end
      else
      begin
        Range.MinCode := C;
        Range.MaxCode := C;
        if Run^ = '-' then
        begin
          Inc(Run);
          RangeEnd := 1;
        end
        else
          AddRange(Symbol.Symbol.CCL, Range);
      end;
    end;
  end;

  if (Run < ListEnd) and (Run^ = ']') then
    Inc(Run)
  else
  begin
    // The parse was not terminated by the character class close symbol (']'), so set an error code.
    FUREBuffer.Error := _URE_CCLASS_OPEN;
  end;
  Result := Run - CP;
end;

function TURESearch.ProbeLowSurrogate(LeftState: PUCS2; Limit: Cardinal; var Code: UCS4): Cardinal;
// probes for a low surrogate hex code
var
  I: Integer;
  Run,
  ListEnd: PUCS2;
begin
  I := 0;
  Code := 0;
  Run := LeftState;
  ListEnd := Run + Limit;

  while (I < 4) and (Run < ListEnd) do
  begin
    if (Run^ >= '0') and (Run^ <= '9') then
      Code := (Code shl 4) + Cardinal(Word(Run^) - Ord('0'))
    else
    begin
      if (Run^ >= 'A') and (Run^ <= 'F') then
        Code := (Code shl 4) + Cardinal(Word(Run^) - Ord('A')) + 10
      else
      begin
        if (Run^ >= 'a') and (Run^ <= 'f') then
          Code := (Code shl 4) + Cardinal(Word(Run^) - Ord('a')) + 10
        else
          Break;
      end;
    end;
    Inc(Run);
  end;

  if (SurrogateLowStart <= Code) and (Code <= SurrogateLowEnd) then
    Result :=  Run - LeftState
  else
    Result := 0;
end;

function TURESearch.CompileSymbol(S: PUCS2; Limit: Cardinal; Symbol: PUcSymbolTableEntry): Cardinal;
var
  C: UCS4;
  Run,
  ListEnd: PUCS2;
begin
  Run := S;
  ListEnd := S + Limit;

  C := UCS4(Run^);
  Inc(Run);
  if C = Ord('\') then
  begin
    if Run = ListEnd then
    begin
      // The EOS was encountered when expecting the reverse solidus to be followed
      // by the character it is escaping. Set an Error code and return the number
      // of characters consumed up to this point.
      FUREBuffer.Error := _URE_UNEXPECTED_EOS;
      Result := Run - S;
      Exit;
    end;

    C := UCS4(Run^);
    Inc(Run);
    case UCS2(C) of
      'p', 'P':
        begin
          if UCS2(C) = 'p' then
            Symbol.AType :=_URE_CCLASS
          else
            Symbol.AType :=_URE_NCCLASS;
          Inc(Run, ParsePropertyList(Run, ListEnd - Run, Symbol.Categories));
        end;
      'a':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $07;
        end;
      'b':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $08;
        end;
      'f':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0C;
        end;
      'n':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0A;
        end;
      'r':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0D;
        end;
      't':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $09;
        end;
      'v':
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := $0B;
        end;
    else
      case UCS2(C) of
        'x', 'X', 'u', 'U':
          begin
            // Collect between 1 and 4 digits representing an UCS2 code.
            if (Run < ListEnd) and
              ((Run^ >= '0') and (Run^ <= '9') or
               (Run^ >= 'A') and (Run^ <= 'F') or
               (Run^ >= 'a') and (Run^ <= 'f')) then
              Inc(Run, MakeHexNumber(Run, ListEnd - Run, C));
          end;
      end;

      // Simply add an escaped character here.
      Symbol.AType := _URE_CHAR;
      Symbol.Symbol.Chr := C;
    end;
  end
  else
  begin
    if (UCS2(C) = '^') or (UCS2(C) = '$') then
    begin
      // Handle the BOL and EOL anchors. This actually consists simply of setting
      // a flag that indicates that the user supplied anchor match function should
      // be called. This needs to be done instead of simply matching line/paragraph
      // separators because beginning-of-text and end-of-text tests are needed as well.
      if UCS2(C) = '^' then
        Symbol.AType := _URE_BOL_ANCHOR
      else
        Symbol.AType := _URE_EOL_ANCHOR;
    end
    else
    begin
      if UCS2(C) = '[' then
      begin
        // construct a character class
        Inc(Run, BuildCharacterClass(Run, ListEnd - Run, Symbol));
      end
      else
      begin
        if UCS2(C) = '.' then
          Symbol.AType := _URE_ANY_CHAR
        else
        begin
          Symbol.AType := _URE_CHAR;
          Symbol.Symbol.Chr := C;
        end;
      end;
    end;
  end;

  // If the symbol type happens to be a character and is a high surrogate, then
  // probe forward to see if it is followed by a low surrogate that needs to be added.
  if (Run < ListEnd) and
     (Symbol.AType = _URE_CHAR) and
     (SurrogateHighStart <= Symbol.Symbol.Chr) and
     (Symbol.Symbol.Chr <= SurrogateHighEnd) then
  begin
    if (SurrogateLowStart <= UCS4(Run^)) and
       (UCS4(Run^) <= SurrogateLowEnd) then
    begin
      Symbol.Symbol.Chr := $10000 + (((Symbol.Symbol.Chr and $03FF) shl 10) or (UCS4(Run^) and $03FF));
      Inc(Run);
    end
    else
    begin
      if (Run^ = '\') and (((Run + 1)^ = 'x') or ((Run + 1)^ = 'X') or
         ((Run + 1)^ = 'u') or ((Run + 1)^ = 'U')) then
      begin
        Inc(Run, ProbeLowSurrogate(Run + 2, ListEnd - (Run + 2), C));
        if (SurrogateLowStart <= C) and (C <= SurrogateLowEnd) then
        begin
          // Take into account the \[xu] in front of the hex code.
          Inc(Run, 2);
          Symbol.Symbol.Chr := $10000 + (((Symbol.Symbol.Chr and $03FF) shl 10) or (C and $03FF));
        end;
      end;
    end;
  end;

  // Last, make sure any _URE_CHAR type symbols are changed to lower if the
  // 'Casefold' flag is set.
  { TODO : use the entire mapping, not only the first character and use the
           case fold abilities of the unit. }
  if ((FUREBuffer.Flags and _URE_DFA_CASEFOLD) <> 0) and (Symbol.AType = _URE_CHAR) then
    Symbol.Symbol.Chr := UnicodeToLower(Symbol.Symbol.Chr)[0];

  // If the symbol constructed is anything other than one of the anchors,
  // make sure the _URE_DFA_BLANKLINE flag is removed.
  if (Symbol.AType <> _URE_BOL_ANCHOR) and (Symbol.AType <> _URE_EOL_ANCHOR) then
    FUREBuffer.Flags := FUREBuffer.Flags and not _URE_DFA_BLANKLINE;

  // Return the number of characters consumed.
  Result := Run - S;
end;

function TURESearch.SymbolsAreDifferent(A, B: PUcSymbolTableEntry): Boolean;
begin
  Result := False;
  if (A.AType <> B.AType) or (A.Mods <> B.Mods) or (A.Categories <> B.Categories) then
    Result := True
  else
  begin
    if (A.AType = _URE_CCLASS) or (A.AType = _URE_NCCLASS) then
    begin
      if A.Symbol.CCL.RangesUsed <> B.Symbol.CCL.RangesUsed then
        Result := True
      else
      begin
        if (A.Symbol.CCL.RangesUsed > 0) and
          not CompareMem(@A.Symbol.CCL.Ranges[0], @B.Symbol.CCL.Ranges[0],
            SizeOf(TUcRange) * A.Symbol.CCL.RangesUsed) then
          Result := True;;
      end;
    end
    else
    begin
      if (A.AType = _URE_CHAR) and (A.Symbol.Chr <> B.Symbol.Chr) then
        Result := True;
    end;
  end;
end;

function TURESearch.MakeSymbol(S: PUCS2; Limit: Cardinal; var Consumed: Cardinal): Cardinal;
// constructs a symbol, but only keep unique symbols
var
  I: Integer;
  Start: PUcSymbolTableEntry;
  Symbol: TUcSymbolTableEntry;
begin
  // Build the next symbol so we can test to see if it is already in the symbol table.
  FillChar(Symbol, SizeOf(TUcSymbolTableEntry), 0);
  Consumed := CompileSymbol(S, Limit, @Symbol);

  // Check to see if the symbol exists.
  I := 0;
  Start := @FUREBuffer.SymbolTable.Symbols[0];
  while (I < FUREBuffer.SymbolTable.SymbolsUsed) and SymbolsAreDifferent(@Symbol, Start) do
  begin
    Inc(I);
    Inc(Start);
  end;

  if I < FUREBuffer.SymbolTable.SymbolsUsed then
  begin
    // Free up any ranges used for the symbol.
    if (Symbol.AType = _URE_CCLASS) or (Symbol.AType = _URE_NCCLASS) then
      Symbol.Symbol.CCL.Ranges := nil;
    Result := FUREBuffer.SymbolTable.Symbols[I].ID;
    Exit;
  end;

  // Need to add the new symbol.
  if FUREBuffer.SymbolTable.SymbolsUsed = Length(FUREBuffer.SymbolTable.Symbols) then
  begin
    SetLength(FUREBuffer.SymbolTable.Symbols, Length(FUREBuffer.SymbolTable.Symbols) + 8);
  end;

  Symbol.ID := FUREBuffer.SymbolTable.SymbolsUsed;
  Inc(FUREBuffer.SymbolTable.SymbolsUsed);
  FUREBuffer.SymbolTable.Symbols[Symbol.ID] := Symbol;
  Result := Symbol.ID;
end;

function TURESearch.MakeExpression(AType, LHS, RHS: Cardinal): Cardinal;
var
  I: Integer;
begin
  // Determine if the expression already exists or not.
  with FUREBuffer.ExpressionList do
  begin
    for I := 0 to ExpressionsUsed - 1 do
    begin
      if (Expressions[I].AType = AType) and
         (Expressions[I].LHS = LHS) and
         (Expressions[I].RHS = RHS) then
      begin
        Result := I;
        Exit;
      end;
    end;

    // Need to add a new expression.
    if ExpressionsUsed = Length(Expressions) then
      SetLength(Expressions, Length(Expressions) + 8);

    Expressions[ExpressionsUsed].OnStack := False;
    Expressions[ExpressionsUsed].AType := AType;
    Expressions[ExpressionsUsed].LHS := LHS;
    Expressions[ExpressionsUsed].RHS := RHS;

    Result := ExpressionsUsed;
    Inc(ExpressionsUsed);
  end;
end;

function IsSpecial(C: Word): Boolean;
begin
  Result := C in [Word('+'), Word('*'), Word('?'), Word('{'), Word('|'), Word(')')];
end;

procedure TURESearch.CollectPendingOperations(var State: Cardinal);
// collects all pending AND and OR operations and make corresponding expressions
var
  Operation: Cardinal;
begin
  repeat
    Operation := Peek;
    if (Operation <> _URE_AND) and (Operation <> _URE_OR) then
      Break;
    // make an expression with the AND or OR operator and its right hand side
    Operation := Pop;
    State := MakeExpression(Operation, Pop, State);
  until False;
end;

function TURESearch.ConvertRegExpToNFA(RE: PWideChar; RELength: Cardinal): Cardinal;
// Converts the regular expression into an NFA in a form that will be easy to
// reduce to a DFA. The starting state for the reduction will be returned.
var
  C: UCS2;
  Head, Tail: PUCS2;
  S: WideString;
  Symbol,
  State,
  LastState,
  Used,
  M, N: Cardinal;
  I: Integer;

begin
  State := _URE_NOOP;

  Head := RE;
  Tail := Head + RELength;
  while (FUREBuffer.Error = _URE_OK) and (Head < Tail) do
  begin
    C := Head^;
    Inc(Head);
    case C of
      '(':
        Push(_URE_PAREN);
      ')': // check for the case of too many close parentheses
        begin
          if Peek = _URE_NOOP then
          begin
            FUREBuffer.Error := _URE_UNBALANCED_GROUP;
            Break;
          end;
          CollectPendingOperations(State);
          // remove the _URE_PAREN off the stack
          Pop;
        end;
      '*':
        State := MakeExpression(_URE_STAR, State, _URE_NOOP);
      '+':
        State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
      '?':
        State := MakeExpression(_URE_QUEST, State, _URE_NOOP);
      '|':
        begin
          CollectPendingOperations(State);
          Push(State);
          Push(_URE_OR);
        end;
      '{': // expressions of the form {m, n}
        begin
          C := #0;
          M := 0;
          N := 0;
          // get first number
          while UnicodeIsWhiteSpace(UCS4(Head^)) do
            Inc(Head);
          S := '';
          while Head^ in [WideChar('0')..WideChar('9')] do
          begin
            S := S + Head^;
            Inc(Head);
          end;
          if S <> '' then
            M := StrToInt(S);

          while UnicodeIsWhiteSpace(UCS4(Head^)) do
            Inc(Head);
          if (Head^ <> ',') and (Head^ <> '}') then
          begin
            FUREBuffer.Error := _URE_INVALID_RANGE;
            Break;
          end;

          // check for an upper limit
          if Head^ <> '}' then
          begin
            Inc(Head);
            // get second number
            while UnicodeIsWhiteSpace(UCS4(Head^)) do
              Inc(Head);
            S := '';
            while Head^ in [WideChar('0')..WideChar('9')] do
            begin
              S := S + Head^;
              Inc(Head);
            end;
            if S <> '' then
              N := StrToInt(S);
          end
          else
            N := M;

          if Head^ <> '}' then
          begin
            FUREBuffer.Error := _URE_RANGE_OPEN;
            Break;
          end
          else
            Inc(Head);

          // N = 0 means unlimited number of occurences
          if N = 0 then
          begin
            case M of
              0: // {,} {0,}  {0, 0} mean the same as the star operator
                State := MakeExpression(_URE_STAR, State, _URE_NOOP);
              1: // {1,} {1, 0} mean the same as the plus operator
                State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
            else
              begin
                // encapsulate the expanded branches as would they be in parenthesis
                // in order to avoid unwanted concatenation with pending operations/symbols
                Push(_URE_PAREN);
                // {m,} {m, 0} mean M fixed occurences plus star operator
                // make E^m...
                for I := 1 to M - 1 do
                begin
                  Push(State);
                  Push(_URE_AND);
                end;
                // ...and repeat the last symbol one or more times
                State := MakeExpression(_URE_PLUS, State, _URE_NOOP);
                CollectPendingOperations(State);
                Pop;
              end;
            end;
          end
          else
          begin
            // check proper range limits
            if M > N then
            begin
              FUREBuffer.Error := _URE_INVALID_RANGE;
              Break;
            end;

            // check special case {0, 1} (which corresponds to the ? operator)
            if (M = 0) and (N = 1) then
              State := MakeExpression(_URE_QUEST, State, _URE_NOOP)
            else
            begin
              // handle the general case by expanding {m, n} into the equivalent
              // expression E^m | E^(m + 1) | ... | E^n

              // encapsulate the expanded branches as would they be in parenthesis
              // in order to avoid unwanted concatenation with pending operations/symbols
              Push(_URE_PAREN);
              // keep initial state as this is the one all alternatives start from
              LastState := State;

              // Consider the special case M = 0 first. Because there's no construct
              // to enter a pure epsilon-transition into the expression array I
              // work around with the question mark operator to describe the first
              // and second branch alternative.
              if M = 0 then
              begin
                State := MakeExpression(_URE_QUEST, State, _URE_NOOP);
                Inc(M, 2);
                // Mark the pending OR operation (there must always follow at
                // least on more alternative because the special case {0, 1} has
                // already been handled).
                Push(State);
                Push(_URE_OR);
              end;

              while M <= N do
              begin
                State := LastState;
                // create E^M
                for I := 1 to Integer(M) - 1 do
                begin
                  Push(State);
                  Push(_URE_AND);
                end;
                // finish the branch and mark it as pending OR operation if it
                // isn't the last one
                CollectPendingOperations(State);
                if M < N then
                begin
                  Push(State);
                  Push(_URE_OR);
                end;
                Inc(M);
              end;
              // remove the _URE_PAREN off the stack
              Pop;
            end;
          end;
        end;
    else
      Dec(Head);
      Symbol := MakeSymbol(Head, Tail - Head, Used);
      Inc(Head, Used);
      State := MakeExpression(_URE_SYMBOL, Symbol, _URE_NOOP);
    end;

    if (C <> '(') and (C <> '|') and (C <> '{') and (Head < Tail) and
       (not IsSpecial(Word(Head^)) or (Head^ = '(')) then
    begin
      Push(State);
      Push(_URE_AND);
    end;
  end;

  CollectPendingOperations(State);
  if FUREBuffer.Stack.ListUsed > 0 then
    FUREBuffer.Error := _URE_UNBALANCED_GROUP;

  if FUREBuffer.Error = _URE_OK then
    Result := State
  else
    Result := _URE_NOOP;
end;

procedure TURESearch.AddSymbolState(Symbol, State: Cardinal);
var
  I, J: Integer;
  Found: Boolean;
begin
  // Locate the symbol in the symbol table so the state can be added.
  // If the symbol doesn't exist, then we are in serious trouble.
  with FUREBuffer.SymbolTable do
  begin
    I := 0;
    while (I < SymbolsUsed) and (Symbol <> Symbols[I].ID) do
      Inc(I);

    Assert(I < SymbolsUsed);
  end;

  // Now find out if the state exists in the symbol's state list.
  with FUREBuffer.SymbolTable.Symbols[I].States do
  begin
    Found := False;
    for J := 0 to ListUsed - 1 do
    begin
      if State <= List[J] then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      J := ListUsed;
    if not Found or (State < List[J]) then
    begin
      // Need to add the state in order.
      if ListUsed = Length(List) then
        SetLength(List, Length(List) + 8);
      if J < ListUsed then
        Move(List[J], List[J + 1], SizeOf(Cardinal) * (ListUsed - J));
      List[J] := State;
      Inc(ListUsed);
    end;
  end;
end;

function TURESearch.AddState(NewStates: array of Cardinal): Cardinal;
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    if (FUREBuffer.States.States[I].StateList.ListUsed = Length(NewStates)) and
       CompareMem(@NewStates[0], @FUREBuffer.States.States[I].StateList.List[0],
         SizeOf(Cardinal) * Length(NewStates)) then
    begin
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    // Need to add a new DFA State (set of NFA states).
    if FUREBuffer.States.StatesUsed = Length(FUREBuffer.States.States) then
      SetLength(FUREBuffer.States.States, Length(FUREBuffer.States.States) + 8);

    with FUREBuffer.States.States[FUREBuffer.States.StatesUsed] do
    begin
      ID := FUREBuffer.States.StatesUsed;
      if (StateList.ListUsed + Length(NewStates)) >= Length(StateList.List) then
        SetLength(StateList.List, Length(StateList.List) + Length(NewStates) + 8);
      Move(NewStates[0], StateList.List[StateList.ListUsed], SizeOf(Cardinal) * Length(NewStates));
      Inc(StateList.ListUsed, Length(NewStates));
    end;
    Inc(FUREBuffer.States.StatesUsed);
  end;

  // Return the ID of the DFA state representing a group of NFA States.
  if Found then
    Result := I
  else
    Result := FUREBuffer.States.StatesUsed - 1;
end;

procedure TURESearch.Reduce(Start: Cardinal);
var
  I, J,
  Symbols: Integer;
  State,
  RHS,
  s1, s2,
  ns1, ns2: Cardinal;
  Evaluating: Boolean;
begin
  FUREBuffer.Reducing := True;

  // Add the starting state for the reduction.
  AddState([Start]);

  // Process each set of NFA states that get created.
  I := 0;
  // further states are added in the loop
  while I < FUREBuffer.States.StatesUsed do
  begin
    with FUREBuffer, States.States[I], ExpressionList do
    begin
      // Push the current states on the stack.
      for J := 0 to StateList.ListUsed - 1 do
        Push(StateList.List[J]);

      // Reduce the NFA states.
      Accepting := False;
      Symbols := 0;
      J := 0;
      // need a while loop here as the stack will be modified within the loop and
      // so also its usage count used to terminate the loop
      while J < FUREBuffer.Stack.ListUsed do
      begin
        State := FUREBuffer.Stack.List[J];
        Evaluating := True;

        // This inner loop is the iterative equivalent of recursively
        // reducing subexpressions generated as a result of a reduction.
        while Evaluating do
        begin
          case Expressions[State].AType of
            _URE_SYMBOL:
              begin
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                AddSymbolState(Expressions[State].LHS, ns1);
                Inc(Symbols);
                Evaluating := False;
              end;
            _URE_ONE:
              begin
                Accepting := True;
                Evaluating := False;
              end;
            _URE_QUEST:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                State := MakeExpression(_URE_OR, ns1, s1);
              end;
            _URE_PLUS:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_STAR, s1, _URE_NOOP);
                State := MakeExpression(_URE_AND, s1, ns1);
              end;
            _URE_STAR:
              begin
                s1 := Expressions[State].LHS;
                ns1 := MakeExpression(_URE_ONE, _URE_NOOP, _URE_NOOP);
                ns2 := MakeExpression(_URE_PLUS, s1, _URE_NOOP);
                State := MakeExpression(_URE_OR, ns1, ns2);
              end;
            _URE_OR:
              begin
                s1 := Expressions[State].LHS;
                s2 := Expressions[State].RHS;
                Push(s1);
                Push(s2);
                Evaluating := False;
              end;
            _URE_AND:
              begin
                s1 := Expressions[State].LHS;
                s2 := Expressions[State].RHS;
                case Expressions[s1].AType of
                  _URE_SYMBOL:
                    begin
                      AddSymbolState(Expressions[s1].LHS, s2);
                      Inc(Symbols);
                      Evaluating := False;
                    end;
                  _URE_ONE:
                    State := s2;
                  _URE_QUEST:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_AND, ns1, s2);
                      State := MakeExpression(_URE_OR, s2, ns2);
                    end;
                  _URE_PLUS:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_OR, s2, State);
                      State := MakeExpression(_URE_AND, ns1, ns2);
                    end;
                  _URE_STAR:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := MakeExpression(_URE_AND, ns1, State);
                      State := MakeExpression(_URE_OR, s2, ns2);
                    end;
                  _URE_OR:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := Expressions[s1].RHS;
                      ns1 := MakeExpression(_URE_AND, ns1, s2);
                      ns2 := MakeExpression(_URE_AND, ns2, s2);
                      State := MakeExpression(_URE_OR, ns1, ns2);
                    end;
                  _URE_AND:
                    begin
                      ns1 := Expressions[s1].LHS;
                      ns2 := Expressions[s1].RHS;
                      ns2 := MakeExpression(_URE_AND, ns2, s2);
                      State := MakeExpression(_URE_AND, ns1, ns2);
                    end;
                end;
              end;
          end;
        end;
        Inc(J);
      end;

      // clear the state stack
      while Pop <> _URE_NOOP do
        { nothing };

      // generate the DFA states for the symbols collected during the current reduction
      if (TransitionsUsed + Symbols) > Length(Transitions) then
        SetLength(Transitions, Length(Transitions) + Symbols);

      // go through the symbol table and generate the DFA state transitions for
      // each symbol that has collected NFA states
      Symbols := 0;
      J := 0;
      while J < FUREBuffer.SymbolTable.SymbolsUsed do
      begin
        begin
          if FUREBuffer.SymbolTable.Symbols[J].States.ListUsed > 0 then
          begin
            Transitions[Symbols].LHS := FUREBuffer.SymbolTable.Symbols[J].ID;
            with FUREBuffer.SymbolTable.Symbols[J] do
            begin
              RHS := AddState(Copy(States.List, 0, States.ListUsed));
              States.ListUsed := 0;
            end;
            Transitions[Symbols].RHS := RHS;
            Inc(Symbols);
          end;
        end;
        Inc(J);
      end;

      // set the number of transitions actually used
      // Note: we need again to qualify a part of the TransistionsUsed path since the
      //       state array could be reallocated in the AddState call above and the
      //       with ... do will then be invalid.
      States.States[I].TransitionsUsed := Symbols;
    end;
    Inc(I);
  end;
  FUREBuffer.Reducing := False;
end;

procedure TURESearch.AddEquivalentPair(L, R: Cardinal);
var
  I: Integer;
begin
  L := FUREBuffer.States.States[L].ID;
  R := FUREBuffer.States.States[R].ID;

  if L <> R then
  begin
    if L > R then
    begin
      I := L;
      L := R;
      R := I;
    end;

    // Check to see if the equivalence pair already exists.
    I := 0;
    with FUREBuffer.EquivalentList do
    begin
      while (I < EquivalentsUsed) and
            ((Equivalents[I].Left <> L) or (Equivalents[I].Right <> R)) do
        Inc(I);

      if I >= EquivalentsUsed then
      begin
        if EquivalentsUsed = Length(Equivalents) then
          SetLength(Equivalents, Length(Equivalents) + 8);

        Equivalents[EquivalentsUsed].Left := L;
        Equivalents[EquivalentsUsed].Right := R;
        Inc(EquivalentsUsed);
      end;
    end;
  end;
end;

procedure TURESearch.MergeEquivalents;
// merges the DFA states that are equivalent
var
  I, J, K,
  Equal: Integer;
  Done: Boolean;
  State1, State2,
  LeftState,
  RightState: PUcState;
begin
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    State1 := @FUREBuffer.States.States[I];
    if State1.ID = Cardinal(I) then
    begin
      J := 0;
      while J < I do
      begin
        State2 := @FUREBuffer.States.States[J];
        if State2.ID = Cardinal(J) then
        begin
          FUREBuffer.EquivalentList.EquivalentsUsed := 0;
          AddEquivalentPair(I, J);

          Done := False;
          Equal := 0;
          while Equal < FUREBuffer.EquivalentList.EquivalentsUsed do
          begin
            LeftState := @FUREBuffer.States.States[FUREBuffer.EquivalentList.Equivalents[Equal].Left];
            RightState := @FUREBuffer.States.States[FUREBuffer.EquivalentList.Equivalents[Equal].Right];

            if (LeftState.Accepting <> RightState.Accepting) or
               (LeftState.TransitionsUsed <> RightState.TransitionsUsed) then
            begin
              Done := True;
              Break;
            end;

            K := 0;
            while (K < LeftState.TransitionsUsed) and
                  (LeftState.Transitions[K].LHS = RightState.Transitions[K].LHS) do
              Inc(K);

            if K < LeftState.TransitionsUsed then
            begin
              Done := True;
              Break;
            end;

            for K := 0 to LeftState.TransitionsUsed - 1 do
              AddEquivalentPair(LeftState.Transitions[K].RHS, RightState.Transitions[K].RHS);

            Inc(Equal);
          end;

          if not Done then
            Break;
        end;
        Inc(J);
      end;

      if J < I then
      begin
        with FUREBuffer do
        begin
          for Equal := 0 to EquivalentList.EquivalentsUsed - 1 do
          begin
            States.States[EquivalentList.Equivalents[Equal].Right].ID :=
              States.States[EquivalentList.Equivalents[Equal].Left].ID;
          end;
        end;
      end;

    end;
  end;

  // Renumber the states appropriately
  State1 := @FUREBuffer.States.States[0];
  Equal := 0;
  for I := 0 to FUREBuffer.States.StatesUsed - 1 do
  begin
    if State1.ID = Cardinal(I) then
    begin
      State1.ID := Equal;
      Inc(Equal);
    end
    else
      State1.ID := FUREBuffer.States.States[State1.ID].ID;
    Inc(State1);
  end;
end;

procedure TURESearch.ClearUREBuffer;
var
  I: Integer;
begin
  with FUREBuffer do
  begin
    // quite a few dynamic arrays to free
    Stack.List := nil;
    ExpressionList.Expressions := nil;

    // the symbol table has been handed over to the DFA and will be freed on
    // release of the DFA
    SymbolTable.SymbolsUsed := 0;

    for I := 0 to States.StatesUsed - 1 do
    begin
      States.States[I].Transitions := nil;
      States.States[I].StateList.List := nil;
      States.States[I].StateList.ListUsed := 0;
      States.States[I].TransitionsUsed := 0;
    end;

    States.StatesUsed := 0;
    States.States := nil;
    EquivalentList.Equivalents := nil;
  end;
  FillChar(FUREBuffer, SizeOf(FUREBuffer), 0);
end;

procedure TURESearch.CompileURE(RE: PWideChar; RELength: Cardinal; Casefold: Boolean);
var
  I, J: Integer;
  State: Cardinal;
  Run: PUcState;
  TP: Integer;

  procedure UREError(Text: string; RE: PWideChar);
  var
    S: string;
  begin
    S := RE;
    raise EJclUnicodeError.CreateResFmt(@RsUREErrorFmt, [LoadResString(@RsUREBaseString), Text, S]);
  end;

begin
  // be paranoid
  if (RE <> nil) and (RE^ <> WideNull) and (RELength > 0) then
  begin
    // Reset the various fields of the compilation buffer. Default the Flags
    // to indicate the presense of the "^$" pattern.  If any other pattern
    // occurs, then this flag will be removed.  This is done to catch this
    // special pattern and handle it specially when matching.
    ClearUREBuffer;
    ClearDFA;
    FUREBuffer.Flags := _URE_DFA_BLANKLINE;
    if Casefold then
      FUREBuffer.Flags := FUREBuffer.Flags or _URE_DFA_CASEFOLD;

    // Construct the NFA. If this stage returns a 0, then an error occured or an
    // empty expression was passed.
    State := ConvertRegExpToNFA(RE, RELength);
    if State <> _URE_NOOP then
    begin
      // Do the expression reduction to get the initial DFA.
      Reduce(State);

      // Merge all the equivalent DFA States.
      MergeEquivalents;

      // Construct the minimal DFA.
      FDFA.Flags := FUREBuffer.Flags and (_URE_DFA_CASEFOLD or _URE_DFA_BLANKLINE);

      // Free up the NFA state groups and transfer the symbols from the buffer
      // to the DFA.
      FDFA.SymbolTable := FUREBuffer.SymbolTable;
      FUREBuffer.SymbolTable.Symbols := nil;

      // Collect the total number of states and transitions needed for the DFA.
      State := 0;
      for I := 0 to FUREBuffer.States.StatesUsed - 1 do
      begin
        if FUREBuffer.States.States[I].ID = State then
        begin
          Inc(FDFA.StateList.StatesUsed);
          Inc(FDFA.TransitionList.TransitionsUsed, FUREBuffer.States.States[I].TransitionsUsed);
          Inc(State);
        end;
      end;

      // Allocate enough space for the states and transitions.
      SetLength(FDFA.StateList.States, FDFA.StateList.StatesUsed);
      SetLength(FDFA.TransitionList.Transitions, FDFA.TransitionList.TransitionsUsed);

      // Actually transfer the DFA States from the buffer.
      State := 0;
      TP := 0;
      Run := @FUREBuffer.States.States[0];
      for I := 0 to FUREBuffer.States.StatesUsed - 1 do
      begin
        if Run.ID = State then
        begin
          FDFA.StateList.States[I].StartTransition := TP;
          FDFA.StateList.States[I].NumberTransitions := Run.TransitionsUsed;
          FDFA.StateList.States[I].Accepting := Run.Accepting;

          // Add the transitions for the state
          for J := 0 to FDFA.StateList.States[I].NumberTransitions - 1 do
          begin
            FDFA.TransitionList.Transitions[TP].Symbol := Run.Transitions[J].LHS;
            FDFA.TransitionList.Transitions[TP].NextState :=
              FUREBuffer.States.States[Run.Transitions[J].RHS].ID;
            Inc(TP);
          end;

          Inc(State);
        end;
        Inc(Run);
      end;
    end
    else
    begin
      // there might be an error while parsing the pattern, show it if so
      case FUREBuffer.Error of
        _URE_UNEXPECTED_EOS:
          UREError(LoadResString(@RsUREUnexpectedEOS), RE);
        _URE_CCLASS_OPEN:
          UREError(LoadResString(@RsURECharacterClassOpen), RE);
        _URE_UNBALANCED_GROUP:
          UREError(LoadResString(@RsUREUnbalancedGroup), RE);
        _URE_INVALID_PROPERTY:
          UREError(LoadResString(@RsUREInvalidCharProperty), RE);
        _URE_INVALID_RANGE:
          UREError(LoadResString(@RsUREInvalidRepeatRange), RE);
        _URE_RANGE_OPEN:
          UREError(LoadResString(@RsURERepeatRangeOpen), RE);
      else
        // expression was empty
        raise EJclUnicodeError.CreateRes(@RsUREExpressionEmpty);
      end;
    end;
  end;
end;

procedure TURESearch.ClearDFA;
var
  I: Integer;
begin
  with FDFA do
  begin
    for I := 0 to SymbolTable.SymbolsUsed - 1 do
    begin
      if (SymbolTable.Symbols[I].AType = _URE_CCLASS) or
         (SymbolTable.Symbols[I].AType = _URE_NCCLASS) then
        SymbolTable.Symbols[I].Symbol.CCL.Ranges := nil;
    end;

    for I := 0 to SymbolTable.SymbolsUsed - 1 do
    begin
      FDFA.SymbolTable.Symbols[I].States.List := nil;
      FDFA.SymbolTable.Symbols[I].States.ListUsed := 0;
    end;
    SymbolTable.SymbolsUsed := 0;

    SymbolTable.Symbols := nil;
    StateList.States := nil;
    TransitionList.Transitions := nil;
  end;
  FillChar(FDFA, SizeOf(FDFA), 0);
end;

function IsSeparator(C: UCS4): Boolean;
begin
  Result := (C = $D) or (C = $A) or (C = $2028) or (C = $2029);
end;

function TURESearch.ExecuteURE(Flags: Cardinal; Text: PUCS2; TextLen: Cardinal; var MatchStart,
  MatchEnd: Cardinal): Boolean;
var
  I, J: Integer;
  Matched,
  Found: Boolean;
  Start, Stop: Integer;
  C: UCS4;
  Run, Tail, lp: PUCS2;
  LastState: PDFAState;
  Symbol: PUcSymbolTableEntry;
  Rp: PUcRange;
begin
  Result := False;
  if Text <> nil then
  begin
    // Handle the special case of an empty string matching the "^$" pattern.
    if (Textlen = 0) and ((FDFA.Flags and _URE_DFA_BLANKLINE) <> 0) then
    begin
      MatchStart := 0;
      MatchEnd := 0;
      Result := True;
      Exit;
    end;

    Run := Text;
    Tail := Run + TextLen;
    Start := -1;
    Stop := -1;
    LastState := @FDFA.StateList.States[0];

    Found := False;
    while not Found and (Run < Tail) do
    begin
      lp := Run;
      C := UCS4(Run^);
      Inc(Run);

      // Check to see if this is a high surrogate that should be combined with a
      // following low surrogate.
      if (Run < Tail) and
         (SurrogateHighStart <= C) and (C <= SurrogateHighEnd) and
         (SurrogateLowStart <= UCS4(Run^)) and (UCS4(Run^) <= SurrogateLowEnd) then
      begin
        C := $10000 + (((C and $03FF) shl 10) or (UCS4(Run^) and $03FF));
        Inc(Run);
      end;

      // Determine if the character is non-spacing and should be skipped.
      if ((Flags and URE_IGNORE_NONSPACING) <> 0) and UnicodeIsNonSpacingMark(C) then
      begin
        Inc(Run);
        Continue;
      end;

      if (FDFA.Flags and _URE_DFA_CASEFOLD) <> 0 then
        { TODO : use the entire mapping, not only the first character }
        C := UnicodeToLower(C)[0];

      // See if one of the transitions matches.
      I := LastState.NumberTransitions - 1;
      Matched := False;

      while not Matched and (I >= 0) do
      begin
        Symbol := @FDFA.SymbolTable.Symbols[FDFA.TransitionList.Transitions[LastState.StartTransition + I].Symbol];
        case Symbol.AType of
          _URE_ANY_CHAR:
            if ((Flags and URE_DONT_MATCHES_SEPARATORS) <> 0) or
               not IsSeparator(C) then
              Matched := True;
          _URE_CHAR:
            if C = Symbol.Symbol.Chr then
              Matched := True;
          _URE_BOL_ANCHOR:
            if Lp = Text then
            begin
              Run := lp;
              Matched := True;
            end
            else
            begin
              if IsSeparator(C) then
              begin
                if (C = $D) and (Run < Tail) and (Run^ = #$A) then
                  Inc(Run);
                Lp := Run;
                Matched := True;
              end;
            end;
          _URE_EOL_ANCHOR:
            if IsSeparator(C) then
            begin
              // Put the pointer back before the separator so the match end
              // position will be correct. This  will also cause the `Run'
              // pointer to be advanced over the current separator once the
              // match end point has been recorded.
              Run := Lp;
              Matched := True;
            end;
          _URE_CCLASS,
          _URE_NCCLASS:
            with Symbol^ do
            begin
              if Categories <> [] then
                Matched := CategoryLookup(C, Categories);
              if Symbol.CCL.RangesUsed > 0 then
              begin
                Rp := @Symbol.CCL.Ranges[0];
                for J := 0 to Symbol.CCL.RangesUsed - 1 do
                begin
                  if (Rp.MinCode <= C) and (C <= Rp.MaxCode) then
                  begin
                    Matched := True;
                    Break;
                  end;
                  Inc(Rp);
                end;
              end;

              if AType = _URE_NCCLASS then
                Matched := not Matched;
            end;
        end;

        if Matched then
        begin
          if Start = -1 then
            Start := Lp - Text
          else
            Stop := Run - Text;

          LastState := @FDFA.StateList.States[FDFA.TransitionList.Transitions[LastState.StartTransition + I].NextState];

          // If the match was an EOL anchor, adjust the pointer past the separator
          // that caused the match. The correct match position has been recorded
          // already.
          if Symbol.AType = _URE_EOL_ANCHOR then
          begin
            // skip the character that caused the match
            Inc(Run);
            // handle the infamous CRLF situation
            if (Run < Tail) and (C = $D) and (Run^ = #$A) then
              Inc(Run);
          end;
        end;
        Dec(I);
      end;

      if not Matched then
      begin
        Found := LastState.Accepting;
        if not Found then
        begin
          // If the last state was not accepting, then reset and start over.
          LastState := @FDFA.StateList.States[0];
          Start := -1;
          Stop := -1;
        end
        else
        begin
          // set start and stop pointer if not yet done
          if Start = -1 then
          begin
            Start := Lp - Text;
            Stop := Run - Text;
          end
          else
          begin
            if Stop = -1 then
              Stop := Lp - Text;
          end;
        end;
      end
      else
      begin
        if Run = Tail then
        begin
          if not LastState.Accepting then
          begin
            // This ugly hack is to make sure the end-of-line anchors match
            // when the source text hits the end. This is only done if the last
            // subexpression matches.
            for I := 0 to LastState.NumberTransitions - 1 do
            begin
              if Found then
                Break;
              Symbol := @FDFA.SymbolTable.Symbols[FDFA.TransitionList.Transitions[LastState.StartTransition + I].Symbol];
              if Symbol.AType =_URE_EOL_ANCHOR then
              begin
                LastState := @FDFA.StateList.States[FDFA.TransitionList.Transitions[LastState.StartTransition + I].NextState];
                if LastState.Accepting then
                begin
                  Stop := Run - Text;
                  Found := True;
                end
                else
                  Break;
              end;
            end;
          end
          else
          begin
            // Make sure any conditions that match all the way to the end of
            // the string match.
            Found := True;
            Stop := Run - Text;
          end;
        end;
      end;
    end;

    if Found then
    begin
      MatchStart := Start;
      MatchEnd := Stop;
    end;
    Result := Found;
  end;
end;

function TURESearch.FindAll(const Text: WideString): Boolean;
begin
  Result := FindAll(PWideChar(Text), Length(Text));
end;

function TURESearch.FindAll(Text: PWideChar; TextLen: Cardinal): Boolean;
// Looks for all occurences of the pattern passed to FindPrepare and creates an
// internal list of their positions.
var
  Start, Stop: Cardinal;
  Run: PWideChar;
  RunLen: Cardinal;
begin
  ClearResults;
  Run := Text;
  RunLen := TextLen;
  // repeat to find all occurences of the pattern
  while ExecuteURE(0, Run, RunLen, Start, Stop) do
  begin
    // store this result (consider text pointer movement)...
    AddResult(Start + Run - Text, Stop + Run - Text);
    // ... and advance text position and length
    Inc(Run, Stop);
    Dec(RunLen, Stop);
  end;
  Result := FResults.Count > 0;
end;

function TURESearch.FindFirst(const Text: WideString; var Start, Stop: Cardinal): Boolean;
begin
  Result := FindFirst(PWideChar(Text), Length(Text), Start, Stop);
end;

function TURESearch.FindFirst(Text: PWideChar; TextLen: Cardinal; var Start, Stop: Cardinal): Boolean;
// Looks for the first occurence of the pattern passed to FindPrepare in Text and
// returns True if one could be found (in which case Start and Stop are set to
// the according indices) otherwise False. This function is in particular of
// interest if only one occurence needs to be found.
begin
  ClearResults;
  Result := ExecuteURE(0, PWideChar(Text), Length(Text), Start, Stop);
  if Result then
    AddResult(Start, Stop);
end;

procedure TURESearch.FindPrepare(Pattern: PWideChar; PatternLength: Cardinal; Options: TSearchFlags);
begin
  CompileURE(Pattern, PatternLength, not (sfCaseSensitive in Options));
end;

procedure TURESearch.FindPrepare(const Pattern: WideString; Options: TSearchFlags);
begin
  CompileURE(PWideChar(Pattern), Length(Pattern), not (sfCaseSensitive in Options));
end;

//=== { TWideStrings } =======================================================

constructor TWideStrings.Create;
begin
  inherited Create;
  FLanguage := GetUserDefaultLCID;
  FNormalizationForm := nfC;
  FSaveFormat := sfUnicodeLSB;
end;

procedure TWideStrings.SetLanguage(Value: LCID);
begin
  FLanguage := Value;
end;

function TWideStrings.GetSaveUnicode: Boolean;
begin
  Result := SaveFormat = sfUnicodeLSB;
end;

procedure TWideStrings.SetSaveUnicode(const Value: Boolean);
begin
  if Value then
    SaveFormat := sfUnicodeLSB
  else
    SaveFormat := sfAnsi;
end;

function TWideStrings.Add(const S: WideString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TWideStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TWideStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
  S: WideString;
  CP: Integer;
begin
  BeginUpdate;
  try
    CP := CodePageFromLocale(FLanguage);
    for I := 0 to Strings.Count - 1 do
    begin
      S := StringToWideStringEx(Strings[I], CP);
      AddObject(S, Strings.Objects[I]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.AddStrings(Strings: TWideStrings);
var
  I: Integer;
  SourceCP,
  TargetCP: Integer;
  S: WideString;
begin
  Assert(Strings <> nil);

  BeginUpdate;
  try
    if Strings.FLanguage <> FLanguage then
    begin
      SourceCP := CodePageFromLocale(Strings.FLanguage);
      TargetCP := CodePageFromLocale(FLanguage);
      for I := 0 to Strings.Count - 1 do
      begin
        S := TranslateString(Strings[I], SourceCP, TargetCP);
        AddObject(S, Strings.Objects[I]);
      end;
    end
    else
    begin
      for I := 0 to Strings.Count - 1 do
        AddObject(Strings[I], Strings.Objects[I]);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Assign(Source: TPersistent);
// usual assignment routine, but able to assign wide and small strings
begin
  if Source is TWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TWideStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
  begin
    if Source is TStrings then
    begin
      BeginUpdate;
      try
        Clear;
        AddStrings(TStrings(Source));
      finally
        EndUpdate;
      end;
    end
    else
      inherited Assign(Source);
  end;
end;

procedure TWideStrings.AssignTo(Dest: TPersistent);
// need to do also assignment to old style TStrings, but this class doesn't know
// TWideStrings, so we need to do it from here
var
  I: Integer;
  S: string;
  CP: Integer;
begin
  if Dest is TStrings then
  begin
    with Dest as TStrings do
    begin
      BeginUpdate;
      try
        CP := CodePageFromLocale(FLanguage);
        Clear;
        for I := 0 to Self.Count - 1 do
        begin
          S := WideStringToStringEx(Self[I], CP);
          AddObject(S, Self.Objects[I]);
        end;
      finally
        EndUpdate;
      end;
    end;
  end
  else
  begin
    if Dest is TWideStrings then
    begin
      with Dest as TWideStrings do
      begin
        BeginUpdate;
        try
          Clear;
          AddStrings(Self);
        finally
          EndUpdate;
        end;
      end;
    end
    else
      inherited AssignTo(Dest);
  end;
end;

procedure TWideStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TWideStrings.DefineProperties(Filer: TFiler);

// Defines a private property for the content of the list.
// There's a bug in the handling of text DFMs in Classes.pas which prevents
// WideStrings from loading under some circumstances. Zbysek Hlinka
// (zhlinka att login dott cz) brought this to my attention and supplied also a solution.
// See ReadData and WriteData methods for implementation details.

  //--------------- local function --------------------------------------------

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TWideStrings then
        Result := not Equals(TWideStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;

  //--------------- end local function ----------------------------------------

begin
  Filer.DefineProperty('WideStrings', ReadData, WriteData, DoWrite);
end;

procedure TWideStrings.DoConfirmConversion(var Allowed: Boolean);
begin
  if Assigned(FOnConfirmConversion) then
    FOnConfirmConversion(Self, Allowed);
end;

procedure TWideStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

function TWideStrings.Equals(Strings: TWideStrings): Boolean;
var
  I, Count: Integer;
begin
  Assert(Strings <> nil);

  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then
    Exit;
  { TODO : use internal comparation routine as soon as composition is implemented }
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then
      Exit;
  Result := True;
end;

procedure TWideStrings.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX, [EBP + 4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TWideStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TWideStrings.GetCapacity: Integer;
// Descendants may optionally override/replace this default implementation.
begin
  Result := Count;
end;

function TWideStrings.GetCommaText: WideString;
var
  S: WideString;
  P: PWideChar;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := '""'
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not (P^ in [WideNull..WideSpace, WideChar('"'), WideChar(',')]) do
        Inc(P);
      if P^ <> WideNull then
        S := WideQuotedStr(S, '"');
      Result := Result + S + ',';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TWideStrings.GetName(Index: Integer): WideString;
var
  P: Integer;
begin
  Result := Get(Index);
  P := Pos('=', Result);
  if P > 0 then
    SetLength(Result, P - 1)
  else
    Result := '';
end;

function TWideStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TWideStrings.GetSeparatedText(Separators: WideString): WideString;
// Same as GetText but with customizable separator characters.
var
  I, L,
  Size,
  Count,
  SepSize: Integer;
  P: PWideChar;
  S: WideString;
begin
  Count := GetCount;
  SepSize := Length(Separators);
  Size := 0;
  for I := 0 to Count - 1 do
    Inc(Size, Length(Get(I)) + SepSize);

  // set one separator less, the last line does not need a trailing separator
  SetLength(Result, Size - SepSize);
  if Size > 0 then
  begin
    P := Pointer(Result);
    I := 0;
    while True do
    begin
      S := Get(I);
      L := Length(S);
      if L <> 0 then
      begin
        // add current string
        System.Move(Pointer(S)^, P^, 2 * L);
        Inc(P, L);
      end;
      Inc(I);
      if I = Count then
        Break;

      // add separators
      System.Move(Pointer(Separators)^, P^, SizeOf(WideChar) * SepSize);
      Inc(P, SepSize);
    end;
  end;
end;

function TWideStrings.GetTextStr: WideString;
begin
  Result := GetSeparatedText(WideCRLF);
end;

function TWideStrings.GetText: PWideChar;
begin
  Result := StrNewW(GetTextStr);
end;

function TWideStrings.GetValue(const Name: WideString): WideString;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt)
  else
    Result := '';
end;

function TWideStrings.IndexOf(const S: WideString): Integer;
var
  NormString: WideString;
begin
  NormString := WideNormalize(S, FNormalizationForm);

  for Result := 0 to GetCount - 1 do
    if WideCompareText(Get(Result), NormString, FLanguage) = 0 then
      Exit;
  Result := -1;
end;

function TWideStrings.IndexOfName(const Name: WideString): Integer;
var
  P: Integer;
  S: WideString;
  NormName: WideString;
begin
  NormName := WideNormalize(Name, FNormalizationForm);

  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos('=', S);
    if (P > 0) and (WideCompareText(Copy(S, 1, P - 1), NormName, FLanguage) = 0) then
      Exit;
  end;
  Result := -1;
end;

function TWideStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then
      Exit;
  Result := -1;
end;

procedure TWideStrings.InsertObject(Index: Integer; const S: WideString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TWideStrings.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    RaiseLastOSError;
  end;
end;

procedure TWideStrings.LoadFromStream(Stream: TStream);
// usual loader routine, but enhanced to handle byte order marks in stream
var
  Size,
  BytesRead: Integer;
  ByteOrderMask: array [0..5] of Byte; // BOM size is max 5 bytes (cf: wikipedia)
                                       // but it is easier to implement with a multiple of 2
  Loaded: Boolean;
  SW: WideString;
  SA: string;
begin
  BeginUpdate;
  try
    Loaded := False;

    Size := Stream.Size - Stream.Position;
    BytesRead := Stream.Read(ByteOrderMask[0],SizeOf(ByteOrderMask));

    // UTF16 LSB = Unicode LSB
    if (BytesRead >= 2) and (ByteOrderMask[0] = BOM_UTF16_LSB[0])
      and (ByteOrderMask[1] = BOM_UTF16_LSB[1]) then
    begin
      FSaveFormat := sfUTF16LSB;
      SetLength(SW, (Size - 2) div SizeOf(WideChar));
      Assert((Size and 1) <> 1,'Number of chars must be a multiple of 2');
      System.Move(ByteOrderMask[2],SW[1],BytesRead-2); // max 4 bytes = 2 widechars
      Stream.Read(SW[3], Size-BytesRead); // first 2 chars were copied by System.Move
      SetText(SW);
      Loaded := True;
    end;

    // UTF16 MSB = Unicode MSB
    if (BytesRead >= 2) and (ByteOrderMask[0] = BOM_UTF16_MSB[0])
      and (ByteOrderMask[1] = BOM_UTF16_MSB[1]) then
    begin
      FSaveFormat := sfUTF16MSB;
      SetLength(SW, (Size - 2) div SizeOf(WideChar));
      Assert((Size and 1) <> 1,'Number of chars must be a multiple of 2');
      System.Move(ByteOrderMask[2],SW[1],BytesRead-2); // max 4 bytes = 2 widechars
      Stream.Read(SW[3], Size-BytesRead); // first 2 chars were copied by System.Move
      StrSwapByteOrder(PWideChar(SW));
      SetText(SW);
      Loaded := True;
    end;

    // UTF8
    if (BytesRead >= 3) and (ByteOrderMask[0] = BOM_UTF8[0])
      and (ByteOrderMask[1] = BOM_UTF8[1]) and (ByteOrderMask[2] = BOM_UTF8[2]) then
    begin
      FSaveFormat := sfUTF8;
      SetLength(SA, (Size-3) div SizeOf(Char));
      System.Move(ByteOrderMask[3],SA[1],BytesRead-3); // max 3 bytes = 3 chars
      Stream.Read(SA[4], Size-BytesRead); // first 3 chars were copied by System.Move
      SW := UTF8ToWideString(SA);
      SetText(SW);
      Loaded := True;
    end;

    // default case (Ansi)
    if not Loaded then
    begin
      FSaveFormat := sfAnsi;
      SetLength(SA, Size div SizeOf(Char));
      System.Move(ByteOrderMask[0],SA[1],BytesRead); // max 6 bytes = 6 chars
      Stream.Read(SA[7], Size-BytesRead); // first 6 chars were copied by System.Move
      SetText(SA);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TWideStrings.ReadData(Reader: TReader);
begin
  case Reader.NextValue of
    vaLString, vaString:
      SetText(Reader.ReadString);
  else
    SetText(Reader.ReadWideString);
  end;
end;

procedure TWideStrings.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWideStrings.SaveToStream(Stream: TStream; WithBOM: Boolean = True);
// Saves the currently loaded text into the given stream. WithBOM determines whether to write a
// byte order mark or not. Note: when saved as ANSI text there will never be a BOM.
var
  SW: WideString;
  SA: string;
  Allowed: Boolean;
  Run: PWideChar;
begin
  // The application can decide in which format to save the content.
  // If FSaveUnicode is False then all strings are saved in standard ANSI format
  // which is also loadable by TStrings but you should be aware that all Unicode
  // strings are then converted to ANSI based on the current system locale.
  // An extra event is supplied to ask the user about the potential loss of
  // information when converting Unicode to ANSI strings.
  SW := GetTextStr;
  Allowed := True;
  FSaved := False; // be pessimistic
  // A check for potential information loss makes only sense if the application has
  // set an event to be used as call back to ask about the conversion.
  if (FSaveFormat = sfAnsi) and Assigned(FOnConfirmConversion) then
  begin
    // application requests to save only ANSI characters, so check the text and
    // call back in case information could be lost
    Run := PWideChar(SW);
    // only ask if there's at least one Unicode character in the text
    while (Run^>#0) and (run^<=#255) do
      Inc(Run);
    // Note: The application can still set FSaveUnicode to True in the callback.
    if Run^ <> WideNull then
      DoConfirmConversion(Allowed);
  end;

  if Allowed then
  begin
    // only save if allowed
    case SaveFormat of
      sfUTF16LSB :
        begin
          if WithBOM then
            Stream.WriteBuffer(BOM_UTF16_LSB[0],SizeOf(BOM_UTF16_LSB));
          Stream.WriteBuffer(SW[1],Length(SW)*SizeOf(UTF16));
          FSaved := True;
        end;
      sfUTF16MSB :
        begin
          if WithBOM then
            Stream.WriteBuffer(BOM_UTF16_MSB[0],SizeOf(BOM_UTF16_MSB));
          StrSwapByteOrder(PWideChar(SW));
          Stream.WriteBuffer(SW[1],Length(SW)*SizeOf(UTF16));
          FSaved := True;
        end;
      sfUTF8 :
        begin
          if WithBOM then
            Stream.WriteBuffer(BOM_UTF8[0],SizeOf(BOM_UTF8));
          SA := WideStringToUTF8(SW);
          Stream.WriteBuffer(SA[1],Length(SA)*SizeOf(UTF8));
          FSaved := True;
        end;
      sfAnsi :
        begin
          SA := WideStringToStringEx(SW,CodePageFromLocale(FLanguage));
          Stream.WriteBuffer(SA[1],Length(SA)*SizeOf(Char));
          FSaved := True;
        end;
    end;
  end;
end;

procedure TWideStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TWideStrings.SetCommaText(const Value: WideString);
var
  P, P1: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while P^ in [WideChar(#1)..WideSpace] do
      Inc(P);
    while P^ <> WideNull do
    begin
      if P^ = '"' then
        S := WideExtractQuotedStr(P, '"')
      else
      begin
        P1 := P;
        while (P^ > WideSpace) and (P^ <> ',') do 
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);

      while P^ in [WideChar(#1)..WideSpace] do
        Inc(P);
      if P^ = ',' then
      begin
        repeat
          Inc(P);
        until not (P^ in [WideChar(#1)..WideSpace]);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.SetText(const Value: WideString);
var
  Head,
  Tail: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);
    while Head^ <> WideNull do
    begin
      Tail := Head;
      while not (Tail^ in [WideNull, WideLineFeed, WideCarriageReturn, WideVerticalTab, WideFormFeed]) and
        (Tail^ <> WideLineSeparator) and (Tail^ <> WideParagraphSeparator) do
        Inc(Tail);
      SetString(S, Head, Tail - Head);
      Add(S);
      Head := Tail;
      if Head^ <> WideNull then
      begin
        Inc(Head);
        if (Tail^ = WideCarriageReturn) and (Head^ = WideLineFeed) then
          Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWideStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TWideStrings.SetNormalizationForm(const Value: TNormalizationForm);
var
  I: Integer;
begin
  if FNormalizationForm <> Value then
  begin
    FNormalizationForm := Value;
    if FNormalizationForm <> nfNone then
    begin
      // renormalize all strings according to the new form
      for I := 0 to GetCount - 1 do
        Put(I, WideNormalize(Get(I), FNormalizationForm));
    end;
  end;
end;

procedure TWideStrings.SetValue(const Name, Value: WideString);
var
  I : Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then
      I := Add('');
    Put(I, Name + '=' + Value);
  end
  else
  begin
    if I >= 0 then
      Delete(I);
  end;
end;

procedure TWideStrings.WriteData(Writer: TWriter);
begin
  Writer.WriteWideString(GetTextStr);
end;

//=== { TWideStringList } ====================================================

destructor TWideStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  Clear;
  inherited Destroy;
end;

function TWideStringList.Add(const S: WideString): Integer;
begin
  if not Sorted then
    Result := FCount
  else
  begin
    if Find(S, Result) then
    begin
      case Duplicates of
        dupIgnore:
          Exit;
        dupError:
          Error(SDuplicateString, 0);
      end;
    end;
  end;
  InsertItem(Result, S);
end;

procedure TWideStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TWideStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TWideStringList.Clear;
{$IFDEF OWN_WIDESTRING_MEMMGR}
var
  I: Integer;
{$ENDIF OWN_WIDESTRING_MEMMGR}
begin
  if FCount <> 0 then
  begin
    Changing;
    {$IFDEF OWN_WIDESTRING_MEMMGR}
    for I := 0 to FCount - 1 do
      with FList[I] do
        if TDynWideCharArray(FString) <> nil then
          TDynWideCharArray(FString) := nil;
    {$ENDIF OWN_WIDESTRING_MEMMGR}
    // this will automatically finalize the array
    FList := nil;
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TWideStringList.Delete(Index: Integer);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;

  {$IFDEF OWN_WIDESTRING_MEMMGR}
  SetListString(Index, '');
  {$ELSE}
  FList[Index].FString := '';
  {$ENDIF OWN_WIDESTRING_MEMMGR}
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TWideStringItem));
    Pointer(FList[FCount].FString) := nil; // avoid freeing the string, the address is now used in another element
  end;
  Changed;
end;

procedure TWideStringList.Exchange(Index1, Index2: Integer);
begin
  if Cardinal(Index1) >= Cardinal(FCount) then
    Error(SListIndexError, Index1);
  if Cardinal(Index2) >= Cardinal(FCount) then
    Error(SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TWideStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TWideStringItem;
begin
  Temp := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Temp;
end;

function TWideStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
  NormString: WideString;
begin
  Result := False;
  NormString := WideNormalize(S, FNormalizationForm);
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := WideCompareText(FList[I].FString, NormString, FLanguage);
    if C < 0 then
      L := I+1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function TWideStringList.Get(Index: Integer): WideString;
{$IFDEF OWN_WIDESTRING_MEMMGR}
var
  Len: Integer;
{$ENDIF OWN_WIDESTRING_MEMMGR}
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  {$IFDEF OWN_WIDESTRING_MEMMGR}
  with FList[Index] do
  begin
    Len := Length(TDynWideCharArray(FString));
    if Len > 0 then
    begin
      SetLength(Result, Len - 1); // exclude #0
      if Result <> '' then
        System.Move(FString^, Result[1], Len * SizeOf(WideChar));
    end
    else
      Result := '';
  end;
  {$ELSE}
  Result := FList[Index].FString;
  {$ENDIF OWN_WIDESTRING_MEMMGR}
end;

function TWideStringList.GetCapacity: Integer;
begin
  Result := Length(FList);
end;

function TWideStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TWideStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Result := FList[Index].FObject;
end;

procedure TWideStringList.Grow;
var
  Delta,
  Len: Integer;
begin
  Len := Length(FList);
  if Len > 64 then
    Delta := Len div 4
  else
  begin
    if Len > 8 then
      Delta := 16
    else
      Delta := 4;
  end;
  SetCapacity(Len + Delta);
end;

function TWideStringList.IndexOf(const S: WideString): Integer;
begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else
    if not Find(S, Result) then
      Result := -1;
end;

procedure TWideStringList.Insert(Index: Integer; const S: WideString);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if Cardinal(Index) > Cardinal(FCount) then
    Error(SListIndexError, Index);
  InsertItem(Index, S);
end;

{$IFDEF OWN_WIDESTRING_MEMMGR}
procedure TWideStringList.SetListString(Index: Integer; const S: WideString);
var
  Len: Integer;
  A: TDynWideCharArray;
begin
  with FList[Index] do
  begin
    Pointer(A) := TDynWideCharArray(FString);
    if A <> nil then
      A := nil; // free memory

    Len := Length(S);
    if Len > 0 then
    begin
      SetLength(A, Len + 1); // include #0
      System.Move(S[1], A[0], Len * SizeOf(WideChar));
      A[Len] := #0;
    end;

    FString := PWideChar(A);
    Pointer(A) := nil; // do not release the array on procedure exit
  end;
end;
{$ENDIF OWN_WIDESTRING_MEMMGR}

procedure TWideStringList.InsertItem(Index: Integer; const S: WideString);
begin
  Changing;
  if FCount = Length(FList) then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1], (FCount - Index) * SizeOf(TWideStringItem));
  with FList[Index] do
  begin
    Pointer(FString) := nil; // avoid freeing the string, the address is now used in another element
    FObject := nil;
    if (FNormalizationForm <> nfNone) and (Length(S) > 0) then
    {$IFDEF OWN_WIDESTRING_MEMMGR}
      SetListString(Index, WideNormalize(S, FNormalizationForm))
    else
      SetListString(Index, S);
    {$ELSE}
      FString := WideNormalize(S, FNormalizationForm)
    else
      FString := S;
    {$ENDIF OWN_WIDESTRING_MEMMGR}
  end;
  Inc(FCount);
  Changed;
end;

procedure TWideStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then
    Error(SSortedListError, 0);
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;

  if (FNormalizationForm <> nfNone) and (Length(S) > 0) then
  {$IFDEF OWN_WIDESTRING_MEMMGR}
    SetListString(Index, WideNormalize(S, FNormalizationForm))
  else
    SetListString(Index, S);
  {$ELSE}
    FList[Index].FString := WideNormalize(S, FNormalizationForm)
  else
    FList[Index].FString := S;
  {$ENDIF OWN_WIDESTRING_MEMMGR}
  Changed;
end;

procedure TWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(SListIndexError, Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

procedure TWideStringList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P: WideString;
begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FString;
    repeat
      while WideCompareText(FList[I].FString, P, FLanguage) < 0 do
        Inc(I);
      while WideCompareText(FList[J].FString, P, FLanguage) > 0 do
        Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TWideStringList.SetCapacity(NewCapacity: Integer);
begin
  SetLength(FList, NewCapacity);
  if NewCapacity < FCount then
    FCount := NewCapacity;
end;

procedure TWideStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

procedure TWideStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

procedure TWideStringList.Sort;
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1);
    Changed;
  end;
end;

procedure TWideStringList.SetLanguage(Value: LCID);
begin
  inherited SetLanguage(Value);
  if Sorted then
    Sort;
end;

//----------------- functions for null terminated strings ------------------------------------------

function StrLenW(Str: PWideChar): Cardinal;
// returns number of characters in a string excluding the null terminator
asm
       MOV     EDX, EDI
       MOV     EDI, EAX
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       MOV     EAX, 0FFFFFFFEH
       SUB     EAX, ECX
       MOV     EDI, EDX
end;

function StrEndW(Str: PWideChar): PWideChar;
// returns a pointer to the end of a null terminated string
asm
       MOV     EDX, EDI
       MOV     EDI, EAX
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       LEA     EAX, [EDI - 2]
       MOV     EDI, EDX
end;

function StrMoveW(Dest, Source: PWideChar; Count: Cardinal): PWideChar;
// Copies the specified number of characters to the destination string and returns Dest
// also as result. Dest must have enough room to store at least Count characters.
asm
       PUSH    ESI
       PUSH    EDI
       MOV     ESI, EDX
       MOV     EDI, EAX
       MOV     EDX, ECX
       CMP     EDI, ESI
       JG      @@1
       JE      @@2
       SHR     ECX, 1
       REP     MOVSD
       MOV     ECX, EDX
       AND     ECX, 1
       REP     MOVSW
       JMP     @@2
@@1:
       LEA     ESI, [ESI + 2 * ECX - 2]
       LEA     EDI, [EDI + 2 * ECX - 2]
       STD
       AND     ECX, 1
       REP     MOVSW
       SUB     EDI, 2
       SUB     ESI, 2
       MOV     ECX, EDX
       SHR     ECX, 1
       REP     MOVSD
       CLD
@@2:
       POP EDI
       POP ESI
end;

function StrCopyW(Dest, Source: PWideChar): PWideChar;
// copies Source to Dest and returns Dest
asm
       PUSH    EDI
       PUSH    ESI
       MOV     ESI, EAX
       MOV     EDI, EDX
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       NOT     ECX
       MOV     EDI, ESI
       MOV     ESI, EDX
       MOV     EDX, ECX
       MOV     EAX, EDI
       SHR     ECX, 1
       REP     MOVSD
       MOV     ECX, EDX
       AND     ECX, 1
       REP     MOVSW
       POP     ESI
       POP     EDI

end;

function StrECopyW(Dest, Source: PWideChar): PWideChar;
// copies Source to Dest and returns a pointer to the null character ending the string
asm
       PUSH    EDI
       PUSH    ESI
       MOV     ESI, EAX
       MOV     EDI, EDX
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       NOT     ECX
       MOV     EDI, ESI
       MOV     ESI, EDX
       MOV     EDX, ECX
       SHR     ECX, 1
       REP     MOVSD
       MOV     ECX, EDX
       AND     ECX, 1
       REP     MOVSW
       LEA     EAX, [EDI - 2]
       POP     ESI
       POP     EDI

end;

function StrLCopyW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
// copies a specified maximum number of characters from Source to Dest
asm
       PUSH    EDI
       PUSH    ESI
       PUSH    EBX
       MOV     ESI, EAX
       MOV     EDI, EDX
       MOV     EBX, ECX
       XOR     AX, AX
       TEST    ECX, ECX
       JZ      @@1
       REPNE   SCASW
       JNE     @@1
       INC     ECX
@@1:
       SUB     EBX, ECX
       MOV     EDI, ESI
       MOV     ESI, EDX
       MOV     EDX, EDI
       MOV     ECX, EBX
       SHR     ECX, 1
       REP     MOVSD
       MOV     ECX, EBX
       AND     ECX, 1
       REP     MOVSW
       STOSW
       MOV     EAX, EDX
       POP     EBX
       POP     ESI
       POP     EDI
end;

function StrPCopyWW(Dest: PWideChar; const Source: WideString): PWideChar;
// copies a Pascal-style WideString to a null-terminated wide string
begin
  Result := StrLCopyW(Dest, PWideChar(Source), Length(Source));
end;

function StrPCopyW(Dest: PWideChar; const Source: string): PWideChar;
// copies a Pascal-style string to a null-terminated wide string
begin
  Result := StrPLCopyW(Dest, Source, Cardinal(Length(Source)));
  Result[Length(Source)] := WideNull;
end;

function StrPLCopyWW(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
// copies characters from a Pascal-style WideString into a null-terminated wide string
begin
  Result := StrLCopyW(Dest, PWideChar(Source), MaxLen);
end;

function StrPLCopyW(Dest: PWideChar; const Source: string; MaxLen: Cardinal): PWideChar;
// copies characters from a Pascal-style string into a null-terminated wide string
asm
       PUSH EDI
       PUSH ESI
       MOV EDI, EAX
       MOV ESI, EDX
       MOV EDX, EAX
       XOR AX, AX
@@1:   LODSB
       STOSW
       DEC ECX
       JNZ @@1
       MOV EAX, EDX
       POP ESI
       POP EDI
end;

function StrCatW(Dest: PWideChar; const Source: PWideChar): PWideChar;
// appends a copy of Source to the end of Dest and returns the concatenated string
begin
  StrCopyW(StrEndW(Dest), Source);
  Result := Dest;
end;

// appends a specified maximum number of WideCharacters to string

function StrLCatW(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
asm
       PUSH    EDI
       PUSH    ESI
       PUSH    EBX
       MOV     EDI, Dest
       MOV     ESI, Source
       MOV     EBX, MaxLen
       SHL     EBX, 1
       CALL    StrEndW
       MOV     ECX, EDI
       ADD     ECX, EBX
       SUB     ECX, EAX
       JBE     @@1
       MOV     EDX, ESI
       SHR     ECX, 1
       CALL    StrLCopyW
@@1:
       MOV     EAX, EDI
       POP     EBX
       POP     ESI
       POP     EDI
end;

const
  // data used to bring UTF-16 coded strings into correct UTF-32 order for correct comparation
  UTF16Fixup: array [0..31] of Word = (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    $2000, $F800, $F800, $F800, $F800
  );

function StrCompW(const Str1, Str2: PWideChar): Integer;
// Binary comparation of Str1 and Str2 with surrogate fix-up.
// Returns < 0 if Str1 is smaller in binary order than Str2, = 0 if both strings are
// equal and > 0 if Str1 is larger than Str2.
//
// This code is based on an idea of Markus W. Scherer (IBM).
// Note: The surrogate fix-up is necessary because some single value code points have
//       larger values than surrogates which are in UTF-32 actually larger.
var
  C1, C2: Word;
  Run1, Run2: PWideChar;
begin
  Run1 := Str1;
  Run2 := Str2;
  repeat
    C1 := Word(Run1^);
    C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
    C2 := Word(Run2^);
    C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

    // now C1 and C2 are in UTF-32-compatible order
    Result := Integer(C1) - Integer(C2);
    if(Result <> 0) or (C1 = 0) or (C2 = 0) then
      Break;
    Inc(Run1);
    Inc(Run2);
  until False;

  // If the strings have different lengths but the comparation returned equity so far
  // then adjust the result so that the longer string is marked as the larger one.
  if Result = 0 then
    Result := (Run1 - Str1) - (Run2 - Str2);
end;

function StrICompW(const Str1, Str2: PWideChar): Integer;
// Compares Str1 to Str2 without case sensitivity.
// See also comments in StrCompW, but keep in mind that case folding might result in
// one-to-many mappings which must be considered here.
var
  C1, C2: Word;
  S1, S2: PWideChar;
  Run1, Run2: PWideChar;
  Folded1, Folded2: WideString;
begin
  // Because of size changes of the string when doing case folding
  // it is unavoidable to convert both strings completely in advance.
  S1 := Str1;
  S2 := Str2;
  Folded1 := '';
  while S1^ <> #0 do
  begin
    Folded1 := Folded1 + WideCaseFolding(S1^);
    Inc(S1);
  end;
  
  Folded2 := '';
  while S2^ <> #0 do
  begin
    Folded2 := Folded2 + WideCaseFolding(S2^);
    Inc(S2);
  end;

  Run1 := PWideChar(Folded1);
  Run2 := PWideChar(Folded2);
  repeat
    C1 := Word(Run1^);
    C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
    C2 := Word(Run2^);
    C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

    // now C1 and C2 are in UTF-32-compatible order
    Result := Integer(C1) - Integer(C2);
    if(Result <> 0) or (C1 = 0) or (C2 = 0) then
      Break;
    Inc(Run1);
    Inc(Run2);
  until False;

  // If the strings have different lengths but the comparation returned equity so far
  // then adjust the result so that the longer string is marked as the larger one.
  if Result = 0 then
    Result := (Run1 - PWideChar(Folded1)) - (Run2 - PWideChar(Folded2));
end;

function StrLICompW(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
// compares strings up to MaxLen code points
// see also StrICompW
var
  S1, S2: PWideChar;
  C1, C2: Word;
  Run1, Run2: PWideChar;
  Folded1, Folded2: WideString;
begin
  if MaxLen > 0 then
  begin
    // Because of size changes of the string when doing case folding
    // it is unavoidable to convert both strings completely in advance.
    S1 := Str1;
    S2 := Str2;
    Folded1 := '';
    while S1^ <> #0 do
    begin
      Folded1 := Folded1 + WideCaseFolding(S1^);
      Inc(S1);
    end;

    Folded2 := '';
    while S2^ <> #0 do
    begin
      Folded2 := Folded2 + WideCaseFolding(S2^);
      Inc(S2);
    end;

    Run1 := PWideChar(Folded1);
    Run2 := PWideChar(Folded2);

    repeat
      C1 := Word(Run1^);
      C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
      C2 := Word(Run2^);
      C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

      // now C1 and C2 are in UTF-32-compatible order
      { TODO : surrogates take up 2 words and are counted twice here, count them only once }
      Result := Integer(C1) - Integer(C2);
      Dec(MaxLen);
      if(Result <> 0) or (C1 = 0) or (C2 = 0) or (MaxLen = 0) then
        Break;
      Inc(Run1);
      Inc(Run2);
    until False;
  end
  else
    Result := 0;
end;

function StrLCompW(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
// compares strings up to MaxLen code points
// see also StrCompW
var
  S1, S2: PWideChar;
  C1, C2: Word;
begin
  if MaxLen > 0 then
  begin
    S1 := Str1;
    S2 := Str2;
    repeat
      C1 := Word(S1^);
      C1 := Word(C1 + UTF16Fixup[C1 shr 11]);
      C2 := Word(S2^);
      C2 := Word(C2 + UTF16Fixup[C2 shr 11]);

      // now C1 and C2 are in UTF-32-compatible order
      { TODO : surrogates take up 2 words and are counted twice here, count them only once }
      Result := Integer(C1) - Integer(C2);
      Dec(MaxLen);
      if(Result <> 0) or (C1 = 0) or (C2 = 0) or (MaxLen = 0) then
        Break;
      Inc(S1);
      Inc(S2);
    until False;
  end
  else
    Result := 0;
end;

function StrNScanW(const Str1, Str2: PWideChar): Integer;
// Determines where (in Str1) the first time one of the characters of Str2 appear.
// The result is the length of a string part of Str1 where none of the characters of
// Str2 do appear (not counting the trailing #0 and starting with position 0 in Str1).
var
  Run: PWideChar;
begin
  Result := -1;
  if (Str1 <> nil) and (Str2 <> nil) then
  begin
    Run := Str1;
    while Run^ <> #0 do
    begin
      if StrScanW(Str2, Run^) <> nil then
        Break;
      Inc(Run);
    end;
    Result := Run - Str1;
  end;
end;

function StrRNScanW(const Str1, Str2: PWideChar): Integer;
// This function does the same as StrRNScanW but uses Str1 in reverse order. This
// means Str1 points to the last character of a string, is traversed reversely
// and terminates with a starting #0. This is useful for parsing strings stored
// in reversed macro buffers etc.
var
  Run: PWideChar;
begin
  Result := -1;
  if (Str1 <> nil) and (Str2 <> nil) then
  begin
    Run := Str1;
    while Run^ <> #0 do
    begin
      if StrScanW(Str2, Run^) <> nil then
        Break;
      Dec(Run);
    end;
    Result := Str1 - Run;
  end;
end;

// returns a pointer to first occurrence of a specified character in a string

function StrScanW(Str: PWideChar; Chr: WideChar): PWideChar;
asm
        PUSH    EDI
        PUSH    EAX
        MOV     EDI, Str
        MOV     ECX, 0FFFFFFFFH
        XOR     AX, AX
        REPNE   SCASW
        NOT     ECX
        POP     EDI
        MOV     AX, Chr
        REPNE   SCASW
        MOV     EAX, 0
        JNE     @@1
        MOV     EAX, EDI
        SUB     EAX, 2
@@1:
        POP     EDI
end;

// Returns a pointer to first occurrence of a specified character in a string
// or nil if not found.
// Note: this is just a binary search for the specified character and there's no
//       check for a terminating null. Instead at most StrLen characters are
//       searched. This makes this function extremly fast.
//
// on enter EAX contains Str, EDX contains Chr and ECX StrLen
// on exit EAX contains result pointer or nil

function StrScanW(Str: PWideChar; Chr: WideChar; StrLen: Cardinal): PWideChar;
asm
       TEST    EAX, EAX
       JZ      @@Exit        // get out if the string is nil or StrLen is 0
       JCXZ    @@Exit
@@Loop:
       CMP     [EAX], DX     // this unrolled loop is actually faster on modern processors
       JE      @@Exit        // than REP SCASW
       ADD     EAX, 2
       DEC     ECX
       JNZ     @@Loop
       XOR     EAX, EAX
@@Exit:
end;

// returns a pointer to the last occurance of Chr in Str

function StrRScanW(Str: PWideChar; Chr: WideChar): PWideChar;
asm
       PUSH    EDI
       MOV     EDI, Str
       MOV     ECX, 0FFFFFFFFH
       XOR     AX, AX
       REPNE   SCASW
       NOT     ECX
       STD
       SUB     EDI, 2
       MOV     AX, Chr
       REPNE   SCASW
       MOV     EAX, 0
       JNE     @@1
       MOV     EAX, EDI
       ADD     EAX, 2
@@1:
       CLD
       POP     EDI
end;

// returns a pointer to the first occurance of SubStr in Str

function StrPosW(Str, SubStr: PWideChar): PWideChar;
asm
       PUSH    EDI
       PUSH    ESI
       PUSH    EBX
       OR      EAX, EAX
       JZ      @@2
       OR      EDX, EDX
       JZ      @@2
       MOV     EBX, EAX
       MOV     EDI, EDX
       XOR     AX, AX
       MOV     ECX, 0FFFFFFFFH
       REPNE   SCASW
       NOT     ECX
       DEC     ECX
       JZ      @@2
       MOV     ESI, ECX
       MOV     EDI, EBX
       MOV     ECX, 0FFFFFFFFH
       REPNE   SCASW
       NOT     ECX
       SUB     ECX, ESI
       JBE     @@2
       MOV     EDI, EBX
       LEA     EBX, [ESI - 1]
@@1:
       MOV     ESI, EDX
       LODSW
       REPNE   SCASW
       JNE     @@2
       MOV     EAX, ECX
       PUSH    EDI
       MOV     ECX, EBX
       REPE    CMPSW
       POP     EDI
       MOV     ECX, EAX
       JNE     @@1
       LEA     EAX, [EDI - 2]
       JMP     @@3
@@2:
       XOR     EAX, EAX
@@3:
       POP     EBX
       POP     ESI
       POP     EDI
end;

function StrAllocW(WideSize: Cardinal): PWideChar;
// Allocates a buffer for a null-terminated wide string and returns a pointer
// to the first character of the string.
begin
  WideSize := SizeOf(WideChar) * WideSize + SizeOf(Cardinal);
  Result := AllocMem(WideSize);
  Cardinal(Pointer(Result)^) := WideSize;
  Inc(Result, SizeOf(Cardinal) div SizeOf(WideChar));
end;

function StrBufSizeW(const Str: PWideChar): Cardinal;
// Returns max number of wide characters that can be stored in a buffer
// allocated by StrAllocW.
var
  P: PWideChar;
begin
  if Str <> nil then
  begin
    P := Str;
    Dec(P, SizeOf(Cardinal) div SizeOf(WideChar));
    Result := (Cardinal(PInteger(P)^) - SizeOf(Cardinal)) div SizeOf(WideChar);
  end
  else
    Result := 0;
end;

function StrNewW(const Str: PWideChar): PWideChar;
// Duplicates the given string (if not nil) and returns the address of the new string.
var
  Size: Cardinal;
begin
  if Str = nil then
    Result := nil
  else
  begin
    Size := StrLenW(Str) + 1;
    Result := StrMoveW(StrAllocW(Size), Str, Size);
  end;
end;

function StrNewW(const Str: WideString): PWideChar;
begin
  Result := StrNewW(PWideChar(Str));
end;

procedure StrDisposeW(Str: PWideChar);
// releases a string allocated with StrNewW or StrAllocW
begin
  if Str <> nil then
  begin
    Dec(Str, SizeOf(Cardinal) div SizeOf(WideChar));
    FreeMem(Str);
  end;
end;

procedure StrDisposeAndNilW(var Str: PWideChar);
begin
  StrDisposeW(Str);
  Str := nil;
end;

// exchanges in each character of the given string the low order and high order
// byte to go from LSB to MSB and vice versa.
// EAX contains address of string

procedure StrSwapByteOrder(Str: PWideChar);
asm
       PUSH    ESI
       PUSH    EDI
       MOV     ESI, EAX
       MOV     EDI, ESI
       XOR     EAX, EAX // clear high order byte to be able to use 32bit operand below
@@1:
       LODSW
       OR      EAX, EAX
       JZ      @@2
       XCHG    AL, AH
       STOSW
       JMP     @@1
@@2:
       POP     EDI
       POP     ESI
end;

function WideAdjustLineBreaks(const S: WideString): WideString;
var
  Source,
  SourceEnd,
  Dest: PWideChar;
begin
  Source := Pointer(S);
  SourceEnd := Source + Length(S);

  Source := Pointer(S);
  SetString(Result, nil, SourceEnd - Source);
  Dest := Pointer(Result);

  while Source < SourceEnd do
  begin
    case Source^ of
      WideLineFeed:
        begin
          Dest^ := WideLineSeparator;
          Inc(Dest);
          Inc(Source);
        end;
      WideCarriageReturn:
        begin
          Dest^ := WideLineSeparator;
          Inc(Dest);
          Inc(Source);
          if Source^ = WideLineFeed then
            Inc(Source);
        end;
    else
      Dest^ := Source^;
      Inc(Dest);
      Inc(Source);
    end;
  end;

  SetLength(Result, (Integer(Dest) - Integer(Result)) div 2);
end;

function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
// works like QuotedStr from SysUtils.pas but can insert any quotation character
var
  P, Src,
  Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := StrScanW(PWideChar(S), Quote);
  while (P <> nil) do
  begin
    Inc(P);
    Inc(AddCount);
    P := StrScanW(P, Quote);
  end;

  if AddCount = 0 then
    Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := StrScanW(Src, Quote);
    repeat
      Inc(P);
      Move(Src^, Dest^, 2 * (P - Src));
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := StrScanW(Src, Quote);
    until P = nil;
    P := StrEndW(Src);
    Move(Src^, Dest^, 2 * (P - Src));
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;
end;

function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
// extracts a string enclosed in quote characters given by Quote
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then
    Exit;

  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := StrScanW(Src, Quote);

  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then
      Break;
    Inc(Src);
    Inc(DropCount);
    Src := StrScanW(Src, Quote);
  end;

  if Src = nil then
    Src := StrEndW(P);
  if (Src - P) <= 1 then
    Exit;

  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := StrScanW(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then
        Break;
      Move(P^, Dest^, 2 * (Src - P));
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := StrScanW(Src, Quote);
    end;
    if Src = nil then
      Src := StrEndW(P);
    Move(P^, Dest^, 2 * (Src - P - 1));
  end;
end;

function WideStringOfChar(C: WideChar; Count: Cardinal): WideString;
// returns a string of Count characters filled with C
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 1 to Count do
    Result[I] := C;
end;

function WideTrim(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (UnicodeIsWhiteSpace(UCS4(S[I])) or UnicodeIsControl(UCS4(S[I]))) do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while UnicodeIsWhiteSpace(UCS4(S[L])) or UnicodeIsControl(UCS4(S[L])) do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function WideTrimLeft(const S: WideString): WideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (UnicodeIsWhiteSpace(UCS4(S[I])) or UnicodeIsControl(UCS4(S[I]))) do
    Inc(I);
  Result := Copy(S, I, Maxint);
end;

function WideTrimRight(const S: WideString): WideString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (UnicodeIsWhiteSpace(UCS4(S[I])) or UnicodeIsControl(UCS4(S[I]))) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

// returns the index of character Ch in S, starts searching at index Index
// Note: This is a quick memory search. No attempt is made to interpret either
// the given charcter nor the string (ligatures, modifiers, surrogates etc.)
// Code from Azret Botash.

function WideCharPos(const S: WideString; const Ch: WideChar; const Index: Integer): Integer;
asm
       TEST    EAX,EAX        // make sure we are not null
       JZ      @@StrIsNil
       DEC     ECX            // make index zero based
       JL      @@IdxIsSmall
       PUSH    EBX
       PUSH    EDI
       MOV     EDI, EAX       // EDI := S
       XOR     EAX, EAX
       MOV     AX, DX         // AX := Ch
       MOV     EDX, [EDI - 4] // EDX := Length(S) * 2
       SHR     EDX, 1         // EDX := EDX div 2
       MOV     EBX, EDX       // save the length to calc. result
       SUB     EDX, ECX       // EDX = EDX - Index = # of chars to scan
       JLE     @@IdxIsBig
       SHL     ECX, 1         // two bytes per char
       ADD     EDI, ECX       // point to index'th char
       MOV     ECX, EDX       // loop counter
       REPNE   SCASW
       JNE     @@NoMatch
       MOV     EAX, EBX       // result := saved length -
       SUB     EAX, ECX       // loop counter value
       POP     EDI
       POP     EBX
       RET
@@IdxIsBig:
@@NoMatch:
       XOR     EAX,EAX
       POP     EDI
       POP     EBX
       RET
@@IdxIsSmall:
       XOR     EAX, EAX
@@StrIsNil:
end;

function WideComposeHangul(const Source: WideString): WideString;
var
  Len: Integer;
  Ch, Last: WideChar;
  I: Integer;
  LIndex, VIndex,
  SIndex, TIndex: Integer;
begin
  Result := '';
  Len := Length(Source);
  if Len > 0 then
  begin
    Last := Source[1];
    Result := Last;

    for I := 2 to Len do
    begin
      Ch := Source[I];

      // 1. check to see if two current characters are L and V
      LIndex := Word(Last) - LBase;
      if (0 <= LIndex) and (LIndex < LCount) then
      begin
        VIndex := Word(Ch) - VBase;
        if (0 <= VIndex) and (VIndex < VCount) then
        begin
          // make syllable of form LV
          Last := WideChar((SBase + (LIndex * VCount + VIndex) * TCount));
          Result[Length(Result)] := Last; // reset last
          Continue; // discard Ch
        end;
      end;

      // 2. check to see if two current characters are LV and T
      SIndex := Word(Last) - SBase;
      if (0 <= SIndex) and (SIndex < SCount) and ((SIndex mod TCount) = 0) then
      begin
        TIndex := Word(Ch) - TBase;
        if (0 <= TIndex) and (TIndex <= TCount) then
        begin
          // make syllable of form LVT
          Inc(Word(Last), TIndex);
          Result[Length(Result)] := Last; // reset last
          Continue; // discard Ch
        end;
      end;

      // if neither case was true, just add the character
      Last := Ch;
      Result := Result + Ch;
    end;
  end;
end;

// Returns canonical composition of characters in S.

function WideCompose(const S: WideString): WideString;
var
  Buffer: array of UCS4;
  LastInPos, InPos, OutPos, BufferSize, NbProcessed: Integer;
  Composite: UCS4;
begin
  // Set an arbitrary length for the result. This is automatically done when checking
  // for hangul composition.
  Result := WideComposeHangul(S);

  if Result = '' then
    Exit;

  if Compositions = nil then
    LoadCompositionData;

  LastInPos := Length(Result);
  if LastInPos > MaxCompositionSize then
    SetLength(Buffer, MaxCompositionSize)
  else
    SetLength(Buffer, LastInPos);

  BufferSize := 0;
  InPos := 0;
  OutPos := 0;

  while (InPos < LastInPos) or (BufferSize > 0) do
  begin
    // fill buffer from input

    while BufferSize < Length(Buffer) do
    begin
      if InPos < LastInPos then
      begin
        Inc(InPos);
        Buffer[BufferSize] := UCS4(Result[InPos]);
        Inc(BufferSize);
      end
      else
        SetLength(Buffer, BufferSize);
    end;

    if Length(Buffer) = 0 then
      Break;

    NbProcessed := UnicodeCompose(Buffer, Composite);
    if NbProcessed = 0 then
      Break;

    if BufferSize > NbProcessed then
      Move(Buffer[NbProcessed], Buffer[0], (BufferSize - NbProcessed) * SizeOf(UCS4));
    Dec(BufferSize, NbProcessed);

    Inc(OutPos);
    Result[OutPos] := UCS2(Composite);
  end;
  // since we have likely shortened the source string we have to set the correct length on exit
  SetLength(Result, OutPos);
end;

procedure FixCanonical(var S: WideString);
// Examines S and reorders all combining marks in the string so that they are in canonical order.
var
  I: Integer;
  Temp: WideChar;
  CurrentClass,
  LastClass: Cardinal;
begin
  I := Length(S);
  if I > 1 then
  begin
    CurrentClass := CanonicalCombiningClass(UCS4(S[I]));
    repeat
      Dec(I);
      LastClass := CurrentClass;
      CurrentClass := CanonicalCombiningClass(UCS4(S[I]));

      // A swap is presumed to be rare (and a double-swap very rare),
      // so don't worry about efficiency here.
      if (CurrentClass > LastClass) and (LastClass > 0) then
      begin                                        
        // swap characters
        Temp := S[I];
        S[I] := S[I + 1];
        S[I + 1] := Temp;

        // if not at end, backup (one further, to compensate for loop)
        if I < Length(S) - 1 then
          Inc(I, 2);
        // reset type, since we swapped.
        CurrentClass := CanonicalCombiningClass(UCS4(S[I]));
      end;
    until I = 1;
  end;
end;

procedure GetDecompositions(Compatible: Boolean; Code: UCS4; var Buffer: TUCS4Array);
// helper function to recursively decompose a code point
var
  Decomp: TUCS4Array;
  I: Integer;
begin
  Decomp := UnicodeDecompose(Code, Compatible);
  if Assigned(Decomp) then
  begin
    for I := 0 to High(Decomp) do
      GetDecompositions(Compatible, Decomp[I], Buffer);
  end
  else // if no decomp, append
  begin
    I := Length(Buffer);
    SetLength(Buffer, I + 1);
    Buffer[I] := Code;
  end;
end;

function WideDecompose(const S: WideString; Compatible: Boolean): WideString;
// returns a string with all characters of S but decomposed, e.g.  is returned as E^ etc.
var
  I, J: Integer;
  Decomp: TUCS4Array;
begin  
  Result := '';
  Decomp := nil;

  // iterate through each source code point
  for I := 1 to Length(S) do
  begin
    Decomp := nil;
    GetDecompositions(Compatible, UCS4(S[I]), Decomp);
    if Decomp = nil then
      Result := Result + S[I]
    else
      for J := 0 to High(Decomp) do
        Result := Result + WideChar(Decomp[J]);
  end;

  // combining marks must be sorted according to their canonical combining class
  FixCanonical(Result);
end;

//----------------- general purpose case mapping ---------------------------------------------------

// Note that most of the assigned code points don't have a case mapping and are therefore
// returned as they are. Other code points, however, might be converted into several characters
// like the german  (eszett) whose upper case mapping is SS.

function WideCaseFolding(C: WideChar): WideString;
// Special case folding function to map a string to either its lower case or
// to special cases. This can be used for case-insensitive comparation.
var
  I: Integer;
  Mapping: TUCS4Array;
begin
  Mapping := UnicodeCaseFold(UCS4(C));
  SetLength(Result, Length(Mapping));
  for I := 0 to High(Mapping) do
    Result[I + 1] := WideChar(Mapping[I]);
end;

function WideCaseFolding(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + WideCaseFolding(S[I]);
end;

function WideLowerCase(C: WideChar): WideString;
var
  I: Integer;
  Mapping: TUCS4Array;
begin
  Mapping := UnicodeToLower(UCS4(C));
  SetLength(Result, Length(Mapping));
  for I := 0 to High(Mapping) do
    Result[I + 1] := WideChar(Mapping[I]);
end;

function WideLowerCase(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + WideLowerCase(S[I]);
end;

function WideNormalize(const S: WideString; Form: TNormalizationForm): WideString;
var
  Temp: WideString;
  Compatible: Boolean;
begin
  Result := S;

  if Form = nfNone then
    Exit; // No normalization needed.

  Compatible := Form in [nfKC, nfKD];
  if Form in [nfD, nfKD] then
    Result := WideDecompose(S, Compatible)
  else
  begin
    Temp := WideDecompose(S, Compatible);
    Result := WideCompose(Temp);
  end;
end;

function WideSameText(const Str1, Str2: WideString): Boolean;
// Compares both strings case-insensitively and returns True if both are equal, otherwise False is returned.
begin
  Result := Length(Str1) = Length(Str2);
  if Result then
    Result := StrICompW(PWideChar(Str1), PWideChar(Str2)) = 0;
end;

function WideTitleCase(C: WideChar): WideString;
var
  I: Integer;
  Mapping: TUCS4Array;
begin
  Mapping := UnicodeToTitle(UCS4(C));
  SetLength(Result, Length(Mapping));
  for I := 0 to High(Mapping) do
    Result[I + 1] := WideChar(Mapping[I]);
end;

function WideTitleCase(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + WideTitleCase(S[I]);
end;

function WideUpperCase(C: WideChar): WideString;
var
  I: Integer;
  Mapping: TUCS4Array;
begin
  Mapping := UnicodeToUpper(UCS4(C));
  SetLength(Result, Length(Mapping));
  for I := 0 to High(Mapping) do
    Result[I + 1] := WideChar(Mapping[I]);
end;

function WideUpperCase(const S: WideString): WideString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + WideUpperCase(S[I]);
end;

//----------------- character test routines --------------------------------------------------------

function UnicodeIsAlpha(C: UCS4): Boolean; // Is the character alphabetic?
begin
  Result := CategoryLookup(C, ClassLetter);
end;

function UnicodeIsDigit(C: UCS4): Boolean; // Is the character a digit?
begin
  Result := CategoryLookup(C, [ccNumberDecimalDigit]);
end;

function UnicodeIsAlphaNum(C: UCS4): Boolean; // Is the character alphabetic or a number?
begin
  Result := CategoryLookup(C, ClassLetter + [ccNumberDecimalDigit]);
end;

function UnicodeIsCased(C: UCS4): Boolean;
// Is the character a "cased" character, i.e. either lower case, title case or upper case
begin
  Result := CategoryLookup(C, [ccLetterLowercase, ccLetterTitleCase, ccLetterUppercase]);
end;

function UnicodeIsControl(C: UCS4): Boolean;
// Is the character a control character?
begin
  Result := CategoryLookup(C, [ccOtherControl, ccOtherFormat]);
end;

function UnicodeIsSpace(C: UCS4): Boolean;
// Is the character a spacing character?
begin
  Result := CategoryLookup(C, ClassSpace);
end;

function UnicodeIsWhiteSpace(C: UCS4): Boolean;
// Is the character a white space character (same as UnicodeIsSpace plus
// tabulator, new line etc.)?
begin
  Result := CategoryLookup(C, ClassSpace + [ccWhiteSpace, ccSegmentSeparator]);
end;

function UnicodeIsBlank(C: UCS4): Boolean;
// Is the character a space separator?
begin
  Result := CategoryLookup(C, [ccSeparatorSpace]);
end;

function UnicodeIsPunctuation(C: UCS4): Boolean;
// Is the character a punctuation mark?
begin
  Result := CategoryLookup(C, ClassPunctuation);
end;

function UnicodeIsGraph(C: UCS4): Boolean;
// Is the character graphical?
begin
  Result := CategoryLookup(C, ClassMark + ClassNumber + ClassLetter + ClassPunctuation + ClassSymbol);
end;

function UnicodeIsPrintable(C: UCS4): Boolean;
// Is the character printable?
begin
  Result := CategoryLookup(C, ClassMark + ClassNumber + ClassLetter + ClassPunctuation + ClassSymbol +
    [ccSeparatorSpace]);
end;

function UnicodeIsUpper(C: UCS4): Boolean;
// Is the character already upper case?
begin
  Result := CategoryLookup(C, [ccLetterUppercase]);
end;

function UnicodeIsLower(C: UCS4): Boolean;
// Is the character already lower case?
begin
  Result := CategoryLookup(C, [ccLetterLowercase]);
end;

function UnicodeIsTitle(C: UCS4): Boolean;
// Is the character already title case?
begin
  Result := CategoryLookup(C, [ccLetterTitlecase]);
end;

function UnicodeIsHexDigit(C: UCS4): Boolean;
// Is the character a hex digit?
begin
  Result := CategoryLookup(C, [ccHexDigit]);
end;

function UnicodeIsIsoControl(C: UCS4): Boolean;
// Is the character a C0 control character (< 32)?
begin
  Result := CategoryLookup(C, [ccOtherControl]);
end;

function UnicodeIsFormatControl(C: UCS4): Boolean;
// Is the character a format control character?
begin
  Result := CategoryLookup(C, [ccOtherFormat]);
end;

function UnicodeIsSymbol(C: UCS4): Boolean;
// Is the character a symbol?
begin
  Result := CategoryLookup(C, ClassSymbol);
end;

function UnicodeIsNumber(C: UCS4): Boolean;
// Is the character a number or digit?
begin
  Result := CategoryLookup(C, ClassNumber);
end;

function UnicodeIsNonSpacing(C: UCS4): Boolean;
// Is the character non-spacing?
begin
  Result := CategoryLookup(C, [ccMarkNonSpacing]);
end;

function UnicodeIsOpenPunctuation(C: UCS4): Boolean;
// Is the character an open/left punctuation (e.g. '[')?
begin
  Result := CategoryLookup(C, [ccPunctuationOpen]);
end;

function UnicodeIsClosePunctuation(C: UCS4): Boolean;
// Is the character an close/right punctuation (e.g. ']')?
begin
  Result := CategoryLookup(C, [ccPunctuationClose]);
end;

function UnicodeIsInitialPunctuation(C: UCS4): Boolean;
// Is the character an initial punctuation (e.g. U+2018 LEFT SINGLE QUOTATION MARK)?
begin
  Result := CategoryLookup(C, [ccPunctuationInitialQuote]);
end;

function UnicodeIsFinalPunctuation(C: UCS4): Boolean;
// Is the character a final punctuation (e.g. U+2019 RIGHT SINGLE QUOTATION MARK)?
begin
  Result := CategoryLookup(C, [ccPunctuationFinalQuote]);
end;

function UnicodeIsComposed(C: UCS4): Boolean;
// Can the character be decomposed into a set of other characters?
begin
  Result := CategoryLookup(C, [ccComposed]);
end;

function UnicodeIsQuotationMark(C: UCS4): Boolean;
// Is the character one of the many quotation marks?
begin
  Result := CategoryLookup(C, [ccQuotationMark]);
end;

function UnicodeIsSymmetric(C: UCS4): Boolean;
// Is the character one that has an opposite form (i.e. <>)?
begin
  Result := CategoryLookup(C, [ccSymmetric]);
end;

function UnicodeIsMirroring(C: UCS4): Boolean;
// Is the character mirroring (superset of symmetric)?
begin
  Result := CategoryLookup(C, [ccMirroring]);
end;

function UnicodeIsNonBreaking(C: UCS4): Boolean;
// Is the character non-breaking (i.e. non-breaking space)?
begin
  Result := CategoryLookup(C, [ccNonBreaking]);
end;

function UnicodeIsRightToLeft(C: UCS4): Boolean;
// Does the character have strong right-to-left directionality (i.e. Arabic letters)?
begin
  Result := CategoryLookup(C, [ccRightToLeft]);
end;

function UnicodeIsLeftToRight(C: UCS4): Boolean;
// Does the character have strong left-to-right directionality (i.e. Latin letters)?
begin
  Result := CategoryLookup(C, [ccLeftToRight]);
end;

function UnicodeIsStrong(C: UCS4): Boolean;
// Does the character have strong directionality?
begin
  Result := CategoryLookup(C, [ccLeftToRight, ccRightToLeft]);
end;

function UnicodeIsWeak(C: UCS4): Boolean;
// Does the character have weak directionality (i.e. numbers)?
begin
  Result := CategoryLookup(C, ClassEuropeanNumber + [ccArabicNumber, ccCommonNumberSeparator]);
end;

function UnicodeIsNeutral(C: UCS4): Boolean;
// Does the character have neutral directionality (i.e. whitespace)?
begin
  Result := CategoryLookup(C, [ccSeparatorParagraph, ccSegmentSeparator, ccWhiteSpace, ccOtherNeutrals]);
end;

function UnicodeIsSeparator(C: UCS4): Boolean;
// Is the character a block or segment separator?
begin
  Result := CategoryLookup(C, [ccSeparatorParagraph, ccSegmentSeparator]);
end;

function UnicodeIsMark(C: UCS4): Boolean;
// Is the character a mark of some kind?
begin
  Result := CategoryLookup(C, ClassMark);
end;

function UnicodeIsModifier(C: UCS4): Boolean;
// Is the character a letter modifier?
begin
  Result := CategoryLookup(C, [ccLetterModifier]);
end;

function UnicodeIsLetterNumber(C: UCS4): Boolean;
// Is the character a number represented by a letter?
begin
  Result := CategoryLookup(C, [ccNumberLetter]);
end;

function UnicodeIsConnectionPunctuation(C: UCS4): Boolean;
// Is the character connecting punctuation?
begin
  Result := CategoryLookup(C, [ccPunctuationConnector]);
end;

function UnicodeIsDash(C: UCS4): Boolean;
// Is the character a dash punctuation?
begin
  Result := CategoryLookup(C, [ccPunctuationDash]);
end;

function UnicodeIsMath(C: UCS4): Boolean;
// Is the character a math character?
begin
  Result := CategoryLookup(C, [ccSymbolMath]);
end;

function UnicodeIsCurrency(C: UCS4): Boolean;
// Is the character a currency character?
begin
  Result := CategoryLookup(C, [ccSymbolCurrency]);
end;

function UnicodeIsModifierSymbol(C: UCS4): Boolean;
// Is the character a modifier symbol?
begin
  Result := CategoryLookup(C, [ccSymbolModifier]);
end;

function UnicodeIsNonSpacingMark(C: UCS4): Boolean;
// Is the character a non-spacing mark?
begin
  Result := CategoryLookup(C, [ccMarkNonSpacing]);
end;

function UnicodeIsSpacingMark(C: UCS4): Boolean;
// Is the character a spacing mark?
begin
  Result := CategoryLookup(C, [ccMarkSpacingCombining]);
end;

function UnicodeIsEnclosing(C: UCS4): Boolean;
// Is the character enclosing (i.e. enclosing box)?
begin
  Result := CategoryLookup(C, [ccMarkEnclosing]);
end;

function UnicodeIsPrivate(C: UCS4): Boolean;
// Is the character from the Private Use Area?
begin
  Result := CategoryLookup(C, [ccOtherPrivate]);
end;

function UnicodeIsSurrogate(C: UCS4): Boolean;
// Is the character one of the surrogate codes?
begin
  Result := CategoryLookup(C, [ccOtherSurrogate]);
end;

function UnicodeIsLineSeparator(C: UCS4): Boolean;
// Is the character a line separator?
begin
  Result := CategoryLookup(C, [ccSeparatorLine]);
end;

function UnicodeIsParagraphSeparator(C: UCS4): Boolean;
// Is th character a paragraph separator;
begin
  Result := CategoryLookup(C, [ccSeparatorParagraph]);
end;

function UnicodeIsIdentifierStart(C: UCS4): Boolean;
// Can the character begin an identifier?
begin
  Result := CategoryLookup(C, ClassLetter + [ccNumberLetter]);
end;

function UnicodeIsIdentifierPart(C: UCS4): Boolean;
// Can the character appear in an identifier?
begin
  Result := CategoryLookup(C, ClassLetter + [ccNumberLetter, ccMarkNonSpacing, ccMarkSpacingCombining,
    ccNumberDecimalDigit, ccPunctuationConnector, ccOtherFormat]);
end;

function UnicodeIsDefined(C: UCS4): Boolean;
// Is the character defined (appears in one of the data files)?
begin
  Result := CategoryLookup(C, [ccAssigned]);
end;

function UnicodeIsUndefined(C: UCS4): Boolean;
// Is the character undefined (not assigned in the Unicode database)?
begin
  Result := not CategoryLookup(C, [ccAssigned]);
end;

function UnicodeIsHan(C: UCS4): Boolean;
// Is the character a Han ideograph?
begin
  Result := ((C >= $4E00) and (C <= $9FFF))  or ((C >= $F900) and (C <= $FAFF));
end;

function UnicodeIsHangul(C: UCS4): Boolean;
// Is the character a pre-composed Hangul syllable?
begin
  Result := (C >= $AC00) and (C <= $D7FF);
end;

// I need to fix a problem (introduced by MS) here. The first parameter can be a pointer
// (and is so defined) or can be a normal DWORD, depending on the dwFlags parameter.
// As usual, lpSrc has been translated to a var parameter. But this does not work in
// our case, hence the redeclaration of the function with a pointer as first parameter.

function TranslateCharsetInfoEx(lpSrc: PDWORD; var lpCs: TCharsetInfo; dwFlags: DWORD): BOOL; stdcall;
  external 'gdi32.dll' name 'TranslateCharsetInfo';

function GetCharSetFromLocale(Language: LCID; out FontCharSet: Byte): Boolean;
var
  CP: Cardinal;
  CSI: TCharsetInfo;
begin
  CP:= CodePageFromLocale(Language);
  Result := TranslateCharsetInfoEx(Pointer(CP), CSI, TCI_SRCCODEPAGE);
  if Result then
    FontCharset := CSI.ciCharset;
end;

function CharSetFromLocale(Language: LCID): Byte;
begin
  if not GetCharSetFromLocale(Language, Result) then
    RaiseLastOSError;
end;

function CodePageFromLocale(Language: LCID): Integer;
// determines the code page for a given locale
var
  Buf: array [0..6] of Char;
begin
  GetLocaleInfo(Language, LOCALE_IDefaultAnsiCodePage, Buf, 6);
  Result := StrToIntDef(Buf, GetACP);
end;

function KeyboardCodePage: Word;
begin
  Result := CodePageFromLocale(GetKeyboardLayout(0) and $FFFF);
end;

function KeyUnicode(C: Char): WideChar;
// converts the given character (as it comes with a WM_CHAR message) into its
// corresponding Unicode character depending on the active keyboard layout
begin
  MultiByteToWideChar(KeyboardCodePage, MB_USEGLYPHCHARS, @C, 1, @Result, 1);
end;

function CodeBlockRange(const CB: TUnicodeBlock): TUnicodeBlockRange;
// http://www.unicode.org/Public/5.0.0/ucd/Blocks.txt
begin
  Result := UnicodeBlockData[CB].Range;
end;


// Names taken from http://www.unicode.org/Public/5.0.0/ucd/Blocks.txt
function CodeBlockName(const CB: TUnicodeBlock): string;
begin
  Result := UnicodeBlockData[CB].Name;
end;

// Returns an ID for the Unicode code block to which C belongs.
// If C does not belong to any of the defined blocks then ubUndefined is returned.
// Note: the code blocks listed here are based on Unicode Version 5.0.0
function CodeBlockFromChar(const C: UCS4): TUnicodeBlock;
// http://www.unicode.org/Public/5.0.0/ucd/Blocks.txt
var
  L, H, I: TUnicodeBlock;
begin
  Result := ubUndefined;
  L := ubBasicLatin;
  H := High(TUnicodeBlock);
  while L <= H do
  begin
    I := TUnicodeBlock((Cardinal(L) + Cardinal(H)) shr 1);
    if (C >= UnicodeBlockData[I].Range.RangeStart) and (C <= UnicodeBlockData[I].Range.RangeEnd) then
    begin
      Result := I;
      Break;
    end
    else
    if C < UnicodeBlockData[I].Range.RangeStart then
    begin
      Dec(I);
      H := I;
    end
    else
    begin
      Inc(I);
      L := I;
    end;
  end;
end;


function CompareTextWin95(const W1, W2: WideString; Locale: LCID): Integer;
// special comparation function for Win9x since there's no system defined
// comparation function, returns -1 if W1 < W2, 0 if W1 = W2 or 1 if W1 > W2
var
  S1, S2: string;
  CP: Integer;
  L1, L2: Integer;
begin
  L1 := Length(W1);
  L2 := Length(W2);
  SetLength(S1, L1);
  SetLength(S2, L2);
  CP := CodePageFromLocale(Locale);
  WideCharToMultiByte(CP, 0, PWideChar(W1), L1, PChar(S1), L1, nil, nil);
  WideCharToMultiByte(CP, 0, PWideChar(W2), L2, PChar(S2), L2, nil, nil);
  Result := CompareStringA(Locale, NORM_IGNORECASE, PChar(S1), Length(S1),
    PChar(S2), Length(S2)) - 2;
end;

function CompareTextWinNT(const W1, W2: WideString; Locale: LCID): Integer;
// Wrapper function for WinNT since there's no system defined comparation function
// in Win9x and we need a central comparation function for TWideStringList.
// Returns -1 if W1 < W2, 0 if W1 = W2 or 1 if W1 > W2
begin
  Result := CompareStringW(Locale, NORM_IGNORECASE, PWideChar(W1), Length(W1),
    PWideChar(W2), Length(W2)) - 2;
end;

function StringToWideStringEx(const S: string; CodePage: Word): WideString;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(S);
  OutputLength := MultiByteToWideChar(CodePage, 0, PChar(S), InputLength, nil, 0);
  SetLength(Result, OutputLength);
  MultiByteToWideChar(CodePage, 0, PChar(S), InputLength, PWideChar(Result), OutputLength);
end;

function WideStringToStringEx(const WS: WideString; CodePage: Word): string;
var
  InputLength,
  OutputLength: Integer;
begin
  InputLength := Length(WS);
  OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, nil, 0, nil, nil);
  SetLength(Result, OutputLength);
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, PChar(Result), OutputLength, nil, nil);
end;

function TranslateString(const S: string; CP1, CP2: Word): string;
begin
  Result:= WideStringToStringEx(StringToWideStringEx(S, CP1), CP2);
end;

//----------------- conversion routines ------------------------------------------------------------

// Converts the given source ANSI string into a Unicode string by expanding each character
// from one byte to two bytes.
// EAX contains Source, EDX contains Target, ECX contains Count

procedure ExpandANSIString(const Source: PChar; Target: PWideChar; Count: Cardinal);
// Source in EAX
// Target in EDX
// Count in ECX
asm
       JECXZ   @@Finish           // go out if there is nothing to do (ECX = 0)
       PUSH    ESI
       MOV     ESI, EAX
       XOR     EAX, EAX
@@1:
       MOV     AL, [ESI]
       INC     ESI
       MOV     [EDX], AX
       ADD     EDX, 2
       DEC     ECX
       JNZ     @@1
       POP     ESI
@@Finish:
end;

const
  HalfShift: Integer = 10;

  HalfBase: UCS4 = $0010000;
  HalfMask: UCS4 = $3FF;

  OffsetsFromUTF8: array [0..5] of UCS4 =
    ($00000000, $00003080, $000E2080,
     $03C82080, $FA082080, $82082080);

  BytesFromUTF8: array [0..255] of Byte =
   (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5);

  FirstByteMark: array [0..6] of Byte =
    ($00, $00, $C0, $E0, $F0, $F8, $FC);

procedure FlagInvalidSequence(var StrPos: Integer; Increment: Integer; var Ch: UCS4); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Ch := ReplacementCharacter;
  Inc(StrPos, Increment);
  {$ELSE ~UNICODE_SILENT_FAILURE}
  StrPos := -1;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidUCS2Char(var Ch: UCS4);
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Ch := ReplacementCharacter;
  {$ELSE ~UNICODE_SILENT_FAILURE}
  raise EJclUnicodeError.CreateResFmt(@RsEInvalidUCS2Char, [Ch]);
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

procedure FlagInvalidSequence(var StrPos: Integer; Increment: Integer); overload;
begin
  {$IFDEF UNICODE_SILENT_FAILURE}
  Inc(StrPos, Increment);
  {$ELSE ~UNICODE_SILENT_FAILURE}
  StrPos := -1;
  {$ENDIF ~UNICODE_SILENT_FAILURE}
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF8 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF8GetNextChar(const S: AnsiString; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChNext: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      $00..$7F:
        // 1 byte to read
        Inc(StrPos);
      $C0..$DF:
        begin
          // 2 bytes to read
          if StrPos >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $1F) shl 6) or (ChNext and $3F);
          Inc(StrPos, 2);
        end;
      $E0..$EF:
        begin
          // 3 bytes to read
          if (StrPos + 1) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $0F) shl 12) or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 3);
        end;
      $F0..$F7:
        begin
          // 4 bytes to read
          if (StrPos + 2) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $07) shl 18) or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 4);
        end;
      $F8..$FB:
        begin
          // 5 bytes to read
          if (StrPos + 3) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $03) shl 24) or ((ChNext and $3F) shl 18);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 4]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 4, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 5);
        end;
      $FC..$FD:
        begin
          // 6 bytes to read
          if (StrPos + 4) >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result and $01) shl 30) or ((ChNext and $3F) shl 24);
          ChNext := UCS4(S[StrPos + 2]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 2, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 18);
          ChNext := UCS4(S[StrPos + 3]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 3, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 12);
          ChNext := UCS4(S[StrPos + 4]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 4, Result);
            Exit;
          end;
          Result := Result or ((ChNext and $3F) shl 6);
          ChNext := UCS4(S[StrPos + 5]);
          if (ChNext and $C0) <> $80 then
          begin
            FlagInvalidSequence(StrPos, 5, Result);
            Exit;
          end;
          Result := Result or (ChNext and $3F);
          Inc(StrPos, 6);
        end;
    else
      FlagInvalidSequence(StrPos, 1, Result);
      Exit;
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF8 sequence)
// StrPos will be incremented by the number of ansi chars that were skipped
// On return, NbSeq contains the number of UTF8 sequences that were skipped
function UTF8SkipChars(const S: AnsiString; var StrPos: Integer; var NbSeq: Integer): Boolean;
var
  StrLength: Integer;
  Ch: UCS4;
  Index: Integer;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  while (Index < NbSeq) and (StrPos > 0) do
  begin
    Ch := UCS4(S[StrPos]);

    case Ch of
      $00..$7F:
        // 1 byte to skip
        Inc(StrPos);
      $C0..$DF:
        // 2 bytes to skip
        if (StrPos >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
          Inc(StrPos, 2);
      $E0..$EF:
        // 3 bytes to skip
        if ((StrPos + 1) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
          Inc(StrPos, 3);
      $F0..$F7:
        // 4 bytes to skip
        if ((StrPos + 2) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
          Inc(StrPos, 4);
      $F8..$FB:
        // 5 bytes to skip
        if ((StrPos + 3) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
        if (UCS4(S[StrPos + 4]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 4)
        else
          Inc(StrPos, 5);
      $FC..$FD:
        // 6 bytes to skip
        if ((StrPos + 4) >= StrLength) or ((UCS4(S[StrPos + 1]) and $C0) <> $80) then
          FlagInvalidSequence(StrPos, 1)
        else
        if (UCS4(S[StrPos + 2]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 2)
        else
        if (UCS4(S[StrPos + 3]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 3)
        else
        if (UCS4(S[StrPos + 4]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 4)
        else
        if (UCS4(S[StrPos + 5]) and $C0) <> $80 then
          FlagInvalidSequence(StrPos, 5)
        else
          Inc(StrPos, 6);
    else
      FlagInvalidSequence(StrPos, 1);
    end;

    if StrPos <> -1 then
      Inc(Index);
    if (StrPos > StrLength) and (Index < NbSeq) then
    begin
      Result := False;
      Break;
    end;
  end;
  NbSeq := Index;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF8SetNextChar(var S: AnsiString; var StrPos: Integer; Ch: UCS4): Boolean;
var
  StrLength: Integer;
begin
  StrLength := Length(S);

  if Ch <= $7F then
  begin
    // 7 bits to store
    Result := StrPos <= StrLength;
    if Result then
    begin
      S[StrPos] := AnsiChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= $7FF then
  begin
    // 11 bits to store
    Result := StrPos < StrLength;
    if Result then
    begin
      S[StrPos] := AnsiChar($C0 or (Ch shr 6));  // 5 bits
      S[StrPos + 1] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 2);
    end;
  end
  else
  if Ch <= $FFFF then
  begin
    // 16 bits to store
    Result := StrPos < (StrLength - 1);
    if Result then
    begin
      S[StrPos] := AnsiChar($E0 or (Ch shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
  end
  else
  if Ch <= $1FFFFF then
  begin
    // 21 bits to store
    Result := StrPos < (StrLength - 2);
    if Result then
    begin
      S[StrPos] := AnsiChar($F0 or (Ch shr 18)); // 3 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 4);
    end;
  end
  else
  if Ch <= $3FFFFFF then
  begin
    // 26 bits to store
    Result := StrPos < (StrLength - 2);
    if Result then
    begin
      S[StrPos] := AnsiChar($F8 or (Ch shr 24)); // 2 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 5);
    end;
  end
  else
  if Ch <= MaximumUCS4 then
  begin
    // 31 bits to store
    Result := StrPos < (StrLength - 3);
    if Result then
    begin
      S[StrPos] := AnsiChar($FC or (Ch shr 30)); // 1 bits
      S[StrPos + 1] := AnsiChar(((Ch shr 24) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar(((Ch shr 18) and $3F) or $80); // 6 bits
      S[StrPos + 3] := AnsiChar(((Ch shr 12) and $3F) or $80); // 6 bits
      S[StrPos + 4] := AnsiChar(((Ch shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 5] := AnsiChar((Ch and $3F) or $80); // 6 bits
      Inc(StrPos, 6);
    end;
  end
  else
  begin
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StrPos < (StrLength - 1);
    if Result then
    begin
      S[StrPos] := AnsiChar($E0 or (ReplacementCharacter shr 12)); // 4 bits
      S[StrPos + 1] := AnsiChar(((ReplacementCharacter shr 6) and $3F) or $80); // 6 bits
      S[StrPos + 2] := AnsiChar((ReplacementCharacter and $3F) or $80); // 6 bits
      Inc(StrPos, 3);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were read
function UTF16GetNextChar(const S: WideString; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChNext: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= StrLength) and (StrPos > 0) then
  begin
    Result := UCS4(S[StrPos]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        begin
          // 2 bytes to read
          if StrPos >= StrLength then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          ChNext := UCS4(S[StrPos + 1]);
          if (ChNext < SurrogateLowStart) or (ChNext > SurrogateLowEnd) then
          begin
            FlagInvalidSequence(StrPos, 1, Result);
            Exit;
          end;
          Result := ((Result - SurrogateHighStart) shl HalfShift) +  (ChNext - SurrogateLowStart) + HalfBase;
          Inc(StrPos, 2);
        end;
      SurrogateLowStart..SurrogateLowEnd:
        FlagInvalidSequence(StrPos, 1, Result);
    else
      // 1 byte to read
      Inc(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// if UNICODE_SILENT_FAILURE is defined, invalid sequences will be replaced by ReplacementCharacter
// otherwise StrPos is set to -1 on return to flag an error (invalid UTF16 sequence)
// StrPos will be decremented by the number of chars that were read
function UTF16GetPreviousChar(const S: WideString; var StrPos: Integer): UCS4;
var
  StrLength: Integer;
  ChPrev: UCS4;
begin
  StrLength := Length(S);

  if (StrPos <= (StrLength + 1)) and (StrPos > 1) then
  begin
    Result := UCS4(S[StrPos - 1]);

    case Result of
      SurrogateHighStart..SurrogateHighEnd:
        FlagInvalidSequence(StrPos, -1, Result);
      SurrogateLowStart..SurrogateLowEnd:
        begin
          // 2 bytes to read
          if StrPos <= 2 then
          begin
            FlagInvalidSequence(StrPos, -1, Result);
            Exit;
          end;
          ChPrev := UCS4(S[StrPos - 2]);
          if (ChPrev < SurrogateHighStart) or (ChPrev > SurrogateHighEnd) then
          begin
            FlagInvalidSequence(StrPos, -1, Result);
            Exit;
          end;
          Result := ((ChPrev - SurrogateHighStart) shl HalfShift) +  (Result - SurrogateLowStart) + HalfBase;
          Dec(StrPos, 2);
        end;
    else
      // 1 byte to read
      Dec(StrPos);
    end;
  end
  else
  begin
    // StrPos > StrLength
    Result := 0;
    FlagInvalidSequence(StrPos, 0, Result);
  end;
end;

// returns False if String is too small
// if UNICODE_SILENT_FAILURE is not defined StrPos is set to -1 on error (invalid UTF16 sequence)
// StrPos will be incremented by the number of chars that were skipped
// On return, NbSeq contains the number of UTF16 sequences that were skipped
function UTF16SkipChars(const S: WideString; var StrPos: Integer; var NbSeq: Integer): Boolean;
var
  StrLength, Index: Integer;
  Ch, ChNext: UCS4;
begin
  Result := True;
  StrLength := Length(S);

  Index := 0;
  if NbSeq >= 0 then
    while (Index < NbSeq) and (StrPos > 0) do
    begin
      Ch := UCS4(S[StrPos]);
  
      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // 2 bytes to skip
          if StrPos >= StrLength then
            FlagInvalidSequence(StrPos, 1)
          else
          begin
            ChNext := UCS4(S[StrPos + 1]);
            if (ChNext < SurrogateLowStart) or (ChNext > SurrogateLowEnd) then
              FlagInvalidSequence(StrPos, 1)
            else
              Inc(StrPos, 2);
          end;
        SurrogateLowStart..SurrogateLowEnd:
          // error
          FlagInvalidSequence(StrPos, 1);
      else
        // 1 byte to skip
        Inc(StrPos);
      end;

      if StrPos <> -1 then
        Inc(Index);

      if (StrPos > StrLength) and (Index < NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end
  else
    while (Index > NbSeq) and (StrPos > 1) do
    begin
      Ch := UCS4(S[StrPos - 1]);

      case Ch of
        SurrogateHighStart..SurrogateHighEnd:
          // error
          FlagInvalidSequence(StrPos, -1);
        SurrogateLowStart..SurrogateLowEnd:
          // 2 bytes to skip
          if StrPos <= 2 then
            FlagInvalidSequence(StrPos, -1)
          else
          begin
            ChNext := UCS4(S[StrPos - 2]);
            if (ChNext < SurrogateHighStart) or (ChNext > SurrogateHighEnd) then
              FlagInvalidSequence(StrPos, -1)
            else
              Dec(StrPos, 2);
          end;
      else
        // 1 byte to skip
        Dec(StrPos);
      end;

      if StrPos <> -1 then
        Dec(Index);

      if (StrPos = 1) and (Index > NbSeq) then
      begin
        Result := False;
        Break;
      end;
    end;
  NbSeq := Index;
end;

// returns False on error:
//    - if an UCS4 character cannot be stored to an UTF-8 string:
//        - if UNICODE_SILENT_FAILURE is defined, ReplacementCharacter is added
//        - if UNICODE_SILENT_FAILURE is not defined, StrPos is set to -1
//    - StrPos > -1 flags string being too small, caller is responsible for allocating space
// StrPos will be incremented by the number of chars that were written
function UTF16SetNextChar(var S: WideString; var StrPos: Integer; Ch: UCS4): Boolean;
var
  StrLength: Integer;
begin
  StrLength := Length(S);

  if Ch <= MaximumUCS2 then
  begin
    // 16 bits to store in place
    Result := StrPos <= StrLength;
    if Result then
    begin
      S[StrPos] := WideChar(Ch);
      Inc(StrPos);
    end;
  end
  else
  if Ch <= MaximumUTF16 then
  begin
    // stores a surrogate pair
    Result := StrPos < StrLength;
    if Result then
    begin
      Ch := Ch - HalfBase;
      S[StrPos] := WideChar((Ch shr HalfShift) + SurrogateHighStart);
      S[StrPos + 1] := WideChar((Ch and HalfMask) + SurrogateLowStart);
      Inc(StrPos, 2);
    end;
  end
  else
  begin
    {$IFDEF UNICOLE_SILENT_FAILURE}
    // add ReplacementCharacter
    Result := StrPos <= StrLength;
    if Result then
    begin
      S[StrPos] := WideChar(ReplacementCharacter);
      Inc(StrPos, 1);
    end;
    {$ELSE ~UNICODE_SILENT_FAILURE}
    StrPos := -1;
    Result := False;
    {$ENDIF ~UNICODE_SILENT_FAILURE}
  end;
end;

function WideStringToUTF8(const S: WideString): AnsiString;
var
  SrcIndex, SrcLength, DestIndex: Integer;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 1 to SrcLength do
    begin
      UTF8SetNextChar(Result, DestIndex, UCS4(S[SrcIndex]));
      if DestIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function UTF8ToWideString(const S: AnsiString): WideString;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);

      if Ch > MaximumUCS2 then
        FlagInvalidUCS2Char(Ch);

      Result[DestIndex] := UCS2(Ch);
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function WideStringToUTF16(const S: WideString): WideString;
var
  SrcIndex, SrcLength, DestIndex: Integer;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength * 2); // assume worst case
    DestIndex := 1;

    for SrcIndex := 1 to SrcLength do
    begin
      UTF16SetNextChar(Result, DestIndex, UCS4(S[SrcIndex]));
      if DestIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function UTF16ToWideString(const S: WideString): WideString;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);

      if Ch > MaximumUCS2 then
        FlagInvalidUCS2Char(Ch);

      Result[DestIndex] := UCS2(Ch);
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function WideStringToUCS4(const S: WideString): TUCS4Array;
var
  Index, SrcLength: Integer;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // same length

    for Index := 0 to SrcLength - 1 do
      Result[Index] := UCS4(S[Index + 1]);
  end;
end;

function UCS4ToWideString(const S: TUCS4Array): WideString;
var
  Index, SrcLength: Integer;
  Ch: UCS4;
begin
  SrcLength := Length(S);
  if SrcLength = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength); // same length

    for Index := 0 to SrcLength - 1 do
    begin
      Ch := S[Index];

      if Ch > MaximumUCS2 then
        FlagInvalidUCS2Char(Ch);

      Result[Index + 1] := WideChar(Ch);
    end;
  end;
end;

function UTF8ToUTF16(const S: AnsiString): WideString;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);

      UTF16SetNextChar(Result, DestIndex, Ch);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function UTF16ToUTF8(const S: WideString): AnsiString;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S = '' then
    Result := ''
  else
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength * 3); // worste case

    SrcIndex := 1;
    DestIndex := 1;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);

      UTF8SetNextChar(Result, DestIndex, Ch);
    end;
    SetLength(Result, DestIndex - 1); // now fix up length
  end;
end;

function UTF8ToUCS4(const S: AnsiString): TUCS4Array;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF8GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);

      Result[DestIndex] := Ch;
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex); // now fix up length
  end;
end;

function UCS4ToUTF8(const S: TUCS4Array): AnsiString;
var
  SrcIndex, SrcLength, DestIndex: Integer;
begin
  SrcLength := Length(S);
  if Length(S) = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF8SetNextChar(Result, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function UTF16ToUCS4(const S: WideString): TUCS4Array;
var
  SrcIndex, SrcLength, DestIndex: Integer;
  Ch: UCS4;
begin
  if S <> '' then
  begin
    SrcLength := Length(S);
    SetLength(Result, SrcLength); // create enough room

    SrcIndex := 1;
    DestIndex := 0;
    while SrcIndex <= SrcLength do
    begin
      Ch := UTF16GetNextChar(S, SrcIndex);
      if SrcIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);

      Result[DestIndex] := Ch;
      Inc(DestIndex);
    end;
    SetLength(Result, DestIndex); // now fix up length
  end;
end;

function UCS4ToUTF16(const S: TUCS4Array): WideString;
var
  SrcIndex, SrcLength, DestIndex: Integer;
begin
  SrcLength := Length(S);
  if SrcLength = 0 then
    Result := ''
  else
  begin
    SetLength(Result, SrcLength * 3); // assume worst case
    DestIndex := 1;

    for SrcIndex := 0 to SrcLength - 1 do
    begin
      UTF16SetNextChar(Result, DestIndex, S[SrcIndex]);
      if DestIndex = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
    end;

    SetLength(Result, DestIndex - 1); // set to actual length
  end;
end;

function UTF8CharCount(const S: AnsiString): Integer;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Length(S);
  UTF8SkipChars(S, StrPos, Result);
  if StrPos = -1 then
    raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
end;

function UTF16CharCount(const S: WideString): Integer;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Length(S);
  UTF16SkipChars(S, StrPos, Result);
  if StrPos = -1 then
    raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
end;

function UCS2CharCount(const S: WideString): Integer;
begin
  Result := Length(S);
end;

function UCS4CharCount(const S: TUCS4Array): Integer;
begin
  Result := Length(S);
end;

function GetUCS4CharAt(const UTF8Str: AnsiString; Index: Integer; out Value: UCS4): Boolean; overload;
var
  StrPos: Integer;
begin
  StrPos := 1;
  Result := Index >= 0;
  if Result then
    Result := UTF8SkipChars(UTF8Str, StrPos, Index);
  if StrPos = -1 then
    raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
  Result := Result and (StrPos <= Length(UTF8Str));
  if Result then
  begin
    Value := UTF8GetNextChar(UTF8Str, StrPos);
    if StrPos = -1 then
      raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
  end;
end;

function GetUCS4CharAt(const WideStr: WideString; Index: Integer; out Value: UCS4; IsUTF16: Boolean = False): Boolean; overload;
var
  StrPos: Integer;
begin
  if IsUTF16 then
  begin
    StrPos := 1;
    Result := Index >= 0;
    if Result then
      Result := UTF16SkipChars(WideStr, StrPos, Index);
    if StrPos = -1 then
      raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
    Result := Result and (StrPos <= Length(WideStr));
    if Result then
    begin
      Value := UTF16GetNextChar(WideStr, StrPos);
      if StrPos = -1 then
        raise EJclUnicodeError.CreateRes(@RsEUnexpectedEOSeq);
    end;
  end
  else
  begin
    Result := (Index >= 1) and (Index <= Length(WideStr));
    Value := UCS4(WideStr[Index]);
  end;
end;

function GetUCS4CharAt(const UCS4Str: TUCS4Array; Index: Integer; out Value: UCS4): Boolean; overload;
begin
  Result := (Index >= 0) and (Index < Length(UCS4Str));
  if Result then
    Value := UCS4Str[Index];
end;

procedure PrepareUnicodeData;
// Prepares structures which are globally needed.
begin
  LoadInProgress := TJclCriticalSection.Create;

  if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
    @WideCompareText := @CompareTextWinNT
  else
    @WideCompareText := @CompareTextWin95;
end;

procedure FreeUnicodeData;
// Frees all data which has been allocated and which is not automatically freed by Delphi.
begin
  FreeAndNil(LoadInProgress);
end;

initialization
  PrepareUnicodeData;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FreeUnicodeData;

end.
