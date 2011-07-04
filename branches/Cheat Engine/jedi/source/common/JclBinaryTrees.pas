{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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
{ The Original Code is BinaryTree.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008)                         $ }
{ Revision:      $Rev:: 2376                                                                     $ }
{ Author:        $Author:: obones                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclBinaryTrees;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclAlgorithms, JclContainerIntf, JclSynch;

type
  TJclIntfBinaryNode = class
  public
    Value: IInterface;
    Left: TJclIntfBinaryNode;
    Right: TJclIntfBinaryNode;
    Parent: TJclIntfBinaryNode;
  end;

  TJclIntfBinaryTree = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclIntfEqualityComparer, IJclIntfComparer,
    IJclIntfCollection, IJclIntfTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclIntfBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function Equals(const ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(const AInterface: IInterface): Boolean;
    function RemoveAll(const ACollection: IJclIntfCollection): Boolean;
    function RetainAll(const ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntfIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfTree }
    function GetRoot: IJclIntfTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TIntfCompare);
    destructor Destroy; override;
    property Root: IJclIntfTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclAnsiStrBinaryNode = class
  public
    Value: AnsiString;
    Left: TJclAnsiStrBinaryNode;
    Right: TJclAnsiStrBinaryNode;
    Parent: TJclAnsiStrBinaryNode;
  end;

  TJclAnsiStrBinaryTree = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclAnsiStrComparer,
    IJclAnsiStrCollection, IJclAnsiStrTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclAnsiStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Equals(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function First: IJclAnsiStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclAnsiStrIterator; override;
    function Remove(const AString: AnsiString): Boolean; override;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrTree }
    function GetRoot: IJclAnsiStrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TAnsiStrCompare);
    destructor Destroy; override;
    property Root: IJclAnsiStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclWideStrBinaryNode = class
  public
    Value: WideString;
    Left: TJclWideStrBinaryNode;
    Right: TJclWideStrBinaryNode;
    Parent: TJclWideStrBinaryNode;
  end;

  TJclWideStrBinaryTree = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclWideStrComparer,
    IJclWideStrCollection, IJclWideStrTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclWideStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Equals(const ACollection: IJclWideStrCollection): Boolean; override;
    function First: IJclWideStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclWideStrIterator; override;
    function Remove(const AString: WideString): Boolean; override;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrTree }
    function GetRoot: IJclWideStrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TWideStrCompare);
    destructor Destroy; override;
    property Root: IJclWideStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryTree = TJclAnsiStrBinaryTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryTree = TJclWideStrBinaryTree;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleBinaryNode = class
  public
    Value: Single;
    Left: TJclSingleBinaryNode;
    Right: TJclSingleBinaryNode;
    Parent: TJclSingleBinaryNode;
  end;

  TJclSingleBinaryTree = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclSingleContainer, IJclSingleEqualityComparer, IJclSingleComparer,
    IJclSingleCollection, IJclSingleTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclSingleBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclSingleCollection }
    function Add(const AValue: Single): Boolean;
    function AddAll(const ACollection: IJclSingleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Single): Boolean;
    function ContainsAll(const ACollection: IJclSingleCollection): Boolean;
    function Equals(const ACollection: IJclSingleCollection): Boolean;
    function First: IJclSingleIterator;
    function IsEmpty: Boolean;
    function Last: IJclSingleIterator;
    function Remove(const AValue: Single): Boolean;
    function RemoveAll(const ACollection: IJclSingleCollection): Boolean;
    function RetainAll(const ACollection: IJclSingleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclSingleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSingleTree }
    function GetRoot: IJclSingleTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TSingleCompare);
    destructor Destroy; override;
    property Root: IJclSingleTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclDoubleBinaryNode = class
  public
    Value: Double;
    Left: TJclDoubleBinaryNode;
    Right: TJclDoubleBinaryNode;
    Parent: TJclDoubleBinaryNode;
  end;

  TJclDoubleBinaryTree = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclDoubleContainer, IJclDoubleEqualityComparer, IJclDoubleComparer,
    IJclDoubleCollection, IJclDoubleTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclDoubleBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclDoubleCollection }
    function Add(const AValue: Double): Boolean;
    function AddAll(const ACollection: IJclDoubleCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Double): Boolean;
    function ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
    function Equals(const ACollection: IJclDoubleCollection): Boolean;
    function First: IJclDoubleIterator;
    function IsEmpty: Boolean;
    function Last: IJclDoubleIterator;
    function Remove(const AValue: Double): Boolean;
    function RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
    function RetainAll(const ACollection: IJclDoubleCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclDoubleIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclDoubleTree }
    function GetRoot: IJclDoubleTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TDoubleCompare);
    destructor Destroy; override;
    property Root: IJclDoubleTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclExtendedBinaryNode = class
  public
    Value: Extended;
    Left: TJclExtendedBinaryNode;
    Right: TJclExtendedBinaryNode;
    Parent: TJclExtendedBinaryNode;
  end;

  TJclExtendedBinaryTree = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclExtendedContainer, IJclExtendedEqualityComparer, IJclExtendedComparer,
    IJclExtendedCollection, IJclExtendedTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclExtendedBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclExtendedCollection }
    function Add(const AValue: Extended): Boolean;
    function AddAll(const ACollection: IJclExtendedCollection): Boolean;
    procedure Clear;
    function Contains(const AValue: Extended): Boolean;
    function ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
    function Equals(const ACollection: IJclExtendedCollection): Boolean;
    function First: IJclExtendedIterator;
    function IsEmpty: Boolean;
    function Last: IJclExtendedIterator;
    function Remove(const AValue: Extended): Boolean;
    function RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
    function RetainAll(const ACollection: IJclExtendedCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclExtendedIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclExtendedTree }
    function GetRoot: IJclExtendedTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TExtendedCompare);
    destructor Destroy; override;
    property Root: IJclExtendedTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBinaryTree = TJclExtendedBinaryTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBinaryTree = TJclDoubleBinaryTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBinaryTree = TJclSingleBinaryTree;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerBinaryNode = class
  public
    Value: Integer;
    Left: TJclIntegerBinaryNode;
    Right: TJclIntegerBinaryNode;
    Parent: TJclIntegerBinaryNode;
  end;

  TJclIntegerBinaryTree = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclIntegerEqualityComparer, IJclIntegerComparer,
    IJclIntegerCollection, IJclIntegerTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclIntegerBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntegerCollection }
    function Add(AValue: Integer): Boolean;
    function AddAll(const ACollection: IJclIntegerCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Integer): Boolean;
    function ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
    function Equals(const ACollection: IJclIntegerCollection): Boolean;
    function First: IJclIntegerIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntegerIterator;
    function Remove(AValue: Integer): Boolean;
    function RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
    function RetainAll(const ACollection: IJclIntegerCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntegerIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntegerTree }
    function GetRoot: IJclIntegerTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TIntegerCompare);
    destructor Destroy; override;
    property Root: IJclIntegerTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclCardinalBinaryNode = class
  public
    Value: Cardinal;
    Left: TJclCardinalBinaryNode;
    Right: TJclCardinalBinaryNode;
    Parent: TJclCardinalBinaryNode;
  end;

  TJclCardinalBinaryTree = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCardinalEqualityComparer, IJclCardinalComparer,
    IJclCardinalCollection, IJclCardinalTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclCardinalBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCardinalCollection }
    function Add(AValue: Cardinal): Boolean;
    function AddAll(const ACollection: IJclCardinalCollection): Boolean;
    procedure Clear;
    function Contains(AValue: Cardinal): Boolean;
    function ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
    function Equals(const ACollection: IJclCardinalCollection): Boolean;
    function First: IJclCardinalIterator;
    function IsEmpty: Boolean;
    function Last: IJclCardinalIterator;
    function Remove(AValue: Cardinal): Boolean;
    function RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
    function RetainAll(const ACollection: IJclCardinalCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclCardinalIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCardinalTree }
    function GetRoot: IJclCardinalTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCardinalCompare);
    destructor Destroy; override;
    property Root: IJclCardinalTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclInt64BinaryNode = class
  public
    Value: Int64;
    Left: TJclInt64BinaryNode;
    Right: TJclInt64BinaryNode;
    Parent: TJclInt64BinaryNode;
  end;

  TJclInt64BinaryTree = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclInt64EqualityComparer, IJclInt64Comparer,
    IJclInt64Collection, IJclInt64Tree)
  private
    FMaxDepth: Integer;
    FRoot: TJclInt64BinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclInt64Collection }
    function Add(const AValue: Int64): Boolean;
    function AddAll(const ACollection: IJclInt64Collection): Boolean;
    procedure Clear;
    function Contains(const AValue: Int64): Boolean;
    function ContainsAll(const ACollection: IJclInt64Collection): Boolean;
    function Equals(const ACollection: IJclInt64Collection): Boolean;
    function First: IJclInt64Iterator;
    function IsEmpty: Boolean;
    function Last: IJclInt64Iterator;
    function Remove(const AValue: Int64): Boolean;
    function RemoveAll(const ACollection: IJclInt64Collection): Boolean;
    function RetainAll(const ACollection: IJclInt64Collection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclInt64Iterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclInt64Tree }
    function GetRoot: IJclInt64TreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TInt64Compare);
    destructor Destroy; override;
    property Root: IJclInt64TreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFNDEF CLR}
  TJclPtrBinaryNode = class
  public
    Value: Pointer;
    Left: TJclPtrBinaryNode;
    Right: TJclPtrBinaryNode;
    Parent: TJclPtrBinaryNode;
  end;

  TJclPtrBinaryTree = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclPtrEqualityComparer, IJclPtrComparer,
    IJclPtrCollection, IJclPtrTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclPtrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPtrCollection }
    function Add(APtr: Pointer): Boolean;
    function AddAll(const ACollection: IJclPtrCollection): Boolean;
    procedure Clear;
    function Contains(APtr: Pointer): Boolean;
    function ContainsAll(const ACollection: IJclPtrCollection): Boolean;
    function Equals(const ACollection: IJclPtrCollection): Boolean;
    function First: IJclPtrIterator;
    function IsEmpty: Boolean;
    function Last: IJclPtrIterator;
    function Remove(APtr: Pointer): Boolean;
    function RemoveAll(const ACollection: IJclPtrCollection): Boolean;
    function RetainAll(const ACollection: IJclPtrCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclPtrIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclPtrTree }
    function GetRoot: IJclPtrTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TPtrCompare);
    destructor Destroy; override;
    property Root: IJclPtrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;
  {$ENDIF ~CLR}

  TJclBinaryNode = class
  public
    Value: TObject;
    Left: TJclBinaryNode;
    Right: TJclBinaryNode;
    Parent: TJclBinaryNode;
  end;

  TJclBinaryTree = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclObjectOwner, IJclEqualityComparer, IJclComparer,
    IJclCollection, IJclTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function Equals(const ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclTree }
    function GetRoot: IJclTreeIterator;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCompare; AOwnsObjects: Boolean);
    destructor Destroy; override;
    property Root: IJclTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclBinaryNode<T> = class
  public
    Value: T;
    Left: TJclBinaryNode<T>;
    Right: TJclBinaryNode<T>;
    Parent: TJclBinaryNode<T>;
  end;

  TJclBinaryTree<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FMaxDepth: Integer;
    FRoot: TJclBinaryNode<T>;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function Equals(const ACollection: IJclCollection<T>): Boolean;
    function First: IJclIterator<T>;
    function IsEmpty: Boolean;
    function Last: IJclIterator<T>;
    function Remove(const AItem: T): Boolean;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator<T>;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclTree<T> }
    function GetRoot: IJclTreeIterator<T>;
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
  public
    constructor Create(AOwnsItems: Boolean);
    destructor Destroy; override;
    property Root: IJclTreeIterator<T> read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  // E = External helper to compare items
  TJclBinaryTreeE<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FComparer: IComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AComparer: IComparer<T>; AOwnsItems: Boolean);
    property Comparer: IComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclBinaryTreeF<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other
  TJclBinaryTreeI<T: IComparable<T>> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;
  {$ENDIF SUPPORTS_GENERICS}


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclBinaryTrees.pas $';
    Revision: '$Revision: 2376 $';
    Date: '$Date: 2008-06-05 15:35:37 +0200 (jeu., 05 juin 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

type
  TItrStart = (isFirst, isLast, isRoot);

type
  TIntfItr = class(TJclAbstractIterator, IJclIntfIterator, IJclIntfTreeIterator, IJclIntfBinaryTreeIterator)
  protected
    FCursor: TJclIntfBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclIntfCollection;
    FEqualityComparer: IJclIntfEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntfBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclIntfBinaryNode; virtual; abstract;
    { IJclIntfIterator }
    function Add(const AInterface: IInterface): Boolean;
    function Equals(const AIterator: IJclIntfIterator): Boolean;
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AInterface: IInterface): Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetObject(const AInterface: IInterface);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: IInterface read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfTreeIterator }
    function AddChild(const AInterface: IInterface): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): IInterface;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AInterface: IInterface): Integer;
    function InsertChild(Index: Integer; const AInterface: IInterface): Boolean;
    function Parent: IInterface;
    procedure SetChild(Index: Integer; const AInterface: IInterface);
    { IJclIntfBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: IInterface;
    function Right: IInterface;
  public
    constructor Create(const AOwnTree: IJclIntfCollection; ACursor: TJclIntfBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderIntfItr = class(TIntfItr, IJclIntfIterator, IJclIntfTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderIntfItr = class(TIntfItr, IJclIntfIterator, IJclIntfTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderIntfItr = class(TIntfItr, IJclIntfIterator, IJclIntfTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TIntfItr } ===========================================================

constructor TIntfItr.Create(const AOwnTree: IJclIntfCollection; ACursor: TJclIntfBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclIntfEqualityComparer;
end;

function TIntfItr.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnTree.Add(AInterface);
end;

function TIntfItr.AddChild(const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntfItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TIntfItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TIntfItr then
  begin
    ADest := TIntfItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TIntfItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntfItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.Equals(const AIterator: IJclIntfIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TIntfItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TIntfItr then
  begin
    ItrObj := TIntfItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TIntfItr.GetChild(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.GetObject: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.IndexOfChild(const AInterface: IInterface): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AInterface) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AInterface) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AInterface) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.Insert(const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.InsertChild(Index: Integer; const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.Left: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntfItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TIntfItr.Next: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.Parent: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.Previous: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntfItr.Remove;
var
  OldCursor: TJclIntfBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.Reset;
var
  NewCursor: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.Right: IInterface;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.SetChild(Index: Integer; const AInterface: IInterface);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderIntfItr } ===================================================

function TPreOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderIntfItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderIntfItr } ====================================================

function TInOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderIntfItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderIntfItr } ==================================================

function TPostOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderIntfItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

type
  TAnsiStrItr = class(TJclAbstractIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, IJclAnsiStrBinaryTreeIterator)
  protected
    FCursor: TJclAnsiStrBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclAnsiStrCollection;
    FEqualityComparer: IJclAnsiStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclAnsiStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; virtual; abstract;
    { IJclAnsiStrIterator }
    function Add(const AString: AnsiString): Boolean;
    function Equals(const AIterator: IJclAnsiStrIterator): Boolean;
    function GetString: AnsiString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: AnsiString): Boolean;
    function Next: AnsiString;
    function NextIndex: Integer;
    function Previous: AnsiString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: AnsiString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: AnsiString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrTreeIterator }
    function AddChild(const AString: AnsiString): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): AnsiString;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AString: AnsiString): Integer;
    function InsertChild(Index: Integer; const AString: AnsiString): Boolean;
    function Parent: AnsiString;
    procedure SetChild(Index: Integer; const AString: AnsiString);
    { IJclAnsiStrBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: AnsiString;
    function Right: AnsiString;
  public
    constructor Create(const AOwnTree: IJclAnsiStrCollection; ACursor: TJclAnsiStrBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TAnsiStrItr } ===========================================================

constructor TAnsiStrItr.Create(const AOwnTree: IJclAnsiStrCollection; ACursor: TJclAnsiStrBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclAnsiStrEqualityComparer;
end;

function TAnsiStrItr.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnTree.Add(AString);
end;

function TAnsiStrItr.AddChild(const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TAnsiStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TAnsiStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TAnsiStrItr then
  begin
    ADest := TAnsiStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TAnsiStrItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TAnsiStrItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.Equals(const AIterator: IJclAnsiStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TAnsiStrItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TAnsiStrItr then
  begin
    ItrObj := TAnsiStrItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TAnsiStrItr.GetChild(Index: Integer): AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.GetString: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.IndexOfChild(const AString: AnsiString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AString) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.Insert(const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.InsertChild(Index: Integer; const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.Left: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TAnsiStrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TAnsiStrItr.Next: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.Parent: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.Previous: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TAnsiStrItr.Remove;
var
  OldCursor: TJclAnsiStrBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.Reset;
var
  NewCursor: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.Right: AnsiString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.SetChild(Index: Integer; const AString: AnsiString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TAnsiStrItr.SetString(const AString: AnsiString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderAnsiStrItr } ===================================================

function TPreOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderAnsiStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderAnsiStrItr } ====================================================

function TInOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderAnsiStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderAnsiStrItr } ==================================================

function TPostOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderAnsiStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

type
  TWideStrItr = class(TJclAbstractIterator, IJclWideStrIterator, IJclWideStrTreeIterator, IJclWideStrBinaryTreeIterator)
  protected
    FCursor: TJclWideStrBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclWideStrCollection;
    FEqualityComparer: IJclWideStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclWideStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclWideStrBinaryNode; virtual; abstract;
    { IJclWideStrIterator }
    function Add(const AString: WideString): Boolean;
    function Equals(const AIterator: IJclWideStrIterator): Boolean;
    function GetString: WideString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: WideString): Boolean;
    function Next: WideString;
    function NextIndex: Integer;
    function Previous: WideString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetString(const AString: WideString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: WideString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrTreeIterator }
    function AddChild(const AString: WideString): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): WideString;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AString: WideString): Integer;
    function InsertChild(Index: Integer; const AString: WideString): Boolean;
    function Parent: WideString;
    procedure SetChild(Index: Integer; const AString: WideString);
    { IJclWideStrBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: WideString;
    function Right: WideString;
  public
    constructor Create(const AOwnTree: IJclWideStrCollection; ACursor: TJclWideStrBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, IJclWideStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, IJclWideStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, IJclWideStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TWideStrItr } ===========================================================

constructor TWideStrItr.Create(const AOwnTree: IJclWideStrCollection; ACursor: TJclWideStrBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclWideStrEqualityComparer;
end;

function TWideStrItr.Add(const AString: WideString): Boolean;
begin
  Result := FOwnTree.Add(AString);
end;

function TWideStrItr.AddChild(const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TWideStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TWideStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TWideStrItr then
  begin
    ADest := TWideStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TWideStrItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TWideStrItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.Equals(const AIterator: IJclWideStrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TWideStrItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TWideStrItr then
  begin
    ItrObj := TWideStrItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TWideStrItr.GetChild(Index: Integer): WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.GetString: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.IndexOfChild(const AString: WideString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AString) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AString) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.Insert(const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.InsertChild(Index: Integer; const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.Left: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TWideStrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TWideStrItr.Next: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.Parent: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.Previous: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TWideStrItr.Remove;
var
  OldCursor: TJclWideStrBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.Reset;
var
  NewCursor: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.Right: WideString;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.SetChild(Index: Integer; const AString: WideString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TWideStrItr.SetString(const AString: WideString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderWideStrItr } ===================================================

function TPreOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderWideStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderWideStrItr } ====================================================

function TInOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderWideStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderWideStrItr } ==================================================

function TPostOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderWideStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

type
  TSingleItr = class(TJclAbstractIterator, IJclSingleIterator, IJclSingleTreeIterator, IJclSingleBinaryTreeIterator)
  protected
    FCursor: TJclSingleBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclSingleCollection;
    FEqualityComparer: IJclSingleEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclSingleBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclSingleBinaryNode; virtual; abstract;
    { IJclSingleIterator }
    function Add(const AValue: Single): Boolean;
    function Equals(const AIterator: IJclSingleIterator): Boolean;
    function GetValue: Single;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Single): Boolean;
    function Next: Single;
    function NextIndex: Integer;
    function Previous: Single;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Single);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Single read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSingleTreeIterator }
    function AddChild(const AValue: Single): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): Single;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Single): Integer;
    function InsertChild(Index: Integer; const AValue: Single): Boolean;
    function Parent: Single;
    procedure SetChild(Index: Integer; const AValue: Single);
    { IJclSingleBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Single;
    function Right: Single;
  public
    constructor Create(const AOwnTree: IJclSingleCollection; ACursor: TJclSingleBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderSingleItr = class(TSingleItr, IJclSingleIterator, IJclSingleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderSingleItr = class(TSingleItr, IJclSingleIterator, IJclSingleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderSingleItr = class(TSingleItr, IJclSingleIterator, IJclSingleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleBinaryNode; override;
    function GetPreviousCursor: TJclSingleBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TSingleItr } ===========================================================

constructor TSingleItr.Create(const AOwnTree: IJclSingleCollection; ACursor: TJclSingleBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclSingleEqualityComparer;
end;

function TSingleItr.Add(const AValue: Single): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TSingleItr.AddChild(const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TSingleItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TSingleItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TSingleItr then
  begin
    ADest := TSingleItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TSingleItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TSingleItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TSingleItr.Equals(const AIterator: IJclSingleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TSingleItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TSingleItr then
  begin
    ItrObj := TSingleItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TSingleItr.GetChild(Index: Integer): Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.GetValue: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.IndexOfChild(const AValue: Single): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.Insert(const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TSingleItr.InsertChild(Index: Integer; const AValue: Single): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TSingleItr.Left: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TSingleItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TSingleItr.Next: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TSingleItr.Parent: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.Previous: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TSingleItr.Remove;
var
  OldCursor: TJclSingleBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.Reset;
var
  NewCursor: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.Right: Single;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.SetChild(Index: Integer; const AValue: Single);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TSingleItr.SetValue(const AValue: Single);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderSingleItr } ===================================================

function TPreOrderSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderSingleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderSingleItr.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderSingleItr.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderSingleItr } ====================================================

function TInOrderSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderSingleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderSingleItr.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderSingleItr.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderSingleItr } ==================================================

function TPostOrderSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderSingleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderSingleItr.GetNextCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderSingleItr.GetPreviousCursor: TJclSingleBinaryNode;
var
  LastRet: TJclSingleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

type
  TDoubleItr = class(TJclAbstractIterator, IJclDoubleIterator, IJclDoubleTreeIterator, IJclDoubleBinaryTreeIterator)
  protected
    FCursor: TJclDoubleBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclDoubleCollection;
    FEqualityComparer: IJclDoubleEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclDoubleBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclDoubleBinaryNode; virtual; abstract;
    { IJclDoubleIterator }
    function Add(const AValue: Double): Boolean;
    function Equals(const AIterator: IJclDoubleIterator): Boolean;
    function GetValue: Double;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Double): Boolean;
    function Next: Double;
    function NextIndex: Integer;
    function Previous: Double;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Double);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Double read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclDoubleTreeIterator }
    function AddChild(const AValue: Double): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): Double;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Double): Integer;
    function InsertChild(Index: Integer; const AValue: Double): Boolean;
    function Parent: Double;
    procedure SetChild(Index: Integer; const AValue: Double);
    { IJclDoubleBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Double;
    function Right: Double;
  public
    constructor Create(const AOwnTree: IJclDoubleCollection; ACursor: TJclDoubleBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderDoubleItr = class(TDoubleItr, IJclDoubleIterator, IJclDoubleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderDoubleItr = class(TDoubleItr, IJclDoubleIterator, IJclDoubleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderDoubleItr = class(TDoubleItr, IJclDoubleIterator, IJclDoubleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleBinaryNode; override;
    function GetPreviousCursor: TJclDoubleBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TDoubleItr } ===========================================================

constructor TDoubleItr.Create(const AOwnTree: IJclDoubleCollection; ACursor: TJclDoubleBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclDoubleEqualityComparer;
end;

function TDoubleItr.Add(const AValue: Double): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TDoubleItr.AddChild(const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TDoubleItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TDoubleItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TDoubleItr then
  begin
    ADest := TDoubleItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TDoubleItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TDoubleItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TDoubleItr.Equals(const AIterator: IJclDoubleIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TDoubleItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TDoubleItr then
  begin
    ItrObj := TDoubleItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TDoubleItr.GetChild(Index: Integer): Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.GetValue: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.IndexOfChild(const AValue: Double): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.Insert(const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TDoubleItr.InsertChild(Index: Integer; const AValue: Double): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TDoubleItr.Left: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TDoubleItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TDoubleItr.Next: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TDoubleItr.Parent: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.Previous: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TDoubleItr.Remove;
var
  OldCursor: TJclDoubleBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.Reset;
var
  NewCursor: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.Right: Double;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.SetChild(Index: Integer; const AValue: Double);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TDoubleItr.SetValue(const AValue: Double);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderDoubleItr } ===================================================

function TPreOrderDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderDoubleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderDoubleItr.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderDoubleItr.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderDoubleItr } ====================================================

function TInOrderDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderDoubleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderDoubleItr.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderDoubleItr.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderDoubleItr } ==================================================

function TPostOrderDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderDoubleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderDoubleItr.GetNextCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderDoubleItr.GetPreviousCursor: TJclDoubleBinaryNode;
var
  LastRet: TJclDoubleBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

type
  TExtendedItr = class(TJclAbstractIterator, IJclExtendedIterator, IJclExtendedTreeIterator, IJclExtendedBinaryTreeIterator)
  protected
    FCursor: TJclExtendedBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclExtendedCollection;
    FEqualityComparer: IJclExtendedEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclExtendedBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclExtendedBinaryNode; virtual; abstract;
    { IJclExtendedIterator }
    function Add(const AValue: Extended): Boolean;
    function Equals(const AIterator: IJclExtendedIterator): Boolean;
    function GetValue: Extended;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Extended): Boolean;
    function Next: Extended;
    function NextIndex: Integer;
    function Previous: Extended;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Extended);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Extended read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclExtendedTreeIterator }
    function AddChild(const AValue: Extended): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): Extended;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Extended): Integer;
    function InsertChild(Index: Integer; const AValue: Extended): Boolean;
    function Parent: Extended;
    procedure SetChild(Index: Integer; const AValue: Extended);
    { IJclExtendedBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Extended;
    function Right: Extended;
  public
    constructor Create(const AOwnTree: IJclExtendedCollection; ACursor: TJclExtendedBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderExtendedItr = class(TExtendedItr, IJclExtendedIterator, IJclExtendedTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderExtendedItr = class(TExtendedItr, IJclExtendedIterator, IJclExtendedTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderExtendedItr = class(TExtendedItr, IJclExtendedIterator, IJclExtendedTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedBinaryNode; override;
    function GetPreviousCursor: TJclExtendedBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TExtendedItr } ===========================================================

constructor TExtendedItr.Create(const AOwnTree: IJclExtendedCollection; ACursor: TJclExtendedBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclExtendedEqualityComparer;
end;

function TExtendedItr.Add(const AValue: Extended): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TExtendedItr.AddChild(const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TExtendedItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TExtendedItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TExtendedItr then
  begin
    ADest := TExtendedItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TExtendedItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TExtendedItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TExtendedItr.Equals(const AIterator: IJclExtendedIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TExtendedItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TExtendedItr then
  begin
    ItrObj := TExtendedItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TExtendedItr.GetChild(Index: Integer): Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.GetValue: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.IndexOfChild(const AValue: Extended): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.Insert(const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TExtendedItr.InsertChild(Index: Integer; const AValue: Extended): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TExtendedItr.Left: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TExtendedItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TExtendedItr.Next: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TExtendedItr.Parent: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.Previous: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0.0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TExtendedItr.Remove;
var
  OldCursor: TJclExtendedBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.Reset;
var
  NewCursor: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.Right: Extended;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.SetChild(Index: Integer; const AValue: Extended);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TExtendedItr.SetValue(const AValue: Extended);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderExtendedItr } ===================================================

function TPreOrderExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderExtendedItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderExtendedItr.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderExtendedItr.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderExtendedItr } ====================================================

function TInOrderExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderExtendedItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderExtendedItr.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderExtendedItr.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderExtendedItr } ==================================================

function TPostOrderExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderExtendedItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderExtendedItr.GetNextCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderExtendedItr.GetPreviousCursor: TJclExtendedBinaryNode;
var
  LastRet: TJclExtendedBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

type
  TIntegerItr = class(TJclAbstractIterator, IJclIntegerIterator, IJclIntegerTreeIterator, IJclIntegerBinaryTreeIterator)
  protected
    FCursor: TJclIntegerBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclIntegerCollection;
    FEqualityComparer: IJclIntegerEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntegerBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclIntegerBinaryNode; virtual; abstract;
    { IJclIntegerIterator }
    function Add(AValue: Integer): Boolean;
    function Equals(const AIterator: IJclIntegerIterator): Boolean;
    function GetValue: Integer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Integer): Boolean;
    function Next: Integer;
    function NextIndex: Integer;
    function Previous: Integer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(AValue: Integer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Integer read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntegerTreeIterator }
    function AddChild(AValue: Integer): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): Integer;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AValue: Integer): Integer;
    function InsertChild(Index: Integer; AValue: Integer): Boolean;
    function Parent: Integer;
    procedure SetChild(Index: Integer; AValue: Integer);
    { IJclIntegerBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Integer;
    function Right: Integer;
  public
    constructor Create(const AOwnTree: IJclIntegerCollection; ACursor: TJclIntegerBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderIntegerItr = class(TIntegerItr, IJclIntegerIterator, IJclIntegerTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderIntegerItr = class(TIntegerItr, IJclIntegerIterator, IJclIntegerTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderIntegerItr = class(TIntegerItr, IJclIntegerIterator, IJclIntegerTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerBinaryNode; override;
    function GetPreviousCursor: TJclIntegerBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TIntegerItr } ===========================================================

constructor TIntegerItr.Create(const AOwnTree: IJclIntegerCollection; ACursor: TJclIntegerBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclIntegerEqualityComparer;
end;

function TIntegerItr.Add(AValue: Integer): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TIntegerItr.AddChild(AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntegerItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TIntegerItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TIntegerItr then
  begin
    ADest := TIntegerItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TIntegerItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntegerItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TIntegerItr.Equals(const AIterator: IJclIntegerIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TIntegerItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TIntegerItr then
  begin
    ItrObj := TIntegerItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TIntegerItr.GetChild(Index: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.GetValue: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.IndexOfChild(AValue: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.Insert(AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TIntegerItr.InsertChild(Index: Integer; AValue: Integer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TIntegerItr.Left: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntegerItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TIntegerItr.Next: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TIntegerItr.Parent: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.Previous: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntegerItr.Remove;
var
  OldCursor: TJclIntegerBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.Reset;
var
  NewCursor: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.Right: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.SetChild(Index: Integer; AValue: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntegerItr.SetValue(AValue: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderIntegerItr } ===================================================

function TPreOrderIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderIntegerItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderIntegerItr.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderIntegerItr.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderIntegerItr } ====================================================

function TInOrderIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderIntegerItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderIntegerItr.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderIntegerItr.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderIntegerItr } ==================================================

function TPostOrderIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderIntegerItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderIntegerItr.GetNextCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderIntegerItr.GetPreviousCursor: TJclIntegerBinaryNode;
var
  LastRet: TJclIntegerBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

type
  TCardinalItr = class(TJclAbstractIterator, IJclCardinalIterator, IJclCardinalTreeIterator, IJclCardinalBinaryTreeIterator)
  protected
    FCursor: TJclCardinalBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclCardinalCollection;
    FEqualityComparer: IJclCardinalEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclCardinalBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclCardinalBinaryNode; virtual; abstract;
    { IJclCardinalIterator }
    function Add(AValue: Cardinal): Boolean;
    function Equals(const AIterator: IJclCardinalIterator): Boolean;
    function GetValue: Cardinal;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AValue: Cardinal): Boolean;
    function Next: Cardinal;
    function NextIndex: Integer;
    function Previous: Cardinal;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(AValue: Cardinal);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Cardinal read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclCardinalTreeIterator }
    function AddChild(AValue: Cardinal): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): Cardinal;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AValue: Cardinal): Integer;
    function InsertChild(Index: Integer; AValue: Cardinal): Boolean;
    function Parent: Cardinal;
    procedure SetChild(Index: Integer; AValue: Cardinal);
    { IJclCardinalBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Cardinal;
    function Right: Cardinal;
  public
    constructor Create(const AOwnTree: IJclCardinalCollection; ACursor: TJclCardinalBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderCardinalItr = class(TCardinalItr, IJclCardinalIterator, IJclCardinalTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderCardinalItr = class(TCardinalItr, IJclCardinalIterator, IJclCardinalTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderCardinalItr = class(TCardinalItr, IJclCardinalIterator, IJclCardinalTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalBinaryNode; override;
    function GetPreviousCursor: TJclCardinalBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TCardinalItr } ===========================================================

constructor TCardinalItr.Create(const AOwnTree: IJclCardinalCollection; ACursor: TJclCardinalBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclCardinalEqualityComparer;
end;

function TCardinalItr.Add(AValue: Cardinal): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TCardinalItr.AddChild(AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TCardinalItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TCardinalItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TCardinalItr then
  begin
    ADest := TCardinalItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TCardinalItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TCardinalItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TCardinalItr.Equals(const AIterator: IJclCardinalIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TCardinalItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TCardinalItr then
  begin
    ItrObj := TCardinalItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TCardinalItr.GetChild(Index: Integer): Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.GetValue: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.IndexOfChild(AValue: Cardinal): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.Insert(AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TCardinalItr.InsertChild(Index: Integer; AValue: Cardinal): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TCardinalItr.Left: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TCardinalItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TCardinalItr.Next: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TCardinalItr.Parent: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.Previous: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TCardinalItr.Remove;
var
  OldCursor: TJclCardinalBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.Reset;
var
  NewCursor: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.Right: Cardinal;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.SetChild(Index: Integer; AValue: Cardinal);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TCardinalItr.SetValue(AValue: Cardinal);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderCardinalItr } ===================================================

function TPreOrderCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderCardinalItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderCardinalItr.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderCardinalItr.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderCardinalItr } ====================================================

function TInOrderCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderCardinalItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderCardinalItr.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderCardinalItr.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderCardinalItr } ==================================================

function TPostOrderCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderCardinalItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderCardinalItr.GetNextCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderCardinalItr.GetPreviousCursor: TJclCardinalBinaryNode;
var
  LastRet: TJclCardinalBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

type
  TInt64Itr = class(TJclAbstractIterator, IJclInt64Iterator, IJclInt64TreeIterator, IJclInt64BinaryTreeIterator)
  protected
    FCursor: TJclInt64BinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclInt64Collection;
    FEqualityComparer: IJclInt64EqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclInt64BinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclInt64BinaryNode; virtual; abstract;
    { IJclInt64Iterator }
    function Add(const AValue: Int64): Boolean;
    function Equals(const AIterator: IJclInt64Iterator): Boolean;
    function GetValue: Int64;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AValue: Int64): Boolean;
    function Next: Int64;
    function NextIndex: Integer;
    function Previous: Int64;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetValue(const AValue: Int64);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Int64 read GetValue;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclInt64TreeIterator }
    function AddChild(const AValue: Int64): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): Int64;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AValue: Int64): Integer;
    function InsertChild(Index: Integer; const AValue: Int64): Boolean;
    function Parent: Int64;
    procedure SetChild(Index: Integer; const AValue: Int64);
    { IJclInt64BinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Int64;
    function Right: Int64;
  public
    constructor Create(const AOwnTree: IJclInt64Collection; ACursor: TJclInt64BinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderInt64Itr = class(TInt64Itr, IJclInt64Iterator, IJclInt64TreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderInt64Itr = class(TInt64Itr, IJclInt64Iterator, IJclInt64TreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderInt64Itr = class(TInt64Itr, IJclInt64Iterator, IJclInt64TreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64BinaryNode; override;
    function GetPreviousCursor: TJclInt64BinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TInt64Itr } ===========================================================

constructor TInt64Itr.Create(const AOwnTree: IJclInt64Collection; ACursor: TJclInt64BinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclInt64EqualityComparer;
end;

function TInt64Itr.Add(const AValue: Int64): Boolean;
begin
  Result := FOwnTree.Add(AValue);
end;

function TInt64Itr.AddChild(const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TInt64Itr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TInt64Itr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TInt64Itr then
  begin
    ADest := TInt64Itr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TInt64Itr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TInt64Itr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TInt64Itr.Equals(const AIterator: IJclInt64Iterator): Boolean;
var
  Obj: TObject;
  ItrObj: TInt64Itr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TInt64Itr then
  begin
    ItrObj := TInt64Itr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TInt64Itr.GetChild(Index: Integer): Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.GetValue: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.IndexOfChild(const AValue: Int64): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AValue) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AValue) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.Insert(const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TInt64Itr.InsertChild(Index: Integer; const AValue: Int64): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TInt64Itr.Left: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TInt64Itr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TInt64Itr.Next: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TInt64Itr.Parent: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.Previous: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := 0;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TInt64Itr.Remove;
var
  OldCursor: TJclInt64BinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.Reset;
var
  NewCursor: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.Right: Int64;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.SetChild(Index: Integer; const AValue: Int64);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TInt64Itr.SetValue(const AValue: Int64);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderInt64Itr } ===================================================

function TPreOrderInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderInt64Itr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderInt64Itr.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderInt64Itr.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderInt64Itr } ====================================================

function TInOrderInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderInt64Itr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderInt64Itr.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderInt64Itr.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderInt64Itr } ==================================================

function TPostOrderInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderInt64Itr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderInt64Itr.GetNextCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderInt64Itr.GetPreviousCursor: TJclInt64BinaryNode;
var
  LastRet: TJclInt64BinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

{$IFNDEF CLR}
type
  TPtrItr = class(TJclAbstractIterator, IJclPtrIterator, IJclPtrTreeIterator, IJclPtrBinaryTreeIterator)
  protected
    FCursor: TJclPtrBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclPtrCollection;
    FEqualityComparer: IJclPtrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclPtrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclPtrBinaryNode; virtual; abstract;
    { IJclPtrIterator }
    function Add(APtr: Pointer): Boolean;
    function Equals(const AIterator: IJclPtrIterator): Boolean;
    function GetPointer: Pointer;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(APtr: Pointer): Boolean;
    function Next: Pointer;
    function NextIndex: Integer;
    function Previous: Pointer;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetPointer(APtr: Pointer);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: Pointer read GetPointer;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclPtrTreeIterator }
    function AddChild(APtr: Pointer): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): Pointer;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(APtr: Pointer): Integer;
    function InsertChild(Index: Integer; APtr: Pointer): Boolean;
    function Parent: Pointer;
    procedure SetChild(Index: Integer; APtr: Pointer);
    { IJclPtrBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: Pointer;
    function Right: Pointer;
  public
    constructor Create(const AOwnTree: IJclPtrCollection; ACursor: TJclPtrBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderPtrItr = class(TPtrItr, IJclPtrIterator, IJclPtrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderPtrItr = class(TPtrItr, IJclPtrIterator, IJclPtrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderPtrItr = class(TPtrItr, IJclPtrIterator, IJclPtrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrBinaryNode; override;
    function GetPreviousCursor: TJclPtrBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TPtrItr } ===========================================================

constructor TPtrItr.Create(const AOwnTree: IJclPtrCollection; ACursor: TJclPtrBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclPtrEqualityComparer;
end;

function TPtrItr.Add(APtr: Pointer): Boolean;
begin
  Result := FOwnTree.Add(APtr);
end;

function TPtrItr.AddChild(APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TPtrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TPtrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TPtrItr then
  begin
    ADest := TPtrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TPtrItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TPtrItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TPtrItr.Equals(const AIterator: IJclPtrIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TPtrItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TPtrItr then
  begin
    ItrObj := TPtrItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TPtrItr.GetChild(Index: Integer): Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.GetPointer: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.IndexOfChild(APtr: Pointer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, APtr) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, APtr) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, APtr) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.Insert(APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TPtrItr.InsertChild(Index: Integer; APtr: Pointer): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TPtrItr.Left: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TPtrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TPtrItr.Next: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TPtrItr.Parent: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.Previous: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TPtrItr.Remove;
var
  OldCursor: TJclPtrBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.Reset;
var
  NewCursor: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.Right: Pointer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.SetChild(Index: Integer; APtr: Pointer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TPtrItr.SetPointer(APtr: Pointer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderPtrItr } ===================================================

function TPreOrderPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderPtrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderPtrItr.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderPtrItr.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderPtrItr } ====================================================

function TInOrderPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderPtrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderPtrItr.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderPtrItr.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderPtrItr } ==================================================

function TPostOrderPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderPtrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderPtrItr.GetNextCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderPtrItr.GetPreviousCursor: TJclPtrBinaryNode;
var
  LastRet: TJclPtrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$ENDIF ~CLR}

type
  TItr = class(TJclAbstractIterator, IJclIterator, IJclTreeIterator, IJclBinaryTreeIterator)
  protected
    FCursor: TJclBinaryNode;
    FStart: TItrStart;
    FOwnTree: IJclCollection;
    FEqualityComparer: IJclEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode; virtual; abstract;
    { IJclIterator }
    function Add(AObject: TObject): Boolean;
    function Equals(const AIterator: IJclIterator): Boolean;
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AObject: TObject): Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetObject(AObject: TObject);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: TObject read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclTreeIterator }
    function AddChild(AObject: TObject): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): TObject;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(AObject: TObject): Integer;
    function InsertChild(Index: Integer; AObject: TObject): Boolean;
    function Parent: TObject;
    procedure SetChild(Index: Integer; AObject: TObject);
    { IJclBinaryTreeIterator }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: TObject;
    function Right: TObject;
  public
    constructor Create(const AOwnTree: IJclCollection; ACursor: TJclBinaryNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderItr = class(TItr, IJclIterator, IJclTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderItr = class(TItr, IJclIterator, IJclTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderItr = class(TItr, IJclIterator, IJclTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TItr } ===========================================================

constructor TItr.Create(const AOwnTree: IJclCollection; ACursor: TJclBinaryNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclEqualityComparer;
end;

function TItr.Add(AObject: TObject): Boolean;
begin
  Result := FOwnTree.Add(AObject);
end;

function TItr.AddChild(AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr then
  begin
    ADest := TItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TItr.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.Equals(const AIterator: IJclIterator): Boolean;
var
  Obj: TObject;
  ItrObj: TItr;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TItr then
  begin
    ItrObj := TItr(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TItr.GetChild(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.GetObject: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.IndexOfChild(AObject: TObject): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AObject) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AObject) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AObject) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.Insert(AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.InsertChild(Index: Integer; AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.Left: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TItr.Next: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.Parent: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.Previous: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr.Remove;
var
  OldCursor: TJclBinaryNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.Reset;
var
  NewCursor: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.Right: TObject;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.SetChild(Index: Integer; AObject: TObject);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr.SetObject(AObject: TObject);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderItr } ===================================================

function TPreOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderItr } ====================================================

function TInOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderItr } ==================================================

function TPostOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

{$IFDEF SUPPORTS_GENERICS}
type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>, IJclTreeIterator<T>, IJclBinaryTreeIterator<T>)
  protected
    FCursor: TJclBinaryNode<T>;
    FStart: TItrStart;
    FOwnTree: IJclCollection<T>;
    FEqualityComparer: IJclEqualityComparer<T>;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclBinaryNode<T>; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode<T>; virtual; abstract;
    { IJclIterator<T> }
    function Add(const AItem: T): Boolean;
    function Equals(const AIterator: IJclIterator<T>): Boolean;
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AItem: T): Boolean;
    function Next: T;
    function NextIndex: Integer;
    function Previous: T;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure Reset;
    procedure SetItem(const AItem: T);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: T read GetItem;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclTreeIterator<T> }
    function AddChild(const AItem: T): Boolean;
    function ChildrenCount: Integer;
    procedure ClearChildren;
    procedure DeleteChild(Index: Integer);
    function GetChild(Index: Integer): T;
    function HasChild(Index: Integer): Boolean;
    function HasParent: Boolean;
    function IndexOfChild(const AItem: T): Integer;
    function InsertChild(Index: Integer; const AItem: T): Boolean;
    function Parent: T;
    procedure SetChild(Index: Integer; const AItem: T);
    { IJclBinaryTreeIterator<T> }
    function HasLeft: Boolean;
    function HasRight: Boolean;
    function Left: T;
    function Right: T;
  public
    constructor Create(const AOwnTree: IJclCollection<T>; ACursor: TJclBinaryNode<T>; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderItr<T> = class(TItr<T>, IJclIterator<T>, IJclTreeIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TInOrderItr<T> = class(TItr<T>, IJclIterator<T>, IJclTreeIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderItr<T> = class(TItr<T>, IJclIterator<T>, IJclTreeIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TItr<T> } ===========================================================

constructor TItr<T>.Create(const AOwnTree: IJclCollection<T>; ACursor: TJclBinaryNode<T>; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FStart := AStart;
  FOwnTree := AOwnTree;
  FEqualityComparer := AOwnTree as IJclEqualityComparer<T>;
end;

function TItr<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnTree.Add(AItem);
end;

function TItr<T>.AddChild(const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr<T> then
  begin
    ADest := TItr<T>(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnTree := FOwnTree;
    ADest.FEqualityComparer := FEqualityComparer;
    ADest.FStart := FStart;
  end;
end;

function TItr<T>.ChildrenCount: Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
        Inc(Result);
      if FCursor.Right <> nil then
        Inc(Result);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.ClearChildren;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr<T>.DeleteChild(Index: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.Equals(const AIterator: IJclIterator<T>): Boolean;
var
  Obj: TObject;
  ItrObj: TItr<T>;
begin
  Result := False;
  if AIterator = nil then
    Exit;
  Obj := AIterator.GetIteratorReference;
  if Obj is TItr<T> then
  begin
    ItrObj := TItr<T>(Obj);
    Result := (FOwnTree = ItrObj.FOwnTree) and (FCursor = ItrObj.FCursor) and (Valid = ItrObj.Valid);
  end;
end;

function TItr<T>.GetChild(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (FCursor <> nil) and (Index = 0) and (FCursor.Left <> nil) then
      FCursor := FCursor.Left
    else
    if (FCursor <> nil) and (Index = 0) then
      FCursor := FCursor.Right
    else
    if (FCursor <> nil) and (Index = 1) then
      FCursor := FCursor.Right
    else
      FCursor := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.GetItem: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasChild(Index: Integer): Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index = 0) then
      Result := (FCursor.Left <> nil) or (FCursor.Right <> nil)
    else
    if (FCursor <> nil) and (Index = 1) then
      Result := (FCursor.Left <> nil) and (FCursor.Right <> nil)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasLeft: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Left <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasParent: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Parent <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasRight: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FCursor.Right <> nil);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.IndexOfChild(const AItem: T): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FCursor <> nil then
    begin
      if FCursor.Left <> nil then
      begin
        if FEqualityComparer.ItemsEqual(FCursor.Left.Value, AItem) then
          Result := 0
        else
        if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AItem) then
          Result := 1;
      end
      else
      if (FCursor.Right <> nil) and FEqualityComparer.ItemsEqual(FCursor.Right.Value, AItem) then
        Result := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.Insert(const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.InsertChild(Index: Integer; const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.Left: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FCursor <> nil then
      FCursor := FCursor.Left;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr<T>.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TItr<T>.Next: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.Parent: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FCursor <> nil then
      FCursor := FCursor.Parent;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.Previous: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr<T>.Remove;
var
  OldCursor: TJclBinaryNode<T>;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnTree.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnTree.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.Reset;
var
  NewCursor: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    case FStart of
      isFirst:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetPreviousCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isLast:
        begin
          NewCursor := FCursor;
          while NewCursor <> nil do
          begin
            NewCursor := GetNextCursor;
            if NewCursor <> nil then
              FCursor := NewCursor;
          end;
        end;
      isRoot:
        begin
          while (FCursor <> nil) and (FCursor.Parent <> nil) do
            FCursor := FCursor.Parent;
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.Right: T;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if FCursor <> nil then
      FCursor := FCursor.Right;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnTree.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.SetChild(Index: Integer; const AItem: T);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderItr<T> } ===================================================

function TPreOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderItr<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderItr<T> } ====================================================

function TInOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderItr<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TInOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderItr<T> } ==================================================

function TPostOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderItr<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$ENDIF SUPPORTS_GENERICS}

//=== { TJclIntfBinaryTree } =================================================

constructor TJclIntfBinaryTree.Create(ACompare: TIntfCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclIntfBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfBinaryTree.Add(const AInterface: IInterface): Boolean;
var
  NewNode, Current, Save: TJclIntfBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AInterface, nil) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AInterface, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclIntfBinaryNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclIntfBinaryNode): TJclIntfBinaryNode;
  begin
    Result := TJclIntfBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclIntfBinaryTree;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfBinaryTree then
  begin
    ADest := TJclIntfBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntfBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfBinaryTree then
    TJclIntfBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntfBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclIntfBinaryTree.Clear;
var
  Current, Parent: TJclIntfBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeObject(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.Contains(const AInterface: IInterface): Boolean;
var
  Comp: Integer;
  Current: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AInterface);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclIntfBinaryTree.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.First: IJclIntfIterator;
var
  Start: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderIntfItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderIntfItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderIntfItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfBinaryTree.GetEnumerator: IJclIntfIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfBinaryTree.GetRoot: IJclIntfTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderIntfItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderIntfItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderIntfItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntfBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfBinaryTree.Last: IJclIntfIterator;
var
  Start: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderIntfItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderIntfItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderIntfItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.Pack;
type
  TLeafArray = array of TJclIntfBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclIntfBinaryNode;
    Offset: Integer): TJclIntfBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclIntfBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.Remove(const AInterface: IInterface): Boolean;
var
  Current, Successor: TJclIntfBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AInterface in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AInterface, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeObject(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntfBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclAnsiStrBinaryTree } =================================================

constructor TJclAnsiStrBinaryTree.Create(ACompare: TAnsiStrCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclAnsiStrBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrBinaryTree.Add(const AString: AnsiString): Boolean;
var
  NewNode, Current, Save: TJclAnsiStrBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AString, '') then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AString, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclAnsiStrBinaryNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclAnsiStrBinaryNode): TJclAnsiStrBinaryNode;
  begin
    Result := TJclAnsiStrBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclAnsiStrBinaryTree;
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrBinaryTree then
  begin
    ADest := TJclAnsiStrBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrBinaryTree then
    TJclAnsiStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclAnsiStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclAnsiStrBinaryTree.Clear;
var
  Current, Parent: TJclAnsiStrBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeString(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.Contains(const AString: AnsiString): Boolean;
var
  Comp: Integer;
  Current: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AString);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrBinaryTree.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItSelf: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.First: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderAnsiStrItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderAnsiStrItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderAnsiStrItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrBinaryTree.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrBinaryTree.GetRoot: IJclAnsiStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderAnsiStrItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderAnsiStrItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderAnsiStrItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclAnsiStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrBinaryTree.Last: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderAnsiStrItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderAnsiStrItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderAnsiStrItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTree.Pack;
type
  TLeafArray = array of TJclAnsiStrBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclAnsiStrBinaryNode;
    Offset: Integer): TJclAnsiStrBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclAnsiStrBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.Remove(const AString: AnsiString): Boolean;
var
  Current, Successor: TJclAnsiStrBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AString in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AString, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeString(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclAnsiStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclWideStrBinaryTree } =================================================

constructor TJclWideStrBinaryTree.Create(ACompare: TWideStrCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclWideStrBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrBinaryTree.Add(const AString: WideString): Boolean;
var
  NewNode, Current, Save: TJclWideStrBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AString, '') then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AString, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclWideStrBinaryNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclWideStrBinaryNode): TJclWideStrBinaryNode;
  begin
    Result := TJclWideStrBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclWideStrBinaryTree;
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrBinaryTree then
  begin
    ADest := TJclWideStrBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrBinaryTree then
    TJclWideStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclWideStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclWideStrBinaryTree.Clear;
var
  Current, Parent: TJclWideStrBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeString(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.Contains(const AString: WideString): Boolean;
var
  Comp: Integer;
  Current: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AString);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclWideStrBinaryTree.Equals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItSelf: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.First: IJclWideStrIterator;
var
  Start: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderWideStrItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderWideStrItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderWideStrItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrBinaryTree.GetEnumerator: IJclWideStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrBinaryTree.GetRoot: IJclWideStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderWideStrItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderWideStrItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderWideStrItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclWideStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrBinaryTree.Last: IJclWideStrIterator;
var
  Start: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderWideStrItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderWideStrItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderWideStrItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTree.Pack;
type
  TLeafArray = array of TJclWideStrBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclWideStrBinaryNode;
    Offset: Integer): TJclWideStrBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclWideStrBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.Remove(const AString: WideString): Boolean;
var
  Current, Successor: TJclWideStrBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AString in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AString, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeString(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclWideStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclSingleBinaryTree } =================================================

constructor TJclSingleBinaryTree.Create(ACompare: TSingleCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclSingleBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleBinaryTree.Add(const AValue: Single): Boolean;
var
  NewNode, Current, Save: TJclSingleBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclSingleBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.AddAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclSingleBinaryNode): TJclSingleBinaryNode;
  begin
    Result := TJclSingleBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclSingleBinaryTree;
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleBinaryTree then
  begin
    ADest := TJclSingleBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclSingleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclSingleBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleBinaryTree then
    TJclSingleBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclSingleBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclSingleBinaryTree.Clear;
var
  Current, Parent: TJclSingleBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeSingle(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.Contains(const AValue: Single): Boolean;
var
  Comp: Integer;
  Current: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclSingleBinaryTree.Equals(const ACollection: IJclSingleCollection): Boolean;
var
  It, ItSelf: IJclSingleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.First: IJclSingleIterator;
var
  Start: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderSingleItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderSingleItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderSingleItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclSingleBinaryTree.GetEnumerator: IJclSingleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleBinaryTree.GetRoot: IJclSingleTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderSingleItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderSingleItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderSingleItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclSingleBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleBinaryTree.Last: IJclSingleIterator;
var
  Start: TJclSingleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderSingleItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderSingleItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderSingleItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTree.Pack;
type
  TLeafArray = array of TJclSingleBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclSingleBinaryNode;
    Offset: Integer): TJclSingleBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclSingleBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.Remove(const AValue: Single): Boolean;
var
  Current, Successor: TJclSingleBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeSingle(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleBinaryTree.RetainAll(const ACollection: IJclSingleCollection): Boolean;
var
  It: IJclSingleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclSingleBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclDoubleBinaryTree } =================================================

constructor TJclDoubleBinaryTree.Create(ACompare: TDoubleCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclDoubleBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleBinaryTree.Add(const AValue: Double): Boolean;
var
  NewNode, Current, Save: TJclDoubleBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclDoubleBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.AddAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclDoubleBinaryNode): TJclDoubleBinaryNode;
  begin
    Result := TJclDoubleBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclDoubleBinaryTree;
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleBinaryTree then
  begin
    ADest := TJclDoubleBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclDoubleCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclDoubleBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleBinaryTree then
    TJclDoubleBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclDoubleBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclDoubleBinaryTree.Clear;
var
  Current, Parent: TJclDoubleBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeDouble(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.Contains(const AValue: Double): Boolean;
var
  Comp: Integer;
  Current: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclDoubleBinaryTree.Equals(const ACollection: IJclDoubleCollection): Boolean;
var
  It, ItSelf: IJclDoubleIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.First: IJclDoubleIterator;
var
  Start: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderDoubleItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderDoubleItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderDoubleItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclDoubleBinaryTree.GetEnumerator: IJclDoubleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleBinaryTree.GetRoot: IJclDoubleTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderDoubleItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderDoubleItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderDoubleItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclDoubleBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleBinaryTree.Last: IJclDoubleIterator;
var
  Start: TJclDoubleBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderDoubleItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderDoubleItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderDoubleItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTree.Pack;
type
  TLeafArray = array of TJclDoubleBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclDoubleBinaryNode;
    Offset: Integer): TJclDoubleBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclDoubleBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.Remove(const AValue: Double): Boolean;
var
  Current, Successor: TJclDoubleBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeDouble(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleBinaryTree.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
var
  It: IJclDoubleIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclDoubleBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclExtendedBinaryTree } =================================================

constructor TJclExtendedBinaryTree.Create(ACompare: TExtendedCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclExtendedBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedBinaryTree.Add(const AValue: Extended): Boolean;
var
  NewNode, Current, Save: TJclExtendedBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0.0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclExtendedBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.AddAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclExtendedBinaryNode): TJclExtendedBinaryNode;
  begin
    Result := TJclExtendedBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclExtendedBinaryTree;
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedBinaryTree then
  begin
    ADest := TJclExtendedBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclExtendedCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclExtendedBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedBinaryTree then
    TJclExtendedBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclExtendedBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclExtendedBinaryTree.Clear;
var
  Current, Parent: TJclExtendedBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeExtended(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.Contains(const AValue: Extended): Boolean;
var
  Comp: Integer;
  Current: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclExtendedBinaryTree.Equals(const ACollection: IJclExtendedCollection): Boolean;
var
  It, ItSelf: IJclExtendedIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.First: IJclExtendedIterator;
var
  Start: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderExtendedItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderExtendedItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderExtendedItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclExtendedBinaryTree.GetEnumerator: IJclExtendedIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedBinaryTree.GetRoot: IJclExtendedTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderExtendedItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderExtendedItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderExtendedItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclExtendedBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedBinaryTree.Last: IJclExtendedIterator;
var
  Start: TJclExtendedBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderExtendedItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderExtendedItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderExtendedItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTree.Pack;
type
  TLeafArray = array of TJclExtendedBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclExtendedBinaryNode;
    Offset: Integer): TJclExtendedBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclExtendedBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.Remove(const AValue: Extended): Boolean;
var
  Current, Successor: TJclExtendedBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeExtended(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedBinaryTree.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
var
  It: IJclExtendedIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclExtendedBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclIntegerBinaryTree } =================================================

constructor TJclIntegerBinaryTree.Create(ACompare: TIntegerCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclIntegerBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerBinaryTree.Add(AValue: Integer): Boolean;
var
  NewNode, Current, Save: TJclIntegerBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclIntegerBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.AddAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclIntegerBinaryNode): TJclIntegerBinaryNode;
  begin
    Result := TJclIntegerBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclIntegerBinaryTree;
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerBinaryTree then
  begin
    ADest := TJclIntegerBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclIntegerCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntegerBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerBinaryTree then
    TJclIntegerBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntegerBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclIntegerBinaryTree.Clear;
var
  Current, Parent: TJclIntegerBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeInteger(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.Contains(AValue: Integer): Boolean;
var
  Comp: Integer;
  Current: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclIntegerBinaryTree.Equals(const ACollection: IJclIntegerCollection): Boolean;
var
  It, ItSelf: IJclIntegerIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.First: IJclIntegerIterator;
var
  Start: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderIntegerItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderIntegerItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderIntegerItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntegerBinaryTree.GetEnumerator: IJclIntegerIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerBinaryTree.GetRoot: IJclIntegerTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderIntegerItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderIntegerItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderIntegerItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntegerBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerBinaryTree.Last: IJclIntegerIterator;
var
  Start: TJclIntegerBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderIntegerItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderIntegerItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderIntegerItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTree.Pack;
type
  TLeafArray = array of TJclIntegerBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclIntegerBinaryNode;
    Offset: Integer): TJclIntegerBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclIntegerBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.Remove(AValue: Integer): Boolean;
var
  Current, Successor: TJclIntegerBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeInteger(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerBinaryTree.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
var
  It: IJclIntegerIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntegerBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclCardinalBinaryTree } =================================================

constructor TJclCardinalBinaryTree.Create(ACompare: TCardinalCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclCardinalBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalBinaryTree.Add(AValue: Cardinal): Boolean;
var
  NewNode, Current, Save: TJclCardinalBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclCardinalBinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.AddAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclCardinalBinaryNode): TJclCardinalBinaryNode;
  begin
    Result := TJclCardinalBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclCardinalBinaryTree;
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalBinaryTree then
  begin
    ADest := TJclCardinalBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCardinalCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclCardinalBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalBinaryTree then
    TJclCardinalBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclCardinalBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclCardinalBinaryTree.Clear;
var
  Current, Parent: TJclCardinalBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeCardinal(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.Contains(AValue: Cardinal): Boolean;
var
  Comp: Integer;
  Current: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclCardinalBinaryTree.Equals(const ACollection: IJclCardinalCollection): Boolean;
var
  It, ItSelf: IJclCardinalIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.First: IJclCardinalIterator;
var
  Start: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderCardinalItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderCardinalItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderCardinalItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclCardinalBinaryTree.GetEnumerator: IJclCardinalIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalBinaryTree.GetRoot: IJclCardinalTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderCardinalItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderCardinalItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderCardinalItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclCardinalBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalBinaryTree.Last: IJclCardinalIterator;
var
  Start: TJclCardinalBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderCardinalItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderCardinalItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderCardinalItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTree.Pack;
type
  TLeafArray = array of TJclCardinalBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclCardinalBinaryNode;
    Offset: Integer): TJclCardinalBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclCardinalBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.Remove(AValue: Cardinal): Boolean;
var
  Current, Successor: TJclCardinalBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeCardinal(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalBinaryTree.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
var
  It: IJclCardinalIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclCardinalBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclInt64BinaryTree } =================================================

constructor TJclInt64BinaryTree.Create(ACompare: TInt64Compare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclInt64BinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64BinaryTree.Add(const AValue: Int64): Boolean;
var
  NewNode, Current, Save: TJclInt64BinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AValue, 0) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclInt64BinaryNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.AddAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclInt64BinaryNode): TJclInt64BinaryNode;
  begin
    Result := TJclInt64BinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclInt64BinaryTree;
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64BinaryTree then
  begin
    ADest := TJclInt64BinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclInt64Collection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclInt64BinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64BinaryTree then
    TJclInt64BinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclInt64BinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclInt64BinaryTree.Clear;
var
  Current, Parent: TJclInt64BinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeInt64(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.Contains(const AValue: Int64): Boolean;
var
  Comp: Integer;
  Current: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AValue);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64BinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclInt64BinaryTree.Equals(const ACollection: IJclInt64Collection): Boolean;
var
  It, ItSelf: IJclInt64Iterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.First: IJclInt64Iterator;
var
  Start: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderInt64Itr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderInt64Itr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderInt64Itr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclInt64BinaryTree.GetEnumerator: IJclInt64Iterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64BinaryTree.GetRoot: IJclInt64TreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderInt64Itr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderInt64Itr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderInt64Itr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclInt64BinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64BinaryTree.Last: IJclInt64Iterator;
var
  Start: TJclInt64BinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderInt64Itr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderInt64Itr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderInt64Itr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTree.Pack;
type
  TLeafArray = array of TJclInt64BinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclInt64BinaryNode;
    Offset: Integer): TJclInt64BinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclInt64BinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.Remove(const AValue: Int64): Boolean;
var
  Current, Successor: TJclInt64BinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AValue in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AValue, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeInt64(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64BinaryTree.RetainAll(const ACollection: IJclInt64Collection): Boolean;
var
  It: IJclInt64Iterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64BinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64BinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclInt64BinaryTree.Size: Integer;
begin
  Result := FSize;
end;

{$IFNDEF CLR}
//=== { TJclPtrBinaryTree } =================================================

constructor TJclPtrBinaryTree.Create(ACompare: TPtrCompare);
begin
  inherited Create();
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclPtrBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrBinaryTree.Add(APtr: Pointer): Boolean;
var
  NewNode, Current, Save: TJclPtrBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(APtr, nil) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(APtr, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclPtrBinaryNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.AddAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclPtrBinaryNode): TJclPtrBinaryNode;
  begin
    Result := TJclPtrBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclPtrBinaryTree;
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrBinaryTree then
  begin
    ADest := TJclPtrBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclPtrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclPtrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrBinaryTree then
    TJclPtrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclPtrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclPtrBinaryTree.Clear;
var
  Current, Parent: TJclPtrBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreePointer(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.Contains(APtr: Pointer): Boolean;
var
  Comp: Integer;
  Current: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, APtr);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

function TJclPtrBinaryTree.Equals(const ACollection: IJclPtrCollection): Boolean;
var
  It, ItSelf: IJclPtrIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.First: IJclPtrIterator;
var
  Start: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderPtrItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderPtrItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderPtrItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclPtrBinaryTree.GetEnumerator: IJclPtrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrBinaryTree.GetRoot: IJclPtrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderPtrItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderPtrItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderPtrItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclPtrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrBinaryTree.Last: IJclPtrIterator;
var
  Start: TJclPtrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderPtrItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderPtrItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderPtrItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTree.Pack;
type
  TLeafArray = array of TJclPtrBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclPtrBinaryNode;
    Offset: Integer): TJclPtrBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclPtrBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.Remove(APtr: Pointer): Boolean;
var
  Current, Successor: TJclPtrBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate APtr in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(APtr, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreePointer(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrBinaryTree.RetainAll(const ACollection: IJclPtrCollection): Boolean;
var
  It: IJclPtrIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclPtrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;
{$ENDIF ~CLR}

//=== { TJclBinaryTree } =================================================

constructor TJclBinaryTree.Create(ACompare: TCompare; AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  SetCompare(ACompare);
end;

destructor TJclBinaryTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree.Add(AObject: TObject): Boolean;
var
  NewNode, Current, Save: TJclBinaryNode;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AObject, nil) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AObject, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclBinaryNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclBinaryNode): TJclBinaryNode;
  begin
    Result := TJclBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclBinaryTree;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclBinaryTree then
  begin
    ADest := TJclBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclBinaryTree then
    TJclBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclBinaryTree.Clear;
var
  Current, Parent: TJclBinaryNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeObject(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.Contains(AObject: TObject): Boolean;
var
  Comp: Integer;
  Current: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AObject);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTree.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTree.Equals(const ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.First: IJclIterator;
var
  Start: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderItr.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderItr.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclBinaryTree.GetEnumerator: IJclIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclBinaryTree.GetRoot: IJclTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderItr.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderItr.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclBinaryTree.Last: IJclIterator;
var
  Start: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderItr.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderItr.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderItr.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.Pack;
type
  TLeafArray = array of TJclBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclBinaryNode;
    Offset: Integer): TJclBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclBinaryNode;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.Remove(AObject: TObject): Boolean;
var
  Current, Successor: TJclBinaryNode;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AObject in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AObject, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeObject(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.RetainAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclBinaryTree<T> } =================================================

constructor TJclBinaryTree<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
end;

destructor TJclBinaryTree<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree<T>.Add(const AItem: T): Boolean;
var
  NewNode, Current, Save: TJclBinaryNode<T>;
  Comp, Depth: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AItem, Default(T)) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AItem, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
        if CheckDuplicate then
          Current := Current.Left // arbitrary decision
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclBinaryNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) <= 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclBinaryNode<T>): TJclBinaryNode<T>;
  begin
    Result := TJclBinaryNode<T>.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclBinaryTree<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclBinaryTree<T> then
  begin
    ADest := TJclBinaryTree<T>(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclBinaryTree<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclBinaryTree<T> then
    TJclBinaryTree<T>(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclBinaryTree<T>.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclBinaryTree<T>.Clear;
var
  Current, Parent: TJclBinaryNode<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeItem(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.Contains(const AItem: T): Boolean;
var
  Comp: Integer;
  Current: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AItem);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclBinaryTree<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  It, ItSelf: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.First: IJclIterator<T>;
var
  Start: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr<T>.Create(Self, Start, False, isFirst);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderItr<T>.Create(Self, Start, False, isFirst);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderItr<T>.Create(Self, Start, False, isFirst);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclBinaryTree<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclBinaryTree<T>.GetRoot: IJclTreeIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr<T>.Create(Self, FRoot, False, isRoot);
      toOrder:
        Result := TInOrderItr<T>.Create(Self, FRoot, False, isRoot);
      toPostOrder:
        Result := TPostOrderItr<T>.Create(Self, FRoot, False, isRoot);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclBinaryTree<T>.Last: IJclIterator<T>;
var
  Start: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderItr<T>.Create(Self, Start, False, isLast);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderItr<T>.Create(Self, Start, False, isLast);
        end;
      toPostOrder:
        Result := TPostOrderItr<T>.Create(Self, Start, False, isLast);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree<T>.Pack;
type
  TLeafArray = array of TJclBinaryNode<T>;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclBinaryNode<T>;
    Offset: Integer): TJclBinaryNode<T>;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclBinaryNode<T>;
  Index: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    SetLength(Leafarray, FSize);
    try
      // in order enumeration of nodes
      ANode := FRoot;
      if ANode <> nil then
      begin
        // find first node
        while ANode.Left <> nil do
          ANode := ANode.Left;

        Index := 0;
        while ANode <> nil do
        begin
          LeafArray[Index] := ANode;
          Inc(Index);
          if ANode.Right <> nil then
          begin
            ANode := ANode.Right;
            while (ANode.Left <> nil) do
              ANode := ANode.Left;
          end
          else
          begin
            BNode := ANode;
            ANode := ANode.Parent;
            while (ANode <> nil) and (ANode.Right = BNode) do
            begin
              BNode := ANode;
              ANode := ANode.Parent;
            end;
          end;
        end;

        Index := FSize shr 1;
        FRoot := LeafArray[Index];
        FRoot.Parent := nil;
        if Index > 0 then
          FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
        else
          FRoot.Left := nil;
        if Index < (FSize - 1) then
          FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
        else
          FRoot.Right := nil;
      end;
    finally
      SetLength(LeafArray, 0);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.Remove(const AItem: T): Boolean;
var
  Current, Successor: TJclBinaryNode<T>;
  Comp: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AItem in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AItem, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          Successor.Parent.Left := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor;
        end;

        // insert successor in new position
        Successor.Left := Current.Left;
        if Current.Left <> nil then
          Current.Left.Parent := Successor;
        Successor.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeItem(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree<T>.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTree<T>.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree<T>.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclBinaryTreeE<T> } =================================================

constructor TJclBinaryTreeE<T>.Create(const AComparer: IComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclBinaryTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeE<T> then
    TJclBinaryTreeE<T>(Dest).FComparer := FComparer;
end;

function TJclBinaryTreeE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeE<T>.Create(Comparer, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclBinaryTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclBinaryTreeF<T> } =================================================

constructor TJclBinaryTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCompare(ACompare);
end;

function TJclBinaryTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclBinaryTreeI<T> } =================================================

function TJclBinaryTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeI<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := A.CompareTo(B);
end;

function TJclBinaryTreeI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.CompareTo(B) = 0;
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

