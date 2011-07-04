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
{ The Original Code is JclTrees.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by                }
{ Florent Ouchet are Copyright (C) Florent Ouchet <outchy att users dott sourceforge dott net      }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008)                       $ }
{ Revision:      $Rev:: 2393                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclTrees;

interface

{$I jcl.inc}

uses
  Classes,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;


type
  TJclIntfTreeNode = class
  public
    Value: IInterface;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclIntfTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclIntfTreeNode;
    function IndexOfChild(AChild: TJclIntfTreeNode): Integer;
    function IndexOfValue(const AInterface: IInterface; const AEqualityComparer: IJclIntfEqualityComparer): Integer;
  end;

  TJclIntfTree = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclIntfEqualityComparer,
    IJclIntfCollection, IJclIntfTree)
  private
    FRoot: TJclIntfTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclIntfTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclIntfTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclAnsiStrTreeNode = class
  public
    Value: AnsiString;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclAnsiStrTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclAnsiStrTreeNode;
    function IndexOfChild(AChild: TJclAnsiStrTreeNode): Integer;
    function IndexOfValue(const AString: AnsiString; const AEqualityComparer: IJclAnsiStrEqualityComparer): Integer;
  end;

  TJclAnsiStrTree = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclAnsiStrEqualityComparer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrCollection, IJclAnsiStrTree)
  private
    FRoot: TJclAnsiStrTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclAnsiStrTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclAnsiStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclWideStrTreeNode = class
  public
    Value: WideString;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclWideStrTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclWideStrTreeNode;
    function IndexOfChild(AChild: TJclWideStrTreeNode): Integer;
    function IndexOfValue(const AString: WideString; const AEqualityComparer: IJclWideStrEqualityComparer): Integer;
  end;

  TJclWideStrTree = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclWideStrEqualityComparer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrCollection, IJclWideStrTree)
  private
    FRoot: TJclWideStrTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclWideStrTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclWideStrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrTree = TJclAnsiStrTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrTree = TJclWideStrTree;
  {$ENDIF CONTAINER_WIDESTR}

  TJclSingleTreeNode = class
  public
    Value: Single;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclSingleTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclSingleTreeNode;
    function IndexOfChild(AChild: TJclSingleTreeNode): Integer;
    function IndexOfValue(const AValue: Single; const AEqualityComparer: IJclSingleEqualityComparer): Integer;
  end;

  TJclSingleTree = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclSingleEqualityComparer, IJclSingleContainer,
    IJclSingleCollection, IJclSingleTree)
  private
    FRoot: TJclSingleTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclSingleTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclSingleTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclDoubleTreeNode = class
  public
    Value: Double;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclDoubleTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclDoubleTreeNode;
    function IndexOfChild(AChild: TJclDoubleTreeNode): Integer;
    function IndexOfValue(const AValue: Double; const AEqualityComparer: IJclDoubleEqualityComparer): Integer;
  end;

  TJclDoubleTree = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclDoubleEqualityComparer, IJclDoubleContainer,
    IJclDoubleCollection, IJclDoubleTree)
  private
    FRoot: TJclDoubleTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclDoubleTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclDoubleTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclExtendedTreeNode = class
  public
    Value: Extended;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclExtendedTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclExtendedTreeNode;
    function IndexOfChild(AChild: TJclExtendedTreeNode): Integer;
    function IndexOfValue(const AValue: Extended; const AEqualityComparer: IJclExtendedEqualityComparer): Integer;
  end;

  TJclExtendedTree = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclExtendedEqualityComparer, IJclExtendedContainer,
    IJclExtendedCollection, IJclExtendedTree)
  private
    FRoot: TJclExtendedTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclExtendedTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclExtendedTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatTree = TJclExtendedTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatTree = TJclDoubleTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatTree = TJclSingleTree;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerTreeNode = class
  public
    Value: Integer;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclIntegerTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclIntegerTreeNode;
    function IndexOfChild(AChild: TJclIntegerTreeNode): Integer;
    function IndexOfValue(AValue: Integer; const AEqualityComparer: IJclIntegerEqualityComparer): Integer;
  end;

  TJclIntegerTree = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclIntegerEqualityComparer,
    IJclIntegerCollection, IJclIntegerTree)
  private
    FRoot: TJclIntegerTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclIntegerTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclIntegerTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclCardinalTreeNode = class
  public
    Value: Cardinal;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclCardinalTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclCardinalTreeNode;
    function IndexOfChild(AChild: TJclCardinalTreeNode): Integer;
    function IndexOfValue(AValue: Cardinal; const AEqualityComparer: IJclCardinalEqualityComparer): Integer;
  end;

  TJclCardinalTree = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclCardinalEqualityComparer,
    IJclCardinalCollection, IJclCardinalTree)
  private
    FRoot: TJclCardinalTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclCardinalTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclCardinalTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

  TJclInt64TreeNode = class
  public
    Value: Int64;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclInt64TreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclInt64TreeNode;
    function IndexOfChild(AChild: TJclInt64TreeNode): Integer;
    function IndexOfValue(const AValue: Int64; const AEqualityComparer: IJclInt64EqualityComparer): Integer;
  end;

  TJclInt64Tree = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclInt64EqualityComparer,
    IJclInt64Collection, IJclInt64Tree)
  private
    FRoot: TJclInt64TreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclInt64TreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclInt64TreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

{$IFNDEF CLR}
  TJclPtrTreeNode = class
  public
    Value: Pointer;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclPtrTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclPtrTreeNode;
    function IndexOfChild(AChild: TJclPtrTreeNode): Integer;
    function IndexOfValue(APtr: Pointer; const AEqualityComparer: IJclPtrEqualityComparer): Integer;
  end;

  TJclPtrTree = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclPtrEqualityComparer,
    IJclPtrCollection, IJclPtrTree)
  private
    FRoot: TJclPtrTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclPtrTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create();
    destructor Destroy; override;
    property Root: IJclPtrTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;
{$ENDIF ~CLR}

  TJclTreeNode = class
  public
    Value: TObject;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclTreeNode;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclTreeNode;
    function IndexOfChild(AChild: TJclTreeNode): Integer;
    function IndexOfValue(AObject: TObject; const AEqualityComparer: IJclEqualityComparer): Integer;
  end;

  TJclTree = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclEqualityComparer, IJclObjectOwner,
    IJclCollection, IJclTree)
  private
    FRoot: TJclTreeNode;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclTreeNode);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    constructor Create(AOwnsObjects: Boolean);
    destructor Destroy; override;
    property Root: IJclTreeIterator read GetRoot;
    property TraverseOrder: TJclTraverseOrder read GetTraverseOrder write SetTraverseOrder;
  end;

{$IFDEF SUPPORTS_GENERICS}
  TJclTreeNode<T> = class
  public
    Value: T;
    {$IFDEF BCB}
    Children: TDynObjectArray;
    {$ELSE ~BCB}
    Children: array of TJclTreeNode<T>;
    {$ENDIF ~BCB}
    ChildrenCount: Integer;
    Parent: TJclTreeNode<T>;
    function IndexOfChild(AChild: TJclTreeNode<T>): Integer;
    function IndexOfValue(const AItem: T; const AEqualityComparer: IJclEqualityComparer<T>): Integer;
  end;

  TJclTree<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclEqualityComparer<T>, IJclItemOwner<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FRoot: TJclTreeNode<T>;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure ClearNode(var ANode: TJclTreeNode<T>);
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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

  // E = External helper to compare items for equality
  TJclTreeE<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; AOwnsItems: Boolean);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclTreeF<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
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

  // I = Items can compare themselves to an other for equality
  TJclTreeI<T: IEquatable<T>> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclTrees.pas $';
    Revision: '$Revision: 2393 $';
    Date: '$Date: 2008-07-20 11:13:23 +0200 (dim., 20 juil. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

type
  TItrStart = (isFirst, isLast, isRoot);

type
  TIntfItr = class(TJclAbstractIterator, IJclIntfIterator, IJclIntfTreeIterator)
  protected
    FCursor: TJclIntfTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclIntfTree;
    FEqualityComparer: IJclIntfEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntfTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclIntfTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclIntfTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclIntfTree; ACursor: TJclIntfTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderIntfItr = class(TIntfItr, IJclIntfIterator, IJclIntfTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfTreeNode; override;
    function GetNextSibling: TJclIntfTreeNode; override;
    function GetPreviousCursor: TJclIntfTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderIntfItr = class(TIntfItr, IJclIntfIterator, IJclIntfTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfTreeNode; override;
    function GetNextSibling: TJclIntfTreeNode; override;
    function GetPreviousCursor: TJclIntfTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TIntfItr } ===========================================================

constructor TIntfItr.Create(OwnTree: TJclIntfTree; ACursor: TJclIntfTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclIntfEqualityComparer;
end;

function TIntfItr.Add(const AInterface: IInterface): Boolean;
var
  ParentNode, NewNode: TJclIntfTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclIntfTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil))
      and ((not FOwnTree.Contains(AInterface)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclIntfTreeNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.AddChild(const AInterface: IInterface): Boolean;
var
  NewNode: TJclIntfTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil))
      and ((not FOwnTree.Contains(AInterface)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclIntfTreeNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclIntfTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclIntfTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclIntfTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TIntfItr.IndexOfChild(const AInterface: IInterface): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AInterface, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.Insert(const AInterface: IInterface): Boolean;
var
  ParentNode, NewNode: TJclIntfTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclIntfTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil))
      and ((not FOwnTree.Contains(AInterface)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclIntfTreeNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.InsertChild(Index: Integer; const AInterface: IInterface): Boolean;
var
  NewNode: TJclIntfTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclIntfTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil))
      and ((not FOwnTree.Contains(AInterface)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclIntfTreeNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclIntfTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.Reset;
var
  NewCursor: TJclIntfTreeNode;
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

procedure TIntfItr.SetChild(Index: Integer; const AInterface: IInterface);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclIntfTreeNode(FCursor.Children[Index]).Value := AInterface
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeObject(FCursor.Value);
      FCursor.Value := AInterface;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderIntfItr } ===================================================

function TPreOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderIntfItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderIntfItr.GetNextCursor: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclIntfTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderIntfItr.GetNextSibling: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderIntfItr.GetPreviousCursor: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclIntfTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderIntfItr } ==================================================

function TPostOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderIntfItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderIntfItr.GetNextCursor: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclIntfTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderIntfItr.GetNextSibling: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclIntfTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderIntfItr.GetPreviousCursor: TJclIntfTreeNode;
var
  LastRet: TJclIntfTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclIntfTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclIntfTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

type
  TAnsiStrItr = class(TJclAbstractIterator, IJclAnsiStrIterator, IJclAnsiStrTreeIterator)
  protected
    FCursor: TJclAnsiStrTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclAnsiStrTree;
    FEqualityComparer: IJclAnsiStrEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclAnsiStrTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclAnsiStrTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclAnsiStrTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclAnsiStrTree; ACursor: TJclAnsiStrTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrTreeNode; override;
    function GetNextSibling: TJclAnsiStrTreeNode; override;
    function GetPreviousCursor: TJclAnsiStrTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, IJclAnsiStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrTreeNode; override;
    function GetNextSibling: TJclAnsiStrTreeNode; override;
    function GetPreviousCursor: TJclAnsiStrTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TAnsiStrItr } ===========================================================

constructor TAnsiStrItr.Create(OwnTree: TJclAnsiStrTree; ACursor: TJclAnsiStrTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclAnsiStrEqualityComparer;
end;

function TAnsiStrItr.Add(const AString: AnsiString): Boolean;
var
  ParentNode, NewNode: TJclAnsiStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclAnsiStrTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclAnsiStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.AddChild(const AString: AnsiString): Boolean;
var
  NewNode: TJclAnsiStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclAnsiStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclAnsiStrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclAnsiStrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclAnsiStrTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TAnsiStrItr.IndexOfChild(const AString: AnsiString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AString, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.Insert(const AString: AnsiString): Boolean;
var
  ParentNode, NewNode: TJclAnsiStrTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclAnsiStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclAnsiStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.InsertChild(Index: Integer; const AString: AnsiString): Boolean;
var
  NewNode: TJclAnsiStrTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclAnsiStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclAnsiStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclAnsiStrTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.Reset;
var
  NewCursor: TJclAnsiStrTreeNode;
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

procedure TAnsiStrItr.SetChild(Index: Integer; const AString: AnsiString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclAnsiStrTreeNode(FCursor.Children[Index]).Value := AString
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.SetString(const AString: AnsiString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeString(FCursor.Value);
      FCursor.Value := AString;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderAnsiStrItr } ===================================================

function TPreOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderAnsiStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderAnsiStrItr.GetNextCursor: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclAnsiStrTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderAnsiStrItr.GetNextSibling: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclAnsiStrTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderAnsiStrItr } ==================================================

function TPostOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderAnsiStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderAnsiStrItr.GetNextCursor: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclAnsiStrTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderAnsiStrItr.GetNextSibling: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclAnsiStrTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrTreeNode;
var
  LastRet: TJclAnsiStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclAnsiStrTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclAnsiStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

type
  TWideStrItr = class(TJclAbstractIterator, IJclWideStrIterator, IJclWideStrTreeIterator)
  protected
    FCursor: TJclWideStrTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclWideStrTree;
    FEqualityComparer: IJclWideStrEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclWideStrTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclWideStrTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclWideStrTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclWideStrTree; ACursor: TJclWideStrTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, IJclWideStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrTreeNode; override;
    function GetNextSibling: TJclWideStrTreeNode; override;
    function GetPreviousCursor: TJclWideStrTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, IJclWideStrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrTreeNode; override;
    function GetNextSibling: TJclWideStrTreeNode; override;
    function GetPreviousCursor: TJclWideStrTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TWideStrItr } ===========================================================

constructor TWideStrItr.Create(OwnTree: TJclWideStrTree; ACursor: TJclWideStrTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclWideStrEqualityComparer;
end;

function TWideStrItr.Add(const AString: WideString): Boolean;
var
  ParentNode, NewNode: TJclWideStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclWideStrTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclWideStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.AddChild(const AString: WideString): Boolean;
var
  NewNode: TJclWideStrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclWideStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclWideStrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclWideStrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclWideStrTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TWideStrItr.IndexOfChild(const AString: WideString): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AString, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.Insert(const AString: WideString): Boolean;
var
  ParentNode, NewNode: TJclWideStrTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclWideStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclWideStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.InsertChild(Index: Integer; const AString: WideString): Boolean;
var
  NewNode: TJclWideStrTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclWideStrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, ''))
      and ((not FOwnTree.Contains(AString)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclWideStrTreeNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclWideStrTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.Reset;
var
  NewCursor: TJclWideStrTreeNode;
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

procedure TWideStrItr.SetChild(Index: Integer; const AString: WideString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclWideStrTreeNode(FCursor.Children[Index]).Value := AString
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.SetString(const AString: WideString);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeString(FCursor.Value);
      FCursor.Value := AString;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderWideStrItr } ===================================================

function TPreOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderWideStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderWideStrItr.GetNextCursor: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclWideStrTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderWideStrItr.GetNextSibling: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderWideStrItr.GetPreviousCursor: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclWideStrTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderWideStrItr } ==================================================

function TPostOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderWideStrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderWideStrItr.GetNextCursor: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclWideStrTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderWideStrItr.GetNextSibling: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclWideStrTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderWideStrItr.GetPreviousCursor: TJclWideStrTreeNode;
var
  LastRet: TJclWideStrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclWideStrTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclWideStrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

type
  TSingleItr = class(TJclAbstractIterator, IJclSingleIterator, IJclSingleTreeIterator)
  protected
    FCursor: TJclSingleTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclSingleTree;
    FEqualityComparer: IJclSingleEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclSingleTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclSingleTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclSingleTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclSingleTree; ACursor: TJclSingleTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderSingleItr = class(TSingleItr, IJclSingleIterator, IJclSingleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleTreeNode; override;
    function GetNextSibling: TJclSingleTreeNode; override;
    function GetPreviousCursor: TJclSingleTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderSingleItr = class(TSingleItr, IJclSingleIterator, IJclSingleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclSingleTreeNode; override;
    function GetNextSibling: TJclSingleTreeNode; override;
    function GetPreviousCursor: TJclSingleTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TSingleItr } ===========================================================

constructor TSingleItr.Create(OwnTree: TJclSingleTree; ACursor: TJclSingleTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclSingleEqualityComparer;
end;

function TSingleItr.Add(const AValue: Single): Boolean;
var
  ParentNode, NewNode: TJclSingleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclSingleTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclSingleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.AddChild(const AValue: Single): Boolean;
var
  NewNode: TJclSingleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclSingleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclSingleTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclSingleTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclSingleTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TSingleItr.IndexOfChild(const AValue: Single): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.Insert(const AValue: Single): Boolean;
var
  ParentNode, NewNode: TJclSingleTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclSingleTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclSingleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TSingleItr.InsertChild(Index: Integer; const AValue: Single): Boolean;
var
  NewNode: TJclSingleTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclSingleTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclSingleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclSingleTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.Reset;
var
  NewCursor: TJclSingleTreeNode;
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

procedure TSingleItr.SetChild(Index: Integer; const AValue: Single);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclSingleTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TSingleItr.SetValue(const AValue: Single);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeSingle(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderSingleItr } ===================================================

function TPreOrderSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderSingleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderSingleItr.GetNextCursor: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclSingleTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderSingleItr.GetNextSibling: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderSingleItr.GetPreviousCursor: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclSingleTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderSingleItr } ==================================================

function TPostOrderSingleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderSingleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderSingleItr.GetNextCursor: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclSingleTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderSingleItr.GetNextSibling: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclSingleTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderSingleItr.GetPreviousCursor: TJclSingleTreeNode;
var
  LastRet: TJclSingleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclSingleTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclSingleTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

type
  TDoubleItr = class(TJclAbstractIterator, IJclDoubleIterator, IJclDoubleTreeIterator)
  protected
    FCursor: TJclDoubleTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclDoubleTree;
    FEqualityComparer: IJclDoubleEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclDoubleTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclDoubleTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclDoubleTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclDoubleTree; ACursor: TJclDoubleTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderDoubleItr = class(TDoubleItr, IJclDoubleIterator, IJclDoubleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleTreeNode; override;
    function GetNextSibling: TJclDoubleTreeNode; override;
    function GetPreviousCursor: TJclDoubleTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderDoubleItr = class(TDoubleItr, IJclDoubleIterator, IJclDoubleTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclDoubleTreeNode; override;
    function GetNextSibling: TJclDoubleTreeNode; override;
    function GetPreviousCursor: TJclDoubleTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TDoubleItr } ===========================================================

constructor TDoubleItr.Create(OwnTree: TJclDoubleTree; ACursor: TJclDoubleTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclDoubleEqualityComparer;
end;

function TDoubleItr.Add(const AValue: Double): Boolean;
var
  ParentNode, NewNode: TJclDoubleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclDoubleTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclDoubleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.AddChild(const AValue: Double): Boolean;
var
  NewNode: TJclDoubleTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclDoubleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclDoubleTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclDoubleTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclDoubleTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TDoubleItr.IndexOfChild(const AValue: Double): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.Insert(const AValue: Double): Boolean;
var
  ParentNode, NewNode: TJclDoubleTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclDoubleTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclDoubleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TDoubleItr.InsertChild(Index: Integer; const AValue: Double): Boolean;
var
  NewNode: TJclDoubleTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclDoubleTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclDoubleTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclDoubleTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.Reset;
var
  NewCursor: TJclDoubleTreeNode;
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

procedure TDoubleItr.SetChild(Index: Integer; const AValue: Double);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclDoubleTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TDoubleItr.SetValue(const AValue: Double);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeDouble(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderDoubleItr } ===================================================

function TPreOrderDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderDoubleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderDoubleItr.GetNextCursor: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclDoubleTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderDoubleItr.GetNextSibling: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderDoubleItr.GetPreviousCursor: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclDoubleTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderDoubleItr } ==================================================

function TPostOrderDoubleItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderDoubleItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderDoubleItr.GetNextCursor: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclDoubleTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderDoubleItr.GetNextSibling: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclDoubleTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderDoubleItr.GetPreviousCursor: TJclDoubleTreeNode;
var
  LastRet: TJclDoubleTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclDoubleTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclDoubleTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

type
  TExtendedItr = class(TJclAbstractIterator, IJclExtendedIterator, IJclExtendedTreeIterator)
  protected
    FCursor: TJclExtendedTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclExtendedTree;
    FEqualityComparer: IJclExtendedEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclExtendedTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclExtendedTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclExtendedTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclExtendedTree; ACursor: TJclExtendedTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderExtendedItr = class(TExtendedItr, IJclExtendedIterator, IJclExtendedTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedTreeNode; override;
    function GetNextSibling: TJclExtendedTreeNode; override;
    function GetPreviousCursor: TJclExtendedTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderExtendedItr = class(TExtendedItr, IJclExtendedIterator, IJclExtendedTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclExtendedTreeNode; override;
    function GetNextSibling: TJclExtendedTreeNode; override;
    function GetPreviousCursor: TJclExtendedTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TExtendedItr } ===========================================================

constructor TExtendedItr.Create(OwnTree: TJclExtendedTree; ACursor: TJclExtendedTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclExtendedEqualityComparer;
end;

function TExtendedItr.Add(const AValue: Extended): Boolean;
var
  ParentNode, NewNode: TJclExtendedTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclExtendedTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclExtendedTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.AddChild(const AValue: Extended): Boolean;
var
  NewNode: TJclExtendedTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclExtendedTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclExtendedTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclExtendedTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclExtendedTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TExtendedItr.IndexOfChild(const AValue: Extended): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.Insert(const AValue: Extended): Boolean;
var
  ParentNode, NewNode: TJclExtendedTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclExtendedTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclExtendedTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TExtendedItr.InsertChild(Index: Integer; const AValue: Extended): Boolean;
var
  NewNode: TJclExtendedTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclExtendedTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0.0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclExtendedTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclExtendedTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.Reset;
var
  NewCursor: TJclExtendedTreeNode;
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

procedure TExtendedItr.SetChild(Index: Integer; const AValue: Extended);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclExtendedTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TExtendedItr.SetValue(const AValue: Extended);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeExtended(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderExtendedItr } ===================================================

function TPreOrderExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderExtendedItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderExtendedItr.GetNextCursor: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclExtendedTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderExtendedItr.GetNextSibling: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderExtendedItr.GetPreviousCursor: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclExtendedTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderExtendedItr } ==================================================

function TPostOrderExtendedItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderExtendedItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderExtendedItr.GetNextCursor: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclExtendedTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderExtendedItr.GetNextSibling: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclExtendedTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderExtendedItr.GetPreviousCursor: TJclExtendedTreeNode;
var
  LastRet: TJclExtendedTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclExtendedTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclExtendedTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

type
  TIntegerItr = class(TJclAbstractIterator, IJclIntegerIterator, IJclIntegerTreeIterator)
  protected
    FCursor: TJclIntegerTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclIntegerTree;
    FEqualityComparer: IJclIntegerEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntegerTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclIntegerTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclIntegerTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclIntegerTree; ACursor: TJclIntegerTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderIntegerItr = class(TIntegerItr, IJclIntegerIterator, IJclIntegerTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerTreeNode; override;
    function GetNextSibling: TJclIntegerTreeNode; override;
    function GetPreviousCursor: TJclIntegerTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderIntegerItr = class(TIntegerItr, IJclIntegerIterator, IJclIntegerTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntegerTreeNode; override;
    function GetNextSibling: TJclIntegerTreeNode; override;
    function GetPreviousCursor: TJclIntegerTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TIntegerItr } ===========================================================

constructor TIntegerItr.Create(OwnTree: TJclIntegerTree; ACursor: TJclIntegerTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclIntegerEqualityComparer;
end;

function TIntegerItr.Add(AValue: Integer): Boolean;
var
  ParentNode, NewNode: TJclIntegerTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclIntegerTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclIntegerTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.AddChild(AValue: Integer): Boolean;
var
  NewNode: TJclIntegerTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclIntegerTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclIntegerTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclIntegerTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclIntegerTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TIntegerItr.IndexOfChild(AValue: Integer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.Insert(AValue: Integer): Boolean;
var
  ParentNode, NewNode: TJclIntegerTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclIntegerTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclIntegerTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntegerItr.InsertChild(Index: Integer; AValue: Integer): Boolean;
var
  NewNode: TJclIntegerTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclIntegerTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclIntegerTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclIntegerTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.Reset;
var
  NewCursor: TJclIntegerTreeNode;
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

procedure TIntegerItr.SetChild(Index: Integer; AValue: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclIntegerTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntegerItr.SetValue(AValue: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeInteger(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderIntegerItr } ===================================================

function TPreOrderIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderIntegerItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderIntegerItr.GetNextCursor: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclIntegerTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderIntegerItr.GetNextSibling: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderIntegerItr.GetPreviousCursor: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclIntegerTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderIntegerItr } ==================================================

function TPostOrderIntegerItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderIntegerItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderIntegerItr.GetNextCursor: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclIntegerTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderIntegerItr.GetNextSibling: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclIntegerTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderIntegerItr.GetPreviousCursor: TJclIntegerTreeNode;
var
  LastRet: TJclIntegerTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclIntegerTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclIntegerTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

type
  TCardinalItr = class(TJclAbstractIterator, IJclCardinalIterator, IJclCardinalTreeIterator)
  protected
    FCursor: TJclCardinalTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclCardinalTree;
    FEqualityComparer: IJclCardinalEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclCardinalTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclCardinalTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclCardinalTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclCardinalTree; ACursor: TJclCardinalTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderCardinalItr = class(TCardinalItr, IJclCardinalIterator, IJclCardinalTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalTreeNode; override;
    function GetNextSibling: TJclCardinalTreeNode; override;
    function GetPreviousCursor: TJclCardinalTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderCardinalItr = class(TCardinalItr, IJclCardinalIterator, IJclCardinalTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclCardinalTreeNode; override;
    function GetNextSibling: TJclCardinalTreeNode; override;
    function GetPreviousCursor: TJclCardinalTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TCardinalItr } ===========================================================

constructor TCardinalItr.Create(OwnTree: TJclCardinalTree; ACursor: TJclCardinalTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclCardinalEqualityComparer;
end;

function TCardinalItr.Add(AValue: Cardinal): Boolean;
var
  ParentNode, NewNode: TJclCardinalTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclCardinalTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclCardinalTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.AddChild(AValue: Cardinal): Boolean;
var
  NewNode: TJclCardinalTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclCardinalTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclCardinalTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclCardinalTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclCardinalTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TCardinalItr.IndexOfChild(AValue: Cardinal): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.Insert(AValue: Cardinal): Boolean;
var
  ParentNode, NewNode: TJclCardinalTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclCardinalTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclCardinalTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TCardinalItr.InsertChild(Index: Integer; AValue: Cardinal): Boolean;
var
  NewNode: TJclCardinalTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclCardinalTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclCardinalTreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclCardinalTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.Reset;
var
  NewCursor: TJclCardinalTreeNode;
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

procedure TCardinalItr.SetChild(Index: Integer; AValue: Cardinal);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclCardinalTreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TCardinalItr.SetValue(AValue: Cardinal);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeCardinal(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderCardinalItr } ===================================================

function TPreOrderCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderCardinalItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderCardinalItr.GetNextCursor: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclCardinalTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderCardinalItr.GetNextSibling: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderCardinalItr.GetPreviousCursor: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclCardinalTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderCardinalItr } ==================================================

function TPostOrderCardinalItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderCardinalItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderCardinalItr.GetNextCursor: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclCardinalTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderCardinalItr.GetNextSibling: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclCardinalTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderCardinalItr.GetPreviousCursor: TJclCardinalTreeNode;
var
  LastRet: TJclCardinalTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclCardinalTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclCardinalTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

type
  TInt64Itr = class(TJclAbstractIterator, IJclInt64Iterator, IJclInt64TreeIterator)
  protected
    FCursor: TJclInt64TreeNode;
    FStart: TItrStart;
    FOwnTree: TJclInt64Tree;
    FEqualityComparer: IJclInt64EqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclInt64TreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclInt64TreeNode; virtual; abstract;
    function GetPreviousCursor: TJclInt64TreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclInt64Tree; ACursor: TJclInt64TreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderInt64Itr = class(TInt64Itr, IJclInt64Iterator, IJclInt64TreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64TreeNode; override;
    function GetNextSibling: TJclInt64TreeNode; override;
    function GetPreviousCursor: TJclInt64TreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderInt64Itr = class(TInt64Itr, IJclInt64Iterator, IJclInt64TreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclInt64TreeNode; override;
    function GetNextSibling: TJclInt64TreeNode; override;
    function GetPreviousCursor: TJclInt64TreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TInt64Itr } ===========================================================

constructor TInt64Itr.Create(OwnTree: TJclInt64Tree; ACursor: TJclInt64TreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclInt64EqualityComparer;
end;

function TInt64Itr.Add(const AValue: Int64): Boolean;
var
  ParentNode, NewNode: TJclInt64TreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclInt64Tree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclInt64TreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.AddChild(const AValue: Int64): Boolean;
var
  NewNode: TJclInt64TreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclInt64TreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclInt64TreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclInt64TreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclInt64TreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TInt64Itr.IndexOfChild(const AValue: Int64): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AValue, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.Insert(const AValue: Int64): Boolean;
var
  ParentNode, NewNode: TJclInt64TreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclInt64Tree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclInt64TreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TInt64Itr.InsertChild(Index: Integer; const AValue: Int64): Boolean;
var
  NewNode: TJclInt64TreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclInt64Tree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AValue, 0))
      and ((not FOwnTree.Contains(AValue)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclInt64TreeNode.Create;
        NewNode.Value := AValue;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclInt64TreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.Reset;
var
  NewCursor: TJclInt64TreeNode;
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

procedure TInt64Itr.SetChild(Index: Integer; const AValue: Int64);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclInt64TreeNode(FCursor.Children[Index]).Value := AValue
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TInt64Itr.SetValue(const AValue: Int64);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeInt64(FCursor.Value);
      FCursor.Value := AValue;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderInt64Itr } ===================================================

function TPreOrderInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderInt64Itr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderInt64Itr.GetNextCursor: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclInt64TreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderInt64Itr.GetNextSibling: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderInt64Itr.GetPreviousCursor: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclInt64TreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderInt64Itr } ==================================================

function TPostOrderInt64Itr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderInt64Itr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderInt64Itr.GetNextCursor: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclInt64TreeNode(Result.Children[0]);
  end;
end;

function TPostOrderInt64Itr.GetNextSibling: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclInt64TreeNode(Result.Children[0]);
  end;
end;

function TPostOrderInt64Itr.GetPreviousCursor: TJclInt64TreeNode;
var
  LastRet: TJclInt64TreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclInt64TreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclInt64TreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

{$IFNDEF CLR}
type
  TPtrItr = class(TJclAbstractIterator, IJclPtrIterator, IJclPtrTreeIterator)
  protected
    FCursor: TJclPtrTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclPtrTree;
    FEqualityComparer: IJclPtrEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclPtrTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclPtrTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclPtrTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclPtrTree; ACursor: TJclPtrTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderPtrItr = class(TPtrItr, IJclPtrIterator, IJclPtrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrTreeNode; override;
    function GetNextSibling: TJclPtrTreeNode; override;
    function GetPreviousCursor: TJclPtrTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderPtrItr = class(TPtrItr, IJclPtrIterator, IJclPtrTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclPtrTreeNode; override;
    function GetNextSibling: TJclPtrTreeNode; override;
    function GetPreviousCursor: TJclPtrTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TPtrItr } ===========================================================

constructor TPtrItr.Create(OwnTree: TJclPtrTree; ACursor: TJclPtrTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclPtrEqualityComparer;
end;

function TPtrItr.Add(APtr: Pointer): Boolean;
var
  ParentNode, NewNode: TJclPtrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclPtrTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil))
      and ((not FOwnTree.Contains(APtr)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclPtrTreeNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.AddChild(APtr: Pointer): Boolean;
var
  NewNode: TJclPtrTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil))
      and ((not FOwnTree.Contains(APtr)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclPtrTreeNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclPtrTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclPtrTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclPtrTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TPtrItr.IndexOfChild(APtr: Pointer): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(APtr, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.Insert(APtr: Pointer): Boolean;
var
  ParentNode, NewNode: TJclPtrTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclPtrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil))
      and ((not FOwnTree.Contains(APtr)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclPtrTreeNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TPtrItr.InsertChild(Index: Integer; APtr: Pointer): Boolean;
var
  NewNode: TJclPtrTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclPtrTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(APtr, nil))
      and ((not FOwnTree.Contains(APtr)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclPtrTreeNode.Create;
        NewNode.Value := APtr;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclPtrTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.Reset;
var
  NewCursor: TJclPtrTreeNode;
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

procedure TPtrItr.SetChild(Index: Integer; APtr: Pointer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclPtrTreeNode(FCursor.Children[Index]).Value := APtr
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TPtrItr.SetPointer(APtr: Pointer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreePointer(FCursor.Value);
      FCursor.Value := APtr;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderPtrItr } ===================================================

function TPreOrderPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderPtrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderPtrItr.GetNextCursor: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclPtrTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderPtrItr.GetNextSibling: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderPtrItr.GetPreviousCursor: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclPtrTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderPtrItr } ==================================================

function TPostOrderPtrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderPtrItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderPtrItr.GetNextCursor: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclPtrTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderPtrItr.GetNextSibling: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclPtrTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderPtrItr.GetPreviousCursor: TJclPtrTreeNode;
var
  LastRet: TJclPtrTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclPtrTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclPtrTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;
{$ENDIF ~CLR}

type
  TItr = class(TJclAbstractIterator, IJclIterator, IJclTreeIterator)
  protected
    FCursor: TJclTreeNode;
    FStart: TItrStart;
    FOwnTree: TJclTree;
    FEqualityComparer: IJclEqualityComparer; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclTreeNode; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclTreeNode; virtual; abstract;
    function GetPreviousCursor: TJclTreeNode; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclTree; ACursor: TJclTreeNode; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderItr = class(TItr, IJclIterator, IJclTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclTreeNode; override;
    function GetNextSibling: TJclTreeNode; override;
    function GetPreviousCursor: TJclTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderItr = class(TItr, IJclIterator, IJclTreeIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclTreeNode; override;
    function GetNextSibling: TJclTreeNode; override;
    function GetPreviousCursor: TJclTreeNode; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TItr } ===========================================================

constructor TItr.Create(OwnTree: TJclTree; ACursor: TJclTreeNode; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclEqualityComparer;
end;

function TItr.Add(AObject: TObject): Boolean;
var
  ParentNode, NewNode: TJclTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclTree.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil))
      and ((not FOwnTree.Contains(AObject)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclTreeNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.AddChild(AObject: TObject): Boolean;
var
  NewNode: TJclTreeNode;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil))
      and ((not FOwnTree.Contains(AObject)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclTreeNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclTreeNode(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclTreeNode(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclTreeNode(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TItr.IndexOfChild(AObject: TObject): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AObject, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.Insert(AObject: TObject): Boolean;
var
  ParentNode, NewNode: TJclTreeNode;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil))
      and ((not FOwnTree.Contains(AObject)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclTreeNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.InsertChild(Index: Integer; AObject: TObject): Boolean;
var
  NewNode: TJclTreeNode;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclTree.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil))
      and ((not FOwnTree.Contains(AObject)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclTreeNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclTreeNode;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.Reset;
var
  NewCursor: TJclTreeNode;
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

procedure TItr.SetChild(Index: Integer; AObject: TObject);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclTreeNode(FCursor.Children[Index]).Value := AObject
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.SetObject(AObject: TObject);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeObject(FCursor.Value);
      FCursor.Value := AObject;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderItr } ===================================================

function TPreOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderItr.GetNextCursor: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclTreeNode(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderItr.GetNextSibling: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderItr.GetPreviousCursor: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclTreeNode(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderItr } ==================================================

function TPostOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderItr.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderItr.GetNextCursor: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderItr.GetNextSibling: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclTreeNode(Result.Children[0]);
  end;
end;

function TPostOrderItr.GetPreviousCursor: TJclTreeNode;
var
  LastRet: TJclTreeNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclTreeNode(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclTreeNode(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;

{$IFDEF SUPPORTS_GENERICS}
type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>, IJclTreeIterator<T>)
  protected
    FCursor: TJclTreeNode<T>;
    FStart: TItrStart;
    FOwnTree: TJclTree<T>;
    FEqualityComparer: IJclEqualityComparer<T>; // keep a reference  of tree interface
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclTreeNode<T>; virtual; abstract;
    // return next node on the same level
    function GetNextSibling: TJclTreeNode<T>; virtual; abstract;
    function GetPreviousCursor: TJclTreeNode<T>; virtual; abstract;
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
  public
    constructor Create(OwnTree: TJclTree<T>; ACursor: TJclTreeNode<T>; AValid: Boolean; AStart: TItrStart);
  end;

  TPreOrderItr<T> = class(TItr<T>, IJclIterator<T>, IJclTreeIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclTreeNode<T>; override;
    function GetNextSibling: TJclTreeNode<T>; override;
    function GetPreviousCursor: TJclTreeNode<T>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

  TPostOrderItr<T> = class(TItr<T>, IJclIterator<T>, IJclTreeIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclTreeNode<T>; override;
    function GetNextSibling: TJclTreeNode<T>; override;
    function GetPreviousCursor: TJclTreeNode<T>; override;
    { IJclCloneable }
    function IJclCloneable.Clone = ObjectClone;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

//=== { TItr<T> } ===========================================================

constructor TItr<T>.Create(OwnTree: TJclTree<T>; ACursor: TJclTreeNode<T>; AValid: Boolean; AStart: TItrStart);
begin
  inherited Create(AValid);
  FCursor := ACursor;
  FOwnTree := OwnTree;
  FStart := AStart;
  FEqualityComparer := OwnTree as IJclEqualityComparer<T>;
end;

function TItr<T>.Add(const AItem: T): Boolean;
var
  ParentNode, NewNode: TJclTreeNode<T>;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // add sibling or, if FCursor is root node, behave like TJclTree<T>.Add
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T)))
      and ((not FOwnTree.Contains(AItem)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      ParentNode := FCursor.Parent;
      if ParentNode = nil then
        ParentNode := FCursor;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclTreeNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := ParentNode;
        ParentNode.Children[ParentNode.ChildrenCount] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.AddChild(const AItem: T): Boolean;
var
  NewNode: TJclTreeNode<T>;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T)))
      and ((not FOwnTree.Contains(AItem)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclTreeNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := FCursor;
        FCursor.Children[FCursor.ChildrenCount] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if FCursor <> nil then
      Result := FCursor.ChildrenCount
    else
      Result := 0;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.ClearChildren;
var
  Index: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
    begin
      for Index := FCursor.ChildrenCount - 1 downto 0 do
        {$IFDEF BCB}
        FOwnTree.ClearNode(TJclTreeNode<T>(FCursor.Children[Index]));
        {$ELSE ~BCB}
        FOwnTree.ClearNode(FCursor.Children[Index]);
        {$ENDIF ~BCB}
      SetLength(FCursor.Children, 0);
      FCursor.ChildrenCount := 0;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.DeleteChild(Index: Integer);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      {$IFDEF BCB}
      FOwnTree.ClearNode(TJclTreeNode<T>(FCursor.Children[Index]))
      {$ELSE ~BCB}
      FOwnTree.ClearNode(FCursor.Children[Index])
      {$ENDIF ~BCB}
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      FCursor := TJclTreeNode<T>(FCursor.Children[Index]);
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
    Result := (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount);
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

function TItr<T>.IndexOfChild(const AItem: T): Integer;
begin
  {$IFDEF THREADSAFE}
  FOwnTree.ReadLock;
  try
  {$ENDIF THREADSAFE}
    if FCursor <> nil then
      Result := FCursor.IndexOfValue(AItem, FEqualityComparer)
    else
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.Insert(const AItem: T): Boolean;
var
  ParentNode, NewNode: TJclTreeNode<T>;
  Index, I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclTree<T>.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T)))
      and ((not FOwnTree.Contains(AItem)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.Parent <> nil then
      begin
        ParentNode := FCursor.Parent;
        Index := 0;
        while (Index < ParentNode.ChildrenCount) and (ParentNode.Children[Index] <> FCursor) do
          Inc(Index);
      end
      else
      begin
        ParentNode := FCursor;
        Index := 0;
      end;

      if ParentNode.ChildrenCount = Length(ParentNode.Children) then
        SetLength(ParentNode.Children, FOwnTree.CalcGrowCapacity(Length(ParentNode.Children), ParentNode.ChildrenCount));
      if ParentNode.ChildrenCount < Length(ParentNode.Children) then
      begin
        NewNode := TJclTreeNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := ParentNode;
        for I := ParentNode.ChildrenCount - 1 downto Index do
          ParentNode.Children[I + 1] := ParentNode.Children[I];
        ParentNode.Children[Index] := NewNode;
        Inc(ParentNode.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.InsertChild(Index: Integer; const AItem: T): Boolean;
var
  NewNode: TJclTreeNode<T>;
  I: Integer;
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    // insert sibling or, if FCursor is root node, behave like TJclTree<T>.Insert
    Result := (FCursor <> nil) and (FOwnTree.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T)))
      and ((not FOwnTree.Contains(AItem)) or FOwnTree.CheckDuplicate);

    if Result then
    begin
      if FCursor.ChildrenCount = Length(FCursor.Children) then
        SetLength(FCursor.Children, FOwnTree.CalcGrowCapacity(Length(FCursor.Children), FCursor.ChildrenCount));
      if FCursor.ChildrenCount < Length(FCursor.Children) then
      begin
        NewNode := TJclTreeNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := FCursor;
        for I := FCursor.ChildrenCount - 1 downto Index do
          FCursor.Children[I + 1] := FCursor.Children[I];
        FCursor.Children[Index] := NewNode;
        Inc(FCursor.ChildrenCount);
        Inc(FOwnTree.FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
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
  OldCursor: TJclTreeNode<T>;
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
    FCursor := GetNextSibling;
    if OldCursor <> nil then
      FOwnTree.ClearNode(OldCursor);
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.Reset;
var
  NewCursor: TJclTreeNode<T>;
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

procedure TItr<T>.SetChild(Index: Integer; const AItem: T);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (FCursor <> nil) and (Index >= 0) and (Index < FCursor.ChildrenCount) then
      TJclTreeNode<T>(FCursor.Children[Index]).Value := AItem
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  if FOwnTree.ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  FOwnTree.WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    if FCursor <> nil then
    begin
      FOwnTree.FreeItem(FCursor.Value);
      FCursor.Value := AItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    FOwnTree.WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TPreOrderItr<T> } ===================================================

function TPreOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderItr<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPreOrderItr<T>.GetNextCursor: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.ChildrenCount > 0 then
    Result := TJclTreeNode<T>(Result.Children[0])
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root = return successor
      Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) + 1]);
  end;
end;

function TPreOrderItr<T>.GetNextSibling: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;

  Result := Result.Parent;
  while (Result <> nil) and (Result.IndexOfChild(LastRet) = (Result.ChildrenCount - 1)) do
  begin
    LastRet := Result;
    Result := Result.Parent;
  end;
  if Result <> nil then // not root = return successor
    Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) + 1]);
end;

function TPreOrderItr<T>.GetPreviousCursor: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) > 0) then
    // come from Right
  begin
    Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) - 1]);
    while (Result.ChildrenCount > 0) do // descend down the tree
      Result := TJclTreeNode<T>(Result.Children[Result.ChildrenCount - 1]);
  end;
end;

//=== { TPostOrderItr<T> } ==================================================

function TPostOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderItr<T>.Create(FOwnTree, FCursor, Valid, FStart);
end;

function TPostOrderItr<T>.GetNextCursor: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclTreeNode<T>(Result.Children[0]);
  end;
end;

function TPostOrderItr<T>.GetNextSibling: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;

  if (Result <> nil) and (Result.IndexOfChild(LastRet) <> (Result.ChildrenCount - 1)) then
  begin
    Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) + 1]);
    while Result.ChildrenCount > 0 do
      Result := TJclTreeNode<T>(Result.Children[0]);
  end;
end;

function TPostOrderItr<T>.GetPreviousCursor: TJclTreeNode<T>;
var
  LastRet: TJclTreeNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.ChildrenCount > 0 then
    Result := TJclTreeNode<T>(Result.Children[Result.ChildrenCount - 1])
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.IndexOfChild(LastRet) = 0) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := TJclTreeNode<T>(Result.Children[Result.IndexOfChild(LastRet) - 1]);
  end;
end;
{$ENDIF SUPPORTS_GENERICS}

//=== { TJclIntfTreeNode } =======================================================

function TJclIntfTreeNode.IndexOfChild(AChild: TJclIntfTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclIntfTreeNode.IndexOfValue(const AInterface: IInterface;
  const AEqualityComparer: IJclIntfEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclIntfTreeNode(Children[Result]).Value, AInterface) then
      Exit;
  Result := -1;
end;

//=== { TJclIntfTree } =======================================================

constructor TJclIntfTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclIntfTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntfTree.Add(const AInterface: IInterface): Boolean;
var
  NewNode: TJclIntfTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AInterface, nil);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AInterface)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclIntfTreeNode.Create;
            NewNode.Value := AInterface;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclIntfTreeNode.Create;
        FRoot.Value := AInterface;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.AddAll(const ACollection: IJclIntfCollection): Boolean;
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

procedure TJclIntfTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclIntfTreeNode): TJclIntfTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclIntfTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclIntfTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclIntfTree;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfTree then
  begin
    ADest := TJclIntfTree(Dest);
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

procedure TJclIntfTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfTree then
    TJclIntfTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntfTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfTree.ClearNode(var ANode: TJclIntfTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclIntfTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclIntfTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeObject(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclIntfTree.Contains(const AInterface: IInterface): Boolean;
  function NodeContains(ANode: TJclIntfTreeNode; const AInterface: IInterface): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AInterface);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclIntfTreeNode(ANode.Children[Index]), AInterface);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AInterface)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclIntfTree.Equals(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfTree.First: IJclIntfIterator;
var
  Start: TJclIntfTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclIntfTreeNode(Start.Children[0]);
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
function TJclIntfTree.GetEnumerator: IJclIntfIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfTree.GetRoot: IJclIntfTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderIntfItr.Create(Self, FRoot, False, isRoot);
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

function TJclIntfTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntfTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfTree.Last: IJclIntfIterator;
var
  Start: TJclIntfTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclIntfTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderIntfItr.Create(Self, Start, False, isLast);
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

procedure TJclIntfTree.Pack;
  procedure PackNode(ANode: TJclIntfTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclIntfTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.Remove(const AInterface: IInterface): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AInterface) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfTree.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfTree.RetainAll(const ACollection: IJclIntfCollection): Boolean;
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

procedure TJclIntfTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntfTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclAnsiStrTreeNode } =======================================================

function TJclAnsiStrTreeNode.IndexOfChild(AChild: TJclAnsiStrTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclAnsiStrTreeNode.IndexOfValue(const AString: AnsiString;
  const AEqualityComparer: IJclAnsiStrEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclAnsiStrTreeNode(Children[Result]).Value, AString) then
      Exit;
  Result := -1;
end;

//=== { TJclAnsiStrTree } =======================================================

constructor TJclAnsiStrTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclAnsiStrTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrTree.Add(const AString: AnsiString): Boolean;
var
  NewNode: TJclAnsiStrTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AString, '');

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AString)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclAnsiStrTreeNode.Create;
            NewNode.Value := AString;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclAnsiStrTreeNode.Create;
        FRoot.Value := AString;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

procedure TJclAnsiStrTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclAnsiStrTreeNode): TJclAnsiStrTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclAnsiStrTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclAnsiStrTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclAnsiStrTree;
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrTree then
  begin
    ADest := TJclAnsiStrTree(Dest);
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

procedure TJclAnsiStrTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrTree then
    TJclAnsiStrTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclAnsiStrTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrTree.ClearNode(var ANode: TJclAnsiStrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclAnsiStrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclAnsiStrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeString(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclAnsiStrTree.Contains(const AString: AnsiString): Boolean;
  function NodeContains(ANode: TJclAnsiStrTreeNode; const AString: AnsiString): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AString);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclAnsiStrTreeNode(ANode.Children[Index]), AString);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrTree.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrTree.First: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclAnsiStrTreeNode(Start.Children[0]);
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
function TJclAnsiStrTree.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrTree.GetRoot: IJclAnsiStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderAnsiStrItr.Create(Self, FRoot, False, isRoot);
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

function TJclAnsiStrTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclAnsiStrTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrTree.Last: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclAnsiStrTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderAnsiStrItr.Create(Self, Start, False, isLast);
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

procedure TJclAnsiStrTree.Pack;
  procedure PackNode(ANode: TJclAnsiStrTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclAnsiStrTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.Remove(const AString: AnsiString): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AString) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrTree.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

function TJclAnsiStrTree.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

procedure TJclAnsiStrTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclAnsiStrTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclWideStrTreeNode } =======================================================

function TJclWideStrTreeNode.IndexOfChild(AChild: TJclWideStrTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclWideStrTreeNode.IndexOfValue(const AString: WideString;
  const AEqualityComparer: IJclWideStrEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclWideStrTreeNode(Children[Result]).Value, AString) then
      Exit;
  Result := -1;
end;

//=== { TJclWideStrTree } =======================================================

constructor TJclWideStrTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclWideStrTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclWideStrTree.Add(const AString: WideString): Boolean;
var
  NewNode: TJclWideStrTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AString, '');

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AString)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclWideStrTreeNode.Create;
            NewNode.Value := AString;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclWideStrTreeNode.Create;
        FRoot.Value := AString;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.AddAll(const ACollection: IJclWideStrCollection): Boolean;
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

procedure TJclWideStrTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclWideStrTreeNode): TJclWideStrTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclWideStrTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclWideStrTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclWideStrTree;
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrTree then
  begin
    ADest := TJclWideStrTree(Dest);
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

procedure TJclWideStrTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrTree then
    TJclWideStrTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclWideStrTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrTree.ClearNode(var ANode: TJclWideStrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclWideStrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclWideStrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeString(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclWideStrTree.Contains(const AString: WideString): Boolean;
  function NodeContains(ANode: TJclWideStrTreeNode; const AString: WideString): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AString);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclWideStrTreeNode(ANode.Children[Index]), AString);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclWideStrTree.Equals(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrTree.First: IJclWideStrIterator;
var
  Start: TJclWideStrTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclWideStrTreeNode(Start.Children[0]);
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
function TJclWideStrTree.GetEnumerator: IJclWideStrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrTree.GetRoot: IJclWideStrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderWideStrItr.Create(Self, FRoot, False, isRoot);
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

function TJclWideStrTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclWideStrTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrTree.Last: IJclWideStrIterator;
var
  Start: TJclWideStrTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclWideStrTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderWideStrItr.Create(Self, Start, False, isLast);
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

procedure TJclWideStrTree.Pack;
  procedure PackNode(ANode: TJclWideStrTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclWideStrTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.Remove(const AString: WideString): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AString) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrTree.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
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

function TJclWideStrTree.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
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

procedure TJclWideStrTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclWideStrTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclSingleTreeNode } =======================================================

function TJclSingleTreeNode.IndexOfChild(AChild: TJclSingleTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclSingleTreeNode.IndexOfValue(const AValue: Single;
  const AEqualityComparer: IJclSingleEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclSingleTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclSingleTree } =======================================================

constructor TJclSingleTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclSingleTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclSingleTree.Add(const AValue: Single): Boolean;
var
  NewNode: TJclSingleTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0.0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclSingleTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclSingleTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.AddAll(const ACollection: IJclSingleCollection): Boolean;
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

procedure TJclSingleTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclSingleTreeNode): TJclSingleTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclSingleTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclSingleTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclSingleTree;
  ACollection: IJclSingleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSingleTree then
  begin
    ADest := TJclSingleTree(Dest);
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

procedure TJclSingleTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleTree then
    TJclSingleTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclSingleTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleTree.ClearNode(var ANode: TJclSingleTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclSingleTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclSingleTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeSingle(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclSingleTree.Contains(const AValue: Single): Boolean;
  function NodeContains(ANode: TJclSingleTreeNode; const AValue: Single): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AValue);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclSingleTreeNode(ANode.Children[Index]), AValue);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.ContainsAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclSingleTree.Equals(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleTree.First: IJclSingleIterator;
var
  Start: TJclSingleTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclSingleTreeNode(Start.Children[0]);
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
function TJclSingleTree.GetEnumerator: IJclSingleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclSingleTree.GetRoot: IJclSingleTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderSingleItr.Create(Self, FRoot, False, isRoot);
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

function TJclSingleTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclSingleTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleTree.Last: IJclSingleIterator;
var
  Start: TJclSingleTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclSingleTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderSingleItr.Create(Self, Start, False, isLast);
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

procedure TJclSingleTree.Pack;
  procedure PackNode(ANode: TJclSingleTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclSingleTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.Remove(const AValue: Single): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleTree.RemoveAll(const ACollection: IJclSingleCollection): Boolean;
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

function TJclSingleTree.RetainAll(const ACollection: IJclSingleCollection): Boolean;
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

procedure TJclSingleTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclSingleTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclSingleTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclDoubleTreeNode } =======================================================

function TJclDoubleTreeNode.IndexOfChild(AChild: TJclDoubleTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclDoubleTreeNode.IndexOfValue(const AValue: Double;
  const AEqualityComparer: IJclDoubleEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclDoubleTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclDoubleTree } =======================================================

constructor TJclDoubleTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclDoubleTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclDoubleTree.Add(const AValue: Double): Boolean;
var
  NewNode: TJclDoubleTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0.0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclDoubleTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclDoubleTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.AddAll(const ACollection: IJclDoubleCollection): Boolean;
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

procedure TJclDoubleTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclDoubleTreeNode): TJclDoubleTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclDoubleTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclDoubleTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclDoubleTree;
  ACollection: IJclDoubleCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclDoubleTree then
  begin
    ADest := TJclDoubleTree(Dest);
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

procedure TJclDoubleTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleTree then
    TJclDoubleTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclDoubleTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleTree.ClearNode(var ANode: TJclDoubleTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclDoubleTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclDoubleTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeDouble(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclDoubleTree.Contains(const AValue: Double): Boolean;
  function NodeContains(ANode: TJclDoubleTreeNode; const AValue: Double): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AValue);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclDoubleTreeNode(ANode.Children[Index]), AValue);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.ContainsAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclDoubleTree.Equals(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleTree.First: IJclDoubleIterator;
var
  Start: TJclDoubleTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclDoubleTreeNode(Start.Children[0]);
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
function TJclDoubleTree.GetEnumerator: IJclDoubleIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclDoubleTree.GetRoot: IJclDoubleTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderDoubleItr.Create(Self, FRoot, False, isRoot);
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

function TJclDoubleTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclDoubleTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleTree.Last: IJclDoubleIterator;
var
  Start: TJclDoubleTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclDoubleTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderDoubleItr.Create(Self, Start, False, isLast);
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

procedure TJclDoubleTree.Pack;
  procedure PackNode(ANode: TJclDoubleTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclDoubleTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.Remove(const AValue: Double): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleTree.RemoveAll(const ACollection: IJclDoubleCollection): Boolean;
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

function TJclDoubleTree.RetainAll(const ACollection: IJclDoubleCollection): Boolean;
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

procedure TJclDoubleTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclDoubleTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclDoubleTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclExtendedTreeNode } =======================================================

function TJclExtendedTreeNode.IndexOfChild(AChild: TJclExtendedTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclExtendedTreeNode.IndexOfValue(const AValue: Extended;
  const AEqualityComparer: IJclExtendedEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclExtendedTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclExtendedTree } =======================================================

constructor TJclExtendedTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclExtendedTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclExtendedTree.Add(const AValue: Extended): Boolean;
var
  NewNode: TJclExtendedTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0.0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclExtendedTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclExtendedTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.AddAll(const ACollection: IJclExtendedCollection): Boolean;
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

procedure TJclExtendedTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclExtendedTreeNode): TJclExtendedTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclExtendedTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclExtendedTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclExtendedTree;
  ACollection: IJclExtendedCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclExtendedTree then
  begin
    ADest := TJclExtendedTree(Dest);
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

procedure TJclExtendedTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedTree then
    TJclExtendedTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclExtendedTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedTree.ClearNode(var ANode: TJclExtendedTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclExtendedTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclExtendedTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeExtended(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclExtendedTree.Contains(const AValue: Extended): Boolean;
  function NodeContains(ANode: TJclExtendedTreeNode; const AValue: Extended): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AValue);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclExtendedTreeNode(ANode.Children[Index]), AValue);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.ContainsAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclExtendedTree.Equals(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedTree.First: IJclExtendedIterator;
var
  Start: TJclExtendedTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclExtendedTreeNode(Start.Children[0]);
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
function TJclExtendedTree.GetEnumerator: IJclExtendedIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclExtendedTree.GetRoot: IJclExtendedTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderExtendedItr.Create(Self, FRoot, False, isRoot);
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

function TJclExtendedTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclExtendedTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedTree.Last: IJclExtendedIterator;
var
  Start: TJclExtendedTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclExtendedTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderExtendedItr.Create(Self, Start, False, isLast);
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

procedure TJclExtendedTree.Pack;
  procedure PackNode(ANode: TJclExtendedTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclExtendedTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.Remove(const AValue: Extended): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedTree.RemoveAll(const ACollection: IJclExtendedCollection): Boolean;
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

function TJclExtendedTree.RetainAll(const ACollection: IJclExtendedCollection): Boolean;
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

procedure TJclExtendedTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclExtendedTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclExtendedTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclIntegerTreeNode } =======================================================

function TJclIntegerTreeNode.IndexOfChild(AChild: TJclIntegerTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclIntegerTreeNode.IndexOfValue(AValue: Integer;
  const AEqualityComparer: IJclIntegerEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclIntegerTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclIntegerTree } =======================================================

constructor TJclIntegerTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclIntegerTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclIntegerTree.Add(AValue: Integer): Boolean;
var
  NewNode: TJclIntegerTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclIntegerTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclIntegerTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.AddAll(const ACollection: IJclIntegerCollection): Boolean;
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

procedure TJclIntegerTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclIntegerTreeNode): TJclIntegerTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclIntegerTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclIntegerTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclIntegerTree;
  ACollection: IJclIntegerCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntegerTree then
  begin
    ADest := TJclIntegerTree(Dest);
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

procedure TJclIntegerTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerTree then
    TJclIntegerTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntegerTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerTree.ClearNode(var ANode: TJclIntegerTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclIntegerTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclIntegerTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeInteger(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclIntegerTree.Contains(AValue: Integer): Boolean;
  function NodeContains(ANode: TJclIntegerTreeNode; AValue: Integer): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AValue);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclIntegerTreeNode(ANode.Children[Index]), AValue);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.ContainsAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclIntegerTree.Equals(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerTree.First: IJclIntegerIterator;
var
  Start: TJclIntegerTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclIntegerTreeNode(Start.Children[0]);
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
function TJclIntegerTree.GetEnumerator: IJclIntegerIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntegerTree.GetRoot: IJclIntegerTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderIntegerItr.Create(Self, FRoot, False, isRoot);
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

function TJclIntegerTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntegerTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerTree.Last: IJclIntegerIterator;
var
  Start: TJclIntegerTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclIntegerTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderIntegerItr.Create(Self, Start, False, isLast);
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

procedure TJclIntegerTree.Pack;
  procedure PackNode(ANode: TJclIntegerTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclIntegerTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.Remove(AValue: Integer): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerTree.RemoveAll(const ACollection: IJclIntegerCollection): Boolean;
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

function TJclIntegerTree.RetainAll(const ACollection: IJclIntegerCollection): Boolean;
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

procedure TJclIntegerTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntegerTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntegerTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclCardinalTreeNode } =======================================================

function TJclCardinalTreeNode.IndexOfChild(AChild: TJclCardinalTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclCardinalTreeNode.IndexOfValue(AValue: Cardinal;
  const AEqualityComparer: IJclCardinalEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclCardinalTreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclCardinalTree } =======================================================

constructor TJclCardinalTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclCardinalTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclCardinalTree.Add(AValue: Cardinal): Boolean;
var
  NewNode: TJclCardinalTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclCardinalTreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclCardinalTreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.AddAll(const ACollection: IJclCardinalCollection): Boolean;
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

procedure TJclCardinalTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclCardinalTreeNode): TJclCardinalTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclCardinalTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclCardinalTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclCardinalTree;
  ACollection: IJclCardinalCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclCardinalTree then
  begin
    ADest := TJclCardinalTree(Dest);
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

procedure TJclCardinalTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalTree then
    TJclCardinalTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclCardinalTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalTree.ClearNode(var ANode: TJclCardinalTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclCardinalTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclCardinalTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeCardinal(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclCardinalTree.Contains(AValue: Cardinal): Boolean;
  function NodeContains(ANode: TJclCardinalTreeNode; AValue: Cardinal): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AValue);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclCardinalTreeNode(ANode.Children[Index]), AValue);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.ContainsAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclCardinalTree.Equals(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalTree.First: IJclCardinalIterator;
var
  Start: TJclCardinalTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclCardinalTreeNode(Start.Children[0]);
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
function TJclCardinalTree.GetEnumerator: IJclCardinalIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclCardinalTree.GetRoot: IJclCardinalTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderCardinalItr.Create(Self, FRoot, False, isRoot);
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

function TJclCardinalTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclCardinalTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalTree.Last: IJclCardinalIterator;
var
  Start: TJclCardinalTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclCardinalTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderCardinalItr.Create(Self, Start, False, isLast);
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

procedure TJclCardinalTree.Pack;
  procedure PackNode(ANode: TJclCardinalTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclCardinalTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.Remove(AValue: Cardinal): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalTree.RemoveAll(const ACollection: IJclCardinalCollection): Boolean;
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

function TJclCardinalTree.RetainAll(const ACollection: IJclCardinalCollection): Boolean;
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

procedure TJclCardinalTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclCardinalTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclCardinalTree.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclInt64TreeNode } =======================================================

function TJclInt64TreeNode.IndexOfChild(AChild: TJclInt64TreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclInt64TreeNode.IndexOfValue(const AValue: Int64;
  const AEqualityComparer: IJclInt64EqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclInt64TreeNode(Children[Result]).Value, AValue) then
      Exit;
  Result := -1;
end;

//=== { TJclInt64Tree } =======================================================

constructor TJclInt64Tree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclInt64Tree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclInt64Tree.Add(const AValue: Int64): Boolean;
var
  NewNode: TJclInt64TreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AValue, 0);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AValue)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclInt64TreeNode.Create;
            NewNode.Value := AValue;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclInt64TreeNode.Create;
        FRoot.Value := AValue;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.AddAll(const ACollection: IJclInt64Collection): Boolean;
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

procedure TJclInt64Tree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclInt64TreeNode): TJclInt64TreeNode;
  var
    Index: Integer;
  begin
    Result := TJclInt64TreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclInt64TreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclInt64Tree;
  ACollection: IJclInt64Collection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclInt64Tree then
  begin
    ADest := TJclInt64Tree(Dest);
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

procedure TJclInt64Tree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64Tree then
    TJclInt64Tree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclInt64Tree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Tree.ClearNode(var ANode: TJclInt64TreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclInt64TreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclInt64TreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeInt64(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclInt64Tree.Contains(const AValue: Int64): Boolean;
  function NodeContains(ANode: TJclInt64TreeNode; const AValue: Int64): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AValue);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclInt64TreeNode(ANode.Children[Index]), AValue);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AValue)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.ContainsAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64Tree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Tree.Create;
  AssignPropertiesTo(Result);
end;

function TJclInt64Tree.Equals(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64Tree.First: IJclInt64Iterator;
var
  Start: TJclInt64TreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclInt64TreeNode(Start.Children[0]);
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
function TJclInt64Tree.GetEnumerator: IJclInt64Iterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclInt64Tree.GetRoot: IJclInt64TreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderInt64Itr.Create(Self, FRoot, False, isRoot);
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

function TJclInt64Tree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclInt64Tree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64Tree.Last: IJclInt64Iterator;
var
  Start: TJclInt64TreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclInt64TreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderInt64Itr.Create(Self, Start, False, isLast);
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

procedure TJclInt64Tree.Pack;
  procedure PackNode(ANode: TJclInt64TreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclInt64TreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.Remove(const AValue: Int64): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AValue) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Tree.RemoveAll(const ACollection: IJclInt64Collection): Boolean;
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

function TJclInt64Tree.RetainAll(const ACollection: IJclInt64Collection): Boolean;
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

procedure TJclInt64Tree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclInt64Tree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclInt64Tree.Size: Integer;
begin
  Result := FSize;
end;

{$IFNDEF CLR}
//=== { TJclPtrTreeNode } =======================================================

function TJclPtrTreeNode.IndexOfChild(AChild: TJclPtrTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclPtrTreeNode.IndexOfValue(APtr: Pointer;
  const AEqualityComparer: IJclPtrEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclPtrTreeNode(Children[Result]).Value, APtr) then
      Exit;
  Result := -1;
end;

//=== { TJclPtrTree } =======================================================

constructor TJclPtrTree.Create();
begin
  inherited Create();
  FTraverseOrder := toPreOrder;
end;

destructor TJclPtrTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclPtrTree.Add(APtr: Pointer): Boolean;
var
  NewNode: TJclPtrTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(APtr, nil);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(APtr)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclPtrTreeNode.Create;
            NewNode.Value := APtr;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclPtrTreeNode.Create;
        FRoot.Value := APtr;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.AddAll(const ACollection: IJclPtrCollection): Boolean;
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

procedure TJclPtrTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclPtrTreeNode): TJclPtrTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclPtrTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclPtrTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclPtrTree;
  ACollection: IJclPtrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclPtrTree then
  begin
    ADest := TJclPtrTree(Dest);
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

procedure TJclPtrTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrTree then
    TJclPtrTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclPtrTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrTree.ClearNode(var ANode: TJclPtrTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclPtrTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclPtrTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreePointer(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclPtrTree.Contains(APtr: Pointer): Boolean;
  function NodeContains(ANode: TJclPtrTreeNode; APtr: Pointer): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, APtr);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclPtrTreeNode(ANode.Children[Index]), APtr);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, APtr)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.ContainsAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrTree.Create;
  AssignPropertiesTo(Result);
end;

function TJclPtrTree.Equals(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrTree.First: IJclPtrIterator;
var
  Start: TJclPtrTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclPtrTreeNode(Start.Children[0]);
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
function TJclPtrTree.GetEnumerator: IJclPtrIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclPtrTree.GetRoot: IJclPtrTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderPtrItr.Create(Self, FRoot, False, isRoot);
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

function TJclPtrTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclPtrTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrTree.Last: IJclPtrIterator;
var
  Start: TJclPtrTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclPtrTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderPtrItr.Create(Self, Start, False, isLast);
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

procedure TJclPtrTree.Pack;
  procedure PackNode(ANode: TJclPtrTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclPtrTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.Remove(APtr: Pointer): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, APtr) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrTree.RemoveAll(const ACollection: IJclPtrCollection): Boolean;
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

function TJclPtrTree.RetainAll(const ACollection: IJclPtrCollection): Boolean;
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

procedure TJclPtrTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclPtrTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclPtrTree.Size: Integer;
begin
  Result := FSize;
end;
{$ENDIF ~CLR}

//=== { TJclTreeNode } =======================================================

function TJclTreeNode.IndexOfChild(AChild: TJclTreeNode): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclTreeNode.IndexOfValue(AObject: TObject;
  const AEqualityComparer: IJclEqualityComparer): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclTreeNode(Children[Result]).Value, AObject) then
      Exit;
  Result := -1;
end;

//=== { TJclTree } =======================================================

constructor TJclTree.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FTraverseOrder := toPreOrder;
end;

destructor TJclTree.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclTree.Add(AObject: TObject): Boolean;
var
  NewNode: TJclTreeNode;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AObject, nil);

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AObject)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclTreeNode.Create;
            NewNode.Value := AObject;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclTreeNode.Create;
        FRoot.Value := AObject;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.AddAll(const ACollection: IJclCollection): Boolean;
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

procedure TJclTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclTreeNode): TJclTreeNode;
  var
    Index: Integer;
  begin
    Result := TJclTreeNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclTreeNode(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclTree;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclTree then
  begin
    ADest := TJclTree(Dest);
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

procedure TJclTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclTree then
    TJclTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclTree.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree.ClearNode(var ANode: TJclTreeNode);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclTreeNode;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclTreeNode(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeObject(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclTree.Contains(AObject: TObject): Boolean;
  function NodeContains(ANode: TJclTreeNode; AObject: TObject): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AObject);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclTreeNode(ANode.Children[Index]), AObject);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AObject)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.ContainsAll(const ACollection: IJclCollection): Boolean;
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

function TJclTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTree.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclTree.Equals(const ACollection: IJclCollection): Boolean;
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

function TJclTree.First: IJclIterator;
var
  Start: TJclTreeNode;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclTreeNode(Start.Children[0]);
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
function TJclTree.GetEnumerator: IJclIterator;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclTree.GetRoot: IJclTreeIterator;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr.Create(Self, FRoot, False, isRoot);
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

function TJclTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclTree.Last: IJclIterator;
var
  Start: TJclTreeNode;
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
            while Start.ChildrenCount > 0 do
              Start := TJclTreeNode(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderItr.Create(Self, Start, False, isLast);
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

procedure TJclTree.Pack;
  procedure PackNode(ANode: TJclTreeNode);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclTreeNode(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.Remove(AObject: TObject): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AObject) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree.RemoveAll(const ACollection: IJclCollection): Boolean;
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

function TJclTree.RetainAll(const ACollection: IJclCollection): Boolean;
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

procedure TJclTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclTree.Size: Integer;
begin
  Result := FSize;
end;

{$IFDEF SUPPORTS_GENERICS}
//=== { TJclTreeNode<T> } =======================================================

function TJclTreeNode<T>.IndexOfChild(AChild: TJclTreeNode<T>): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if Children[Result] = AChild then
      Exit;
  Result := -1;
end;

function TJclTreeNode<T>.IndexOfValue(const AItem: T;
  const AEqualityComparer: IJclEqualityComparer<T>): Integer;
begin
  for Result := 0 to ChildrenCount - 1 do
    if AEqualityComparer.ItemsEqual(TJclTreeNode<T>(Children[Result]).Value, AItem) then
      Exit;
  Result := -1;
end;

//=== { TJclTree<T> } =======================================================

constructor TJclTree<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FTraverseOrder := toPreOrder;
end;

destructor TJclTree<T>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

function TJclTree<T>.Add(const AItem: T): Boolean;
var
  NewNode: TJclTreeNode<T>;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := AllowDefaultElements or not ItemsEqual(AItem, Default(T));

    if Result then
    begin
      if FRoot <> nil then
      begin
        Result := (not Contains(AItem)) or CheckDuplicate;
        if Result then
        begin
          if FRoot.ChildrenCount = Length(FRoot.Children) then
            SetLength(FRoot.Children, CalcGrowCapacity(Length(FRoot.Children), FRoot.ChildrenCount));
          if FRoot.ChildrenCount < Length(FRoot.Children) then
          begin
            NewNode := TJclTreeNode<T>.Create;
            NewNode.Value := AItem;
            NewNode.Parent := FRoot;
            FRoot.Children[FRoot.ChildrenCount] := NewNode;
            Inc(FRoot.ChildrenCount);
            Inc(FSize);
          end
          else
            Result := False;
        end;
      end
      else
      begin
        FRoot := TJclTreeNode<T>.Create;
        FRoot.Value := AItem;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
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

procedure TJclTree<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclTreeNode<T>): TJclTreeNode<T>;
  var
    Index: Integer;
  begin
    Result := TJclTreeNode<T>.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    SetLength(Result.Children, Node.ChildrenCount);
    Result.ChildrenCount := Node.ChildrenCount;
    for Index := 0 to Node.ChildrenCount - 1 do
      Result.Children[Index] := CloneNode(TJclTreeNode<T>(Node.Children[Index]), Result); // recursive call
  end;
var
  ADest: TJclTree<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclTree<T> then
  begin
    ADest := TJclTree<T>(Dest);
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

procedure TJclTree<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclTree<T> then
    TJclTree<T>(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclTree<T>.Clear;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      ClearNode(FRoot);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclTree<T>.ClearNode(var ANode: TJclTreeNode<T>);
var
  Index, ChildIndex, NewCapacity: Integer;
  Parent: TJclTreeNode<T>;
begin
  for Index := ANode.ChildrenCount - 1 downto 0 do
    {$IFDEF BCB}
    ClearNode(TJclTreeNode<T>(ANode.Children[Index]));
    {$ELSE ~BCB}
    ClearNode(ANode.Children[Index]);
    {$ENDIF ~BCB}
  FreeItem(ANode.Value);
  Parent := ANode.Parent;
  if Parent <> nil then
  begin
    ChildIndex := Parent.IndexOfChild(ANode);
    for Index := ChildIndex + 1 to Parent.ChildrenCount - 1 do
      Parent.Children[Index - 1] := Parent.Children[Index];
    Dec(Parent.ChildrenCount);
    NewCapacity := CalcPackCapacity(Length(Parent.Children), Parent.ChildrenCount);
    if NewCapacity < Length(Parent.Children) then
      SetLength(Parent.Children, NewCapacity);
    FreeAndNil(ANode);
  end
  else
  begin
    FreeAndNil(ANode);
    FRoot := nil;
  end;
  Dec(FSize);
end;

function TJclTree<T>.Contains(const AItem: T): Boolean;
  function NodeContains(ANode: TJclTreeNode<T>; const AItem: T): Boolean;
  var
    Index: Integer;
  begin
    Result := ItemsEqual(ANode.Value, AItem);
    if not Result then
      for Index := 0 to ANode.ChildrenCount - 1 do
    begin
      Result := NodeContains(TJclTreeNode<T>(ANode.Children[Index]), AItem);
      if Result then
        Break;
    end;
  end;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      Result := NodeContains(FRoot, AItem)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
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


function TJclTree<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
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

function TJclTree<T>.First: IJclIterator<T>;
var
  Start: TJclTreeNode<T>;
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
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.ChildrenCount > 0) do
              Start := TJclTreeNode<T>(Start.Children[0]);
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
function TJclTree<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclTree<T>.GetRoot: IJclTreeIterator<T>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr<T>.Create(Self, FRoot, False, isRoot);
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

function TJclTree<T>.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclTree<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclTree<T>.Last: IJclIterator<T>;
var
  Start: TJclTreeNode<T>;
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
            while Start.ChildrenCount > 0 do
              Start := TJclTreeNode<T>(Start.Children[Start.ChildrenCount - 1]);
          Result := TPreOrderItr<T>.Create(Self, Start, False, isLast);
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

procedure TJclTree<T>.Pack;
  procedure PackNode(ANode: TJclTreeNode<T>);
  var
    Index: Integer;
  begin
    SetLength(ANode.Children, ANode.ChildrenCount);
    for Index := 0 to ANode.ChildrenCount - 1 do
      PackNode(TJclTreeNode<T>(ANode.Children[Index]));
  end;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FRoot <> nil then
      PackNode(FRoot);
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.Remove(const AItem: T): Boolean;
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
    Result := FRoot <> nil;
    if Result then
    begin
      It := First;
      while It.HasNext do
        if ItemsEqual(It.Next, AItem) then
      begin
        It.Remove;
        if RemoveSingleElement then
          Break;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclTree<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclTree<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
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

procedure TJclTree<T>.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclTree<T>.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclTree<T>.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclTreeE<T> } =======================================================

constructor TJclTreeE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclTreeE<T> then
    TJclTreeE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclTreeE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeE<T>.Create(EqualityComparer, False);
  AssignPropertiesTo(Result);
end;

function TJclTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclTreeF<T> } =======================================================

constructor TJclTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCompare(ACompare);
end;

function TJclTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclTreeI<T> } =======================================================

function TJclTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclTreeI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
    Result := A.Equals(B);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
