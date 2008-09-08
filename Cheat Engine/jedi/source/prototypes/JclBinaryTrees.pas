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
{$I containers\JclContainerCommon.imp}
{$I containers\JclBinaryTrees.imp}
{$I containers\JclBinaryTrees.int}
type
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclIntfBinaryNode,TJclIntfBinaryTree,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfTree,IJclIntfIterator,IJclIntfTreeIterator, IJclIntfEqualityComparer\, IJclIntfComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TIntfCompare,,const ,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclAnsiStrBinaryNode,TJclAnsiStrBinaryTree,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrTree,IJclAnsiStrIterator,IJclAnsiStrTreeIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\, IJclAnsiStrComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TAnsiStrCompare, override;,const ,AString,AnsiString)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclWideStrBinaryNode,TJclWideStrBinaryTree,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrTree,IJclWideStrIterator,IJclWideStrTreeIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\, IJclWideStrComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TWideStrCompare, override;,const ,AString,WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryTree = TJclAnsiStrBinaryTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryTree = TJclWideStrBinaryTree;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclSingleBinaryNode,TJclSingleBinaryTree,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleTree,IJclSingleIterator,IJclSingleTreeIterator, IJclSingleContainer\, IJclSingleEqualityComparer\, IJclSingleComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TSingleCompare,,const ,AValue,Single)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclDoubleBinaryNode,TJclDoubleBinaryTree,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleTree,IJclDoubleIterator,IJclDoubleTreeIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\, IJclDoubleComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TDoubleCompare,,const ,AValue,Double)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclExtendedBinaryNode,TJclExtendedBinaryTree,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedTree,IJclExtendedIterator,IJclExtendedTreeIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\, IJclExtendedComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TExtendedCompare,,const ,AValue,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBinaryTree = TJclExtendedBinaryTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBinaryTree = TJclDoubleBinaryTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBinaryTree = TJclSingleBinaryTree;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclIntegerBinaryNode,TJclIntegerBinaryTree,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerTree,IJclIntegerIterator,IJclIntegerTreeIterator, IJclIntegerEqualityComparer\, IJclIntegerComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TIntegerCompare,,,AValue,Integer)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclCardinalBinaryNode,TJclCardinalBinaryTree,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalTree,IJclCardinalIterator,IJclCardinalTreeIterator, IJclCardinalEqualityComparer\, IJclCardinalComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TCardinalCompare,,,AValue,Cardinal)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclInt64BinaryNode,TJclInt64BinaryTree,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64Tree,IJclInt64Iterator,IJclInt64TreeIterator, IJclInt64EqualityComparer\, IJclInt64Comparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TInt64Compare,,const ,AValue,Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclPtrBinaryNode,TJclPtrBinaryTree,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrTree,IJclPtrIterator,IJclPtrTreeIterator, IJclPtrEqualityComparer\, IJclPtrComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TPtrCompare,,,APtr,Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclBinaryNode,TJclBinaryTree,TJclAbstractContainer,IJclCollection,IJclTree,IJclIterator,IJclTreeIterator, IJclObjectOwner\, IJclEqualityComparer\, IJclComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,ACompare: TCompare; AOwnsObjects: Boolean,,,AObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclBinaryNode<T>,TJclBinaryTree<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\, IJclComparer<T>\,,,,,AOwnsItems: Boolean,,const ,AItem,T)*)

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
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/prototypes/JclBinaryTrees.pas $';
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
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TIntfItr,TPreOrderIntfItr,TInOrderIntfItr,TPostOrderIntfItr,IJclIntfIterator,IJclIntfTreeIterator,IJclIntfBinaryTreeIterator,IJclIntfCollection,IJclIntfEqualityComparer,TJclIntfBinaryNode,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TIntfItr,TPreOrderIntfItr,TInOrderIntfItr,TPostOrderIntfItr,IJclIntfIterator,IJclIntfCollection,IJclIntfEqualityComparer,TJclIntfBinaryNode,const ,AInterface,IInterface,nil,GetObject,SetObject,FreeObject)}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TAnsiStrItr,TPreOrderAnsiStrItr,TInOrderAnsiStrItr,TPostOrderAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,IJclAnsiStrBinaryTreeIterator,IJclAnsiStrCollection,IJclAnsiStrEqualityComparer,TJclAnsiStrBinaryNode,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TAnsiStrItr,TPreOrderAnsiStrItr,TInOrderAnsiStrItr,TPostOrderAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrCollection,IJclAnsiStrEqualityComparer,TJclAnsiStrBinaryNode,const ,AString,AnsiString,'',GetString,SetString,FreeString)}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TWideStrItr,TPreOrderWideStrItr,TInOrderWideStrItr,TPostOrderWideStrItr,IJclWideStrIterator,IJclWideStrTreeIterator,IJclWideStrBinaryTreeIterator,IJclWideStrCollection,IJclWideStrEqualityComparer,TJclWideStrBinaryNode,const ,AString,WideString,GetString,SetString)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TWideStrItr,TPreOrderWideStrItr,TInOrderWideStrItr,TPostOrderWideStrItr,IJclWideStrIterator,IJclWideStrCollection,IJclWideStrEqualityComparer,TJclWideStrBinaryNode,const ,AString,WideString,'',GetString,SetString,FreeString)}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TSingleItr,TPreOrderSingleItr,TInOrderSingleItr,TPostOrderSingleItr,IJclSingleIterator,IJclSingleTreeIterator,IJclSingleBinaryTreeIterator,IJclSingleCollection,IJclSingleEqualityComparer,TJclSingleBinaryNode,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TSingleItr,TPreOrderSingleItr,TInOrderSingleItr,TPostOrderSingleItr,IJclSingleIterator,IJclSingleCollection,IJclSingleEqualityComparer,TJclSingleBinaryNode,const ,AValue,Single,0.0,GetValue,SetValue,FreeValue)}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TDoubleItr,TPreOrderDoubleItr,TInOrderDoubleItr,TPostOrderDoubleItr,IJclDoubleIterator,IJclDoubleTreeIterator,IJclDoubleBinaryTreeIterator,IJclDoubleCollection,IJclDoubleEqualityComparer,TJclDoubleBinaryNode,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TDoubleItr,TPreOrderDoubleItr,TInOrderDoubleItr,TPostOrderDoubleItr,IJclDoubleIterator,IJclDoubleCollection,IJclDoubleEqualityComparer,TJclDoubleBinaryNode,const ,AValue,Double,0.0,GetValue,SetValue,FreeValue)}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TExtendedItr,TPreOrderExtendedItr,TInOrderExtendedItr,TPostOrderExtendedItr,IJclExtendedIterator,IJclExtendedTreeIterator,IJclExtendedBinaryTreeIterator,IJclExtendedCollection,IJclExtendedEqualityComparer,TJclExtendedBinaryNode,const ,AValue,Extended,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TExtendedItr,TPreOrderExtendedItr,TInOrderExtendedItr,TPostOrderExtendedItr,IJclExtendedIterator,IJclExtendedCollection,IJclExtendedEqualityComparer,TJclExtendedBinaryNode,const ,AValue,Extended,0.0,GetValue,SetValue,FreeValue)}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TIntegerItr,TPreOrderIntegerItr,TInOrderIntegerItr,TPostOrderIntegerItr,IJclIntegerIterator,IJclIntegerTreeIterator,IJclIntegerBinaryTreeIterator,IJclIntegerCollection,IJclIntegerEqualityComparer,TJclIntegerBinaryNode,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TIntegerItr,TPreOrderIntegerItr,TInOrderIntegerItr,TPostOrderIntegerItr,IJclIntegerIterator,IJclIntegerCollection,IJclIntegerEqualityComparer,TJclIntegerBinaryNode,,AValue,Integer,0,GetValue,SetValue,FreeValue)}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TCardinalItr,TPreOrderCardinalItr,TInOrderCardinalItr,TPostOrderCardinalItr,IJclCardinalIterator,IJclCardinalTreeIterator,IJclCardinalBinaryTreeIterator,IJclCardinalCollection,IJclCardinalEqualityComparer,TJclCardinalBinaryNode,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TCardinalItr,TPreOrderCardinalItr,TInOrderCardinalItr,TPostOrderCardinalItr,IJclCardinalIterator,IJclCardinalCollection,IJclCardinalEqualityComparer,TJclCardinalBinaryNode,,AValue,Cardinal,0,GetValue,SetValue,FreeValue)}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TInt64Itr,TPreOrderInt64Itr,TInOrderInt64Itr,TPostOrderInt64Itr,IJclInt64Iterator,IJclInt64TreeIterator,IJclInt64BinaryTreeIterator,IJclInt64Collection,IJclInt64EqualityComparer,TJclInt64BinaryNode,const ,AValue,Int64,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TInt64Itr,TPreOrderInt64Itr,TInOrderInt64Itr,TPostOrderInt64Itr,IJclInt64Iterator,IJclInt64Collection,IJclInt64EqualityComparer,TJclInt64BinaryNode,const ,AValue,Int64,0,GetValue,SetValue,FreeValue)}

{$IFNDEF CLR}
type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TPtrItr,TPreOrderPtrItr,TInOrderPtrItr,TPostOrderPtrItr,IJclPtrIterator,IJclPtrTreeIterator,IJclPtrBinaryTreeIterator,IJclPtrCollection,IJclPtrEqualityComparer,TJclPtrBinaryNode,,APtr,Pointer,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TPtrItr,TPreOrderPtrItr,TInOrderPtrItr,TPostOrderPtrItr,IJclPtrIterator,IJclPtrCollection,IJclPtrEqualityComparer,TJclPtrBinaryNode,,APtr,Pointer,nil,GetPointer,SetPointer,FreePointer)}
{$ENDIF ~CLR}

type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TItr,TPreOrderItr,TInOrderItr,TPostOrderItr,IJclIterator,IJclTreeIterator,IJclBinaryTreeIterator,IJclCollection,IJclEqualityComparer,TJclBinaryNode,,AObject,TObject,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TItr,TPreOrderItr,TInOrderItr,TPostOrderItr,IJclIterator,IJclCollection,IJclEqualityComparer,TJclBinaryNode,,AObject,TObject,nil,GetObject,SetObject,FreeObject)}

{$IFDEF SUPPORTS_GENERICS}
type
{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TItr<T>,TPreOrderItr<T>,TInOrderItr<T>,TPostOrderItr<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclBinaryTreeIterator<T>,IJclCollection<T>,IJclEqualityComparer<T>,TJclBinaryNode<T>,const ,AItem,T,GetItem,SetItem)}

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TItr<T>,TPreOrderItr<T>,TInOrderItr<T>,TPostOrderItr<T>,IJclIterator<T>,IJclCollection<T>,IJclEqualityComparer<T>,TJclBinaryNode<T>,const ,AItem,T,Default(T),GetItem,SetItem,FreeItem)}
{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclIntfBinaryTree,TJclIntfBinaryNode,TPreOrderIntfItr,TInOrderIntfItr,TPostOrderIntfItr,IJclIntfCollection,IJclIntfIterator,IJclIntfTreeIterator,ACompare: TIntfCompare,
  SetCompare(ACompare);,,const ,AInterface,IInterface,nil,FreeObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclAnsiStrBinaryTree,TJclAnsiStrBinaryNode,TPreOrderAnsiStrItr,TInOrderAnsiStrItr,TPostOrderAnsiStrItr,IJclAnsiStrCollection,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,ACompare: TAnsiStrCompare,
  SetCompare(ACompare);,,const ,AString,AnsiString,'',FreeString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclWideStrBinaryTree,TJclWideStrBinaryNode,TPreOrderWideStrItr,TInOrderWideStrItr,TPostOrderWideStrItr,IJclWideStrCollection,IJclWideStrIterator,IJclWideStrTreeIterator,ACompare: TWideStrCompare,
  SetCompare(ACompare);,,const ,AString,WideString,'',FreeString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclSingleBinaryTree,TJclSingleBinaryNode,TPreOrderSingleItr,TInOrderSingleItr,TPostOrderSingleItr,IJclSingleCollection,IJclSingleIterator,IJclSingleTreeIterator,ACompare: TSingleCompare,
  SetCompare(ACompare);,,const ,AValue,Single,0.0,FreeSingle)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclDoubleBinaryTree,TJclDoubleBinaryNode,TPreOrderDoubleItr,TInOrderDoubleItr,TPostOrderDoubleItr,IJclDoubleCollection,IJclDoubleIterator,IJclDoubleTreeIterator,ACompare: TDoubleCompare,
  SetCompare(ACompare);,,const ,AValue,Double,0.0,FreeDouble)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclExtendedBinaryTree,TJclExtendedBinaryNode,TPreOrderExtendedItr,TInOrderExtendedItr,TPostOrderExtendedItr,IJclExtendedCollection,IJclExtendedIterator,IJclExtendedTreeIterator,ACompare: TExtendedCompare,
  SetCompare(ACompare);,,const ,AValue,Extended,0.0,FreeExtended)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclIntegerBinaryTree,TJclIntegerBinaryNode,TPreOrderIntegerItr,TInOrderIntegerItr,TPostOrderIntegerItr,IJclIntegerCollection,IJclIntegerIterator,IJclIntegerTreeIterator,ACompare: TIntegerCompare,
  SetCompare(ACompare);,,,AValue,Integer,0,FreeInteger)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclCardinalBinaryTree,TJclCardinalBinaryNode,TPreOrderCardinalItr,TInOrderCardinalItr,TPostOrderCardinalItr,IJclCardinalCollection,IJclCardinalIterator,IJclCardinalTreeIterator,ACompare: TCardinalCompare,
  SetCompare(ACompare);,,,AValue,Cardinal,0,FreeCardinal)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64BinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64BinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclInt64BinaryTree,TJclInt64BinaryNode,TPreOrderInt64Itr,TInOrderInt64Itr,TPostOrderInt64Itr,IJclInt64Collection,IJclInt64Iterator,IJclInt64TreeIterator,ACompare: TInt64Compare,
  SetCompare(ACompare);,,const ,AValue,Int64,0,FreeInt64)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclPtrBinaryTree,TJclPtrBinaryNode,TPreOrderPtrItr,TInOrderPtrItr,TPostOrderPtrItr,IJclPtrCollection,IJclPtrIterator,IJclPtrTreeIterator,ACompare: TPtrCompare,
  SetCompare(ACompare);,,,APtr,Pointer,nil,FreePointer)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTree.Create(Compare, False);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclBinaryTree,TJclBinaryNode,TPreOrderItr,TInOrderItr,TPostOrderItr,IJclCollection,IJclIterator,IJclTreeIterator,ACompare: TCompare; AOwnsObjects: Boolean,
  SetCompare(ACompare);,AOwnsObjects,,AObject,TObject,nil,FreeObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclBinaryTree<T>,TJclBinaryNode<T>,TPreOrderItr<T>,TInOrderItr<T>,TPostOrderItr<T>,IJclCollection<T>,IJclIterator<T>,IJclTreeIterator<T>,AOwnsItems: Boolean,,AOwnsItems,const ,AItem,T,Default(T),FreeItem)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

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

