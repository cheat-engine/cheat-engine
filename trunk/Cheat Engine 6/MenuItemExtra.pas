unit MenuItemExtra;

{$MODE Delphi}

{
V1.0: TMenuItemExtra is just a TMenuItem with an extra data pointer, so menu
items can be created on the fly and assigned extra data in the forum of records
or other objects usefull for an onclick handler
}

interface

uses menus;

type TMenuItemExtra=class(TMenuItem)
  public
    data: pointer;
end;

implementation

end.
