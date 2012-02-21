(******************************************************************************
 * ZedTreeView                                                                *
 *                                                                            *
 * Author: Momchil Rangelov                                                   *
 *                                                                            *
 * You can report bugs at ms_rangelov@hotmail.com                             *
 *                                                                            *
 * This source is free software; you can redistribute it and/or modify it     *
 * under the terms of the GNU General Public License as published by the      *
 * Free Software Foundation at <http://www.gnu.org/copyleft/gpl.html>         *
 *                                                                            *
 * This code is distributed in the hope that it will be useful, but           *
 * WITHOUT ANY WARRANTY; without even the implied warranty of                 *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                       *
 *                                                                            *
 * Abstract:                                                                  *
 * This unit contains helper classes for adding functionality                 *
 * to the TZedDBTreeView                                                      *
 ******************************************************************************)

unit ZedTreeView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, DB;
  
type

  { TZedTreeNode }
  TZedTreeNode = class(TTreeNode)

    private
      FID: Integer;
      FParentID: Integer;
      FDeletedFromDB: Boolean;
      FDBBookmark: TBookmark;
      function GetParent: TZedTreeNode;
      procedure SetDBBookmark(const AValue: TBookmark);
      procedure SetID(const AValue: Integer);
      procedure SetParentID(const AValue: Integer);
      
    public
      destructor Destroy; override;

      procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); override;

      property Parent : TZedTreeNode read GetParent;
      property ID : Integer read FID write SetID;
      property ParentID : Integer read FParentID write SetParentID;
      property DeletedFromDB : Boolean read FDeletedFromDB write FDeletedFromDB;
      property DBBookmark : TBookmark read FDBBookmark write SetDBBookmark;

  end;
  
type

  { TZedTreeNodes }

  TZedTreeNodes = class(TTreeNodes)

    private
      function GetNodeFromIndex(Index: Integer): TZedTreeNode;
      
    public
      function AddNewChild(ParentNode: TZedTreeNode; const S: string): TZedTreeNode;
      function IndexOf(anID : Integer) : Integer;
      property Item[Index: Integer]: TZedTreeNode read GetNodeFromIndex; default;
      
  end;
  
type

  { TZedTreeView }

  TZedTreeView = class(TTreeView)

    private
      FSelecting,
      FSelectChanged: Boolean;
      procedure SetZedTreeNodes(const AValue: TZedTreeNodes);
      function GetZedTreeNodes : TZedTreeNodes;
      function GetSelection: TZedTreeNode;
      procedure SetSelection(const AValue: TZedTreeNode);
      
    protected
      procedure Change(Node: TTreeNode); override;
      procedure Change(Node: TZedTreeNode); dynamic;
      procedure Delete(Node: TTreeNode); override;
      procedure Delete(Node: TZedTreeNode); dynamic;
      procedure EditorEditingDone(Sender: TObject); override;
      procedure Edit(aText: String); dynamic;
      procedure Added(Node: TZedTreeNode); dynamic;
      procedure Moved(aDestNode, aSourceNode : TZedTreeNode); dynamic;
      procedure StartMoving(aDestNode, aSourceNode : TZedTreeNode); dynamic;
      procedure SetDestroyingState; dynamic;
      procedure ClearDestroyingState; dynamic;
      procedure SetInternalAdd; dynamic;
      procedure ClearInternalAdd; dynamic;
      
      function CreateNode: TZedTreeNode; override;
      
    public
      property Items: TZedTreeNodes read GetZedTreeNodes write SetZedTreeNodes;

      constructor Create(AnOwner: TComponent); override;
      property Selected: TZedTreeNode read GetSelection write SetSelection;
      
  end;

implementation

uses StdCtrls;

{ TZedTreeNode }

(******************************************************************************
* Sets key to the ID property.
*******************************************************************************)
procedure TZedTreeNode.SetID(const AValue: Integer);
begin
  if FID=AValue then exit;
  FID:=AValue;
end;

(******************************************************************************
* Returns parent of current node as extended ZedTreeNode.
*******************************************************************************)
function TZedTreeNode.GetParent: TZedTreeNode;
begin
  result := TZedTreeNode(inherited Parent);
end;

(******************************************************************************
* Saves value of bookmark.
*******************************************************************************)
procedure TZedTreeNode.SetDBBookmark(const AValue: TBookmark);
begin
  if FDBBookmark=AValue then exit;
  FDBBookmark:=AValue;
end;

(******************************************************************************
* Sets key to the ParentID property.
*******************************************************************************)
procedure TZedTreeNode.SetParentID(const AValue: Integer);
begin
  if FParentID=AValue then exit;
  FParentID:=AValue;
end;

(******************************************************************************
* Destructor.
*******************************************************************************)
destructor TZedTreeNode.Destroy;
begin
  inherited Destroy;
end;

(******************************************************************************
* Overrides original MoveTo in order to provide moving of extended ZedTreeNode.
*******************************************************************************)
procedure TZedTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
begin
  if not Deleting then
  begin
    (TreeView as TZedTreeView).StartMoving(Destination as TZedTreeNode, self);
    inherited MoveTo(Destination, Mode);
    (TreeView as TZedTreeView).Moved(Destination as TZedTreeNode, self);
  end;
end;


{ TZedTreeView }

(******************************************************************************
* Returns selected node.
*******************************************************************************)
function TZedTreeView.GetSelection: TZedTreeNode;
begin
  Result := TZedTreeNode(inherited Selected);
end;

(******************************************************************************
* Sets given node as selected.
*******************************************************************************)
procedure TZedTreeView.SetSelection(const AValue: TZedTreeNode);
begin
  inherited Selected := AValue;
end;

(******************************************************************************
* Overrides Change method in order to mark TZedTreeNode node as changed.
*******************************************************************************)
procedure TZedTreeView.Change(Node: TTreeNode);
begin
  Change(TZedTreeNode(Node));
  inherited Change(Node);
end;

(******************************************************************************
* Sets flag to indicate that TZedTreeNode node is changed.
*******************************************************************************)
procedure TZedTreeView.Change(Node: TZedTreeNode);
begin
  FSelectChanged := True;
end;

(******************************************************************************
* Removes given node form the tree.
*******************************************************************************)
procedure TZedTreeView.Delete(Node: TTreeNode);
begin
  Delete(TZedTreeNode(Node));
  inherited Delete(Node);
end;

procedure TZedTreeView.Delete(Node: TZedTreeNode);
begin

end;

procedure TZedTreeView.EditorEditingDone(Sender: TObject);
var
  aNewText : String;
  hasToProceed : Boolean;
begin
  hasToProceed := false;
  if Assigned(FEditor) then
    begin
      aNewText := FEditor.Text;
      hasToProceed := true;
    end;
  inherited EditorEditingDone(Sender);
  if hasToProceed then Edit(aNewText);
end;

procedure TZedTreeView.Edit(aText: String);
begin

end;

procedure TZedTreeView.Added(Node: TZedTreeNode);
begin

end;

procedure TZedTreeView.Moved(aDestNode, aSourceNode: TZedTreeNode);
begin

end;

procedure TZedTreeView.StartMoving(aDestNode, aSourceNode: TZedTreeNode);
begin

end;

procedure TZedTreeView.SetDestroyingState;
begin

end;

procedure TZedTreeView.ClearDestroyingState;
begin

end;

procedure TZedTreeView.SetInternalAdd;
begin

end;

procedure TZedTreeView.ClearInternalAdd;
begin

end;

(******************************************************************************
* Creates new node and returns its instance.
*******************************************************************************)
function TZedTreeView.CreateNode: TZedTreeNode;
begin
  Result := nil;
  if Assigned(OnCustomCreateItem) then
    OnCustomCreateItem(Self, Result);
  if Result = nil then
    Result := TZedTreeNode.Create(Items);

  Result.DeletedFromDB:= false;
end;

(******************************************************************************
* Replaces items collection with TZedTreeNodes items.
*******************************************************************************)
procedure TZedTreeView.SetZedTreeNodes(const AValue: TZedTreeNodes);
begin
  inherited Items := AValue;
end;

(******************************************************************************
* Returns items collection as TZedTreeNodes.
*******************************************************************************)
function TZedTreeView.GetZedTreeNodes: TZedTreeNodes;
begin
  Result := TZedTreeNodes(inherited Items);
end;

(******************************************************************************
* Constructor: Creates the tree and its items of TZedTreeNodes.
*******************************************************************************)
constructor TZedTreeView.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  inherited Items := TZedTreeNodes.Create(self);
end;

{ TZedTreeNodes }

(******************************************************************************
* Returns node by given index.
*******************************************************************************)
function TZedTreeNodes.GetNodeFromIndex(Index: Integer): TZedTreeNode;
begin
  Result := TZedTreeNode(inherited Item[Index]);
end;

(******************************************************************************
* Creates and adds new child node to the given parent node.
* Returns instance of newly added node.
*******************************************************************************)
function TZedTreeNodes.AddNewChild(ParentNode: TZedTreeNode; const S: string): TZedTreeNode;
begin
  Result := TZedTreeNode(inherited AddChild(ParentNode, S));
  TZedTreeView(Result.TreeView).Added(Result);
end;

(******************************************************************************
* Returns index of node by given ID key.
*******************************************************************************)
function TZedTreeNodes.IndexOf(anID: Integer): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Item[i].ID = anID then
      begin
        Result := i;
        Break;
      end;
end;

end.

